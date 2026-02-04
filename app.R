library(shiny)
library(igraph)
library(igraphdata)
library(visNetwork)

# --- 1. Daten laden und vorbereiten ---
data("USairports", package = "igraphdata")
g_full <- USairports

# --- UI Definition ---
ui <- fluidPage(
  title = "USAirports Netzwerk-Analyse",
  
  tags$head(
    tags$style(HTML("
      html, body { 
        height: 100%; 
        overflow: hidden; 
        margin: 0; 
        padding: 0;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .header-panel {
        background-color: #333;
        color: white;
        padding: 10px 15px;
        height: 50px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .sidebar {
        background-color: #f8f9fa;
        height: calc(100vh - 50px);
        overflow-y: auto;
        padding: 15px;
        border-right: 1px solid #ddd;
      }
      .main-content {
        height: calc(100vh - 50px);
        padding: 0 !important;
        position: relative;
      }
      .info-box {
        background-color: rgba(255, 255, 255, 0.95);
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        padding: 10px;
        z-index: 100;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        $('#toggleSidebar').click(function() {
          $('.sidebar-column').toggle();
          $('.main-column').toggleClass('col-sm-8 col-sm-9 col-sm-12');
          window.dispatchEvent(new Event('resize'));
        });
      });
    "))
  ),
  
  # --- Header ---
  div(class = "header-panel",
      span(style = "font-size: 18px; font-weight: bold;", "✈️ USAirports Explorer"),
      actionButton("toggleSidebar", "☰ Filter", class = "btn-sm btn-light")
  ),
  
  # --- Layout ---
  fluidRow(
    # Sidebar
    column(3, class = "sidebar sidebar-column",
           h4("Einstellungen"),
           hr(),
           sliderInput("topN", "Anzahl Airports (Top-N nach Degree):",
                       min = 50, max = 755, value = 755, step = 25),
           
           checkboxInput("useGC", "Nur Giant Component", value = TRUE),
           checkboxInput("colorComm", "Communities einfärben", value = TRUE),
           checkboxInput("showEdges", "Kanten anzeigen", value = TRUE),
           
           hr(),
           div(class = "alert alert-info", role = "alert",
               tags$small("Performance-Tipp: Das Netzwerk reagiert jetzt nur noch auf Klick, um das Zoomen flüssig zu halten.")
           )
    ),
    
    # Main Area
    column(9, class = "main-content main-column",
           visNetworkOutput("net", width = "100%", height = "100%"),
           
           absolutePanel(
             id = "details-panel", class = "info-box",
             top = 20, right = 20, width = 250, fixed = FALSE,
             draggable = TRUE,
             h5(icon("info-circle"), "Details"),
             verbatimTextOutput("node_info", placeholder = TRUE)
           )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  g_reactive <- reactive({
    g <- g_full
    if (isTRUE(input$useGC)) {
      comp <- components(g, mode = "weak")
      giant_id <- which.max(comp$csize)
      g <- induced_subgraph(g, vids = which(comp$membership == giant_id))
    }
    if (input$topN < vcount(g)) {
      deg <- degree(g, mode = "all")
      top_nodes <- names(sort(deg, decreasing = TRUE))[1:input$topN]
      g <- induced_subgraph(g, vids = top_nodes)
    }
    g
  })
  
  output$net <- renderVisNetwork({
    g <- g_reactive()
    ids <- V(g)$name
    
    city_map <- if ("City" %in% vertex_attr_names(g)) {
      setNames(as.character(V(g)$City), ids)
    } else {
      setNames(rep("n/a", length(ids)), ids)
    }
    
    deg_all <- degree(g, mode = "all")
    
    nodes <- data.frame(
      id = ids,
      label = ids, 
      value = deg_all[ids],
      # Wir entfernen 'title' hier fast komplett oder lassen es leer, 
      # damit beim Hovern keine störende Box auftaucht.
      # Die Infos kommen ja jetzt rechts oben im Panel.
      group = "Airport",
      stringsAsFactors = FALSE
    )
    
    if (isTRUE(input$colorComm)) {
      g_und <- as.undirected(g, mode = "collapse")
      comm <- cluster_louvain(g_und)
      nodes$group <- as.character(membership(comm)[nodes$id])
    }
    
    if (isTRUE(input$showEdges)) {
      edges <- as.data.frame(get.edgelist(g))
      colnames(edges) <- c("from", "to")
      edges$color <- "rgba(100,100,100,0.2)"
    } else {
      edges <- data.frame(from = character(0), to = character(0))
    }
    
    visNetwork(nodes, edges) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(
        # HIER IST DIE ÄNDERUNG: hover = FALSE
        # Das bedeutet: Das Subnetz wird erst berechnet, wenn du klickst.
        highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE),
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(
        navigationButtons = TRUE,
        # Auch hier: Hover-Events deaktivieren für maximale Performance
        hover = FALSE 
      ) %>%
      visEvents(selectNode = "function(nodes) {
        Shiny.onInputChange('net_selected', nodes.nodes);
      ;}")
  })
  
  output$node_info <- renderText({
    sel <- input$net_selected
    if (is.null(sel) || length(sel) == 0) {
      return("Bitte Knoten anklicken.")
    }
    
    node_id <- sel[1]
    g <- g_reactive()
    
    if (!node_id %in% V(g)$name) return("Knoten nicht im Filter.")
    
    city <- if ("City" %in% vertex_attr_names(g)) V(g)[node_id]$City else "n/a"
    d_in <- degree(g, mode = "in")[node_id]
    d_out <- degree(g, mode = "out")[node_id]
    betw <- round(betweenness(g)[node_id], 1)
    
    paste0(
      "Airport: ", node_id, "\n",
      "City:    ", city, "\n",
      "---------------\n",
      "In-Degree:   ", d_in, "\n",
      "Out-Degree:  ", d_out, "\n",
      "Betweenness: ", betw
    )
  })
}

shinyApp(ui, server)