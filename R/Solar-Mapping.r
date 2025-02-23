library(shiny)
library(raster)
library(dbscan)
library(gridExtra)
library(ggplot2)

# Caminho fixo para o diretório de imagens
dirPath <- "/media/paulojaka/HD_500g/SC_24_Z_D_I_2_NE_A_II_1_D_rescaled-20250223T001141Z-001/SC_24_Z_D_I_2_NE_A_II_1_D_rescaled"
# Lista todos os arquivos .tif dentro do diretório fixo
imgFiles <- list.files(dirPath, pattern = "\\.tif$", full.names = TRUE)

ui <- fluidPage(
  titlePanel("Visualizador e Clusterização de Imagens TIF"),
  
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("selected_dir"),
      numericInput("eps", "Valor de eps:", value = 2, min = 0.1, step = 0.1),
      numericInput("minPts", "Valor de minPts:", value = 50, min = 1, step = 1),
      actionButton("prevImage", "Imagem Anterior", class = "btn-warning"),
      actionButton("nextImage", "Próxima Imagem", class = "btn-success"),
      actionButton("scanButton", "Executar DBSCAN", class = "btn-primary"),
      selectInput("selectedCluster", "Escolha o Cluster:", choices = NULL),
      actionButton("plotSelectedCluster", "Exibir Cluster Selecionado", class = "btn-info")
    ),
    mainPanel(
      plotOutput("raster", width = "1000px", height = "700px"),
      plotOutput("clusterCollage", width = "700px", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  rValues <- reactiveValues(
    imgFiles = imgFiles,
    currentIndex = 1,
    img = NULL,
    clusters = NULL,
    coords = NULL,
    plotCluster = 0
  )
  
  output$selected_dir <- renderText({
    paste0("Diretório Selecionado: ", dirPath)
  })
  
  if (length(imgFiles) > 0) {
    rValues$img <- brick(imgFiles[1])
  }
  
  output$raster <- renderPlot({
    req(rValues$img)
    plotRGB(rValues$img)
  }, width = 1000, height = 700)
  
  observeEvent(input$nextImage, {
    req(rValues$imgFiles)
    if (rValues$currentIndex < length(rValues$imgFiles)) {
      rValues$currentIndex <- rValues$currentIndex + 1
      rValues$img <- brick(rValues$imgFiles[rValues$currentIndex])
    }
  })
  
  observeEvent(input$prevImage, {
    req(rValues$imgFiles)
    if (rValues$currentIndex > 1) {
      rValues$currentIndex <- rValues$currentIndex - 1
      rValues$img <- brick(rValues$imgFiles[rValues$currentIndex])
    }
  })
  
  observeEvent(input$scanButton, {
    req(rValues$img)
    
    img <- rValues$img
    val <- getValues(img)
    coords <- coordinates(img)
    val <- cbind(val, coords)
    
    eps_val <- input$eps
    minPts_val <- input$minPts
    
    dbScanResult <- dbscan(val, eps = eps_val, minPts = minPts_val)
    
    rValues$clusters <- dbScanResult$cluster
    rValues$coords <- coords
    rValues$plotCluster <- 0
    
    clusterNumbers <- sort(unique(rValues$clusters))
    clusterNumbers <- clusterNumbers[clusterNumbers != 0] # Remover cluster 0 (outliers)
    
    updateSelectInput(session, "selectedCluster", choices = clusterNumbers)
    
    output$clusterCollage <- renderPlot({
      clusterPlots <- lapply(clusterNumbers[1:min(27, length(clusterNumbers))], function(clust) {
        I <- rValues$clusters == clust
        ggplot(data = as.data.frame(rValues$coords[I, ]), aes(x = x, y = y)) +
          geom_point(color = "purple", size = 0.5) +
          ggtitle(paste("Cluster", clust)) +
          theme_minimal()
      })
      
      do.call(grid.arrange, c(clusterPlots, ncol = 3))
    })
  })
  
  observeEvent(input$plotSelectedCluster, {
    req(rValues$clusters)
    
    rValues$plotCluster <- as.numeric(input$selectedCluster)
    
    output$raster <- renderPlot({
      I <- rValues$clusters == rValues$plotCluster
      plotRGB(rValues$img)
      points(rValues$coords[I, ], col = "purple", pch = 20, cex = 0.05)
    }, width = 1000, height = 700)
  })
}

shinyApp(ui, server)
