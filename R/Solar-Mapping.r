library(shiny)
library(raster)
library(dbscan)

# Caminho fixo para o diretório de imagens
dirPath <- "/media/paulojaka/HD_150g/TCC/imagens particionadas-20250209T002945Z-001/imagens particionadas/SC_24_Z_D_I_2_NE_A_II_1_D_rescaled"
# Lista todos os arquivos .tif dentro do diretório fixo
imgFiles <- list.files(dirPath, pattern = "\\.tif$", full.names = TRUE)

ui <- fluidPage(
  titlePanel("Visualizador e Clusterização de Imagens TIF"),
  
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("selected_dir"),
      actionButton("prevImage", "Imagem Anterior", class = "btn-warning"),
      actionButton("nextImage", "Próxima Imagem", class = "btn-success"),
      actionButton("scanButton", "Executar DBSCAN", class = "btn-primary"),
      actionButton("plotNextClusterButton", "Próximo Cluster", class = "btn-info"),
      actionButton("plotPriorClusterButton", "Cluster Anterior", class = "btn-secondary")
    ),
    mainPanel(
      plotOutput("raster", width = "1000px", height = "700px")
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
  
  # Carrega e exibe a primeira imagem se houver imagens na pasta
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
    
    # Executa o DBSCAN
    dbScanResult <- dbscan(val, eps = 2, minPts = 50)
    
    rValues$clusters <- dbScanResult$cluster
    rValues$coords <- coords
    rValues$plotCluster <- 0
    
    output$raster <- renderPlot({
      I <- rValues$clusters == 0
      plotRGB(rValues$img)
      points(rValues$coords[I, ], col = "purple", pch = 20, cex = 0.05)
    }, width = 1000, height = 700)
  })
  
  observeEvent(input$plotNextClusterButton, {
    req(rValues$clusters)
    rValues$plotCluster <- rValues$plotCluster + 1
    
    output$raster <- renderPlot({
      I <- rValues$clusters == rValues$plotCluster
      plotRGB(rValues$img)
      points(rValues$coords[I, ], col = "purple", pch = 20, cex = 0.05)
    }, width = 1000, height = 700)
  })
  
  observeEvent(input$plotPriorClusterButton, {
    req(rValues$clusters)
    rValues$plotCluster <- max(0, rValues$plotCluster - 1)
    
    output$raster <- renderPlot({
      I <- rValues$clusters == rValues$plotCluster
      plotRGB(rValues$img)
      points(rValues$coords[I, ], col = "purple", pch = 20, cex = 0.05)
    }, width = 1000, height = 700)
  })
}

shinyApp(ui, server)
