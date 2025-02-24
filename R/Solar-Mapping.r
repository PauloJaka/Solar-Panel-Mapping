library(shiny)
library(raster)
library(dbscan)
library(gridExtra)
library(ggplot2)
library(DBI)
library(RPostgres)
library(jsonlite)
library(shinydashboard)
library(shinyBS)

# Módulo para carregar arquivos
loadImageFiles <- function(dirPath) {
  list.files(dirPath, pattern = "\\.tif$", full.names = TRUE)
}

# Módulo para conexão com banco de dados
getDBConnection <- function(host, dbname, user, password, port = 5432) {
  dbConnect(
    RPostgres::Postgres(),
    host = host,
    dbname = dbname,
    user = user,
    password = password,
    port = port
  )
}

# Módulo para processamento DBSCAN
runDBSCAN <- function(img, eps_val, minPts_val) {
  val <- getValues(img)
  coords <- coordinates(img)
  val <- cbind(val, coords)
  
  # Executar DBSCAN
  dbScanResult <- dbscan(val, eps = eps_val, minPts = minPts_val)
  
  # Resultados
  return(list(
    clusters = dbScanResult$cluster,
    coords = coords
  ))
}

# Módulo para salvar clusters no banco
saveClustersToDB <- function(imgName, clusterNumbers, clusters, coords, eps, minPts, plotCluster, 
                             dbHost, dbName, dbUser, dbPassword) {
  # Conectar ao banco de dados
  conn <- getDBConnection(dbHost, dbName, dbUser, dbPassword)
  insertedCount <- 0
  
  # Processar cada cluster
  for(clusterId in clusterNumbers) {
    # Determinar quais pontos pertencem a este cluster
    I <- clusters == clusterId
    clusterCoords <- coords[I, ]
    
    # Converter coordenadas para JSON
    coordsJSON <- toJSON(clusterCoords)
    
    # Determinar se é o cluster selecionado
    isSolar <- ifelse(clusterId == plotCluster, 1, 0)
    
    # Preparar query SQL de inserção
    query <- "INSERT INTO clusters_detectados_solar_rec 
              (image_name, id_cluster, is_solar, eps, minPts, coords) 
              VALUES ($1, $2, $3, $4, $5, $6)"
    
    # Executar a inserção
    dbExecute(
      conn, 
      query, 
      params = list(
        imgName,
        clusterId,
        isSolar,
        eps,
        minPts,
        coordsJSON
      )
    )
    
    insertedCount <- insertedCount + 1
  }
  
  # Desconectar do banco
  dbDisconnect(conn)
  
  return(insertedCount)
}

# Caminho fixo para o diretório de imagens
dirPath <- "/media/paulojaka/HD_500g/SC_24_Z_D_I_2_NE_A_II_1_D_rescaled-20250223T001141Z-001/SC_24_Z_D_I_2_NE_A_II_1_D_rescaled"

# Configurações do banco de dados
db_config <- list(
  host = ".env",
  name = ".env",
  user = ".env",
  password = ".env."
)

# UI customizada e mais compacta
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Detector de Painéis Solares", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    tags$head(
      tags$style(HTML("
        .skin-black .main-header .logo { background-color: #222d32; }
        .skin-black .main-header .navbar { background-color: #222d32; }
        .content-wrapper { background-color: #f4f4f4; }
        .box { margin-bottom: 10px; }
        .small-box { margin-bottom: 10px; }
        .btn-white { 
          background-color: white; 
          color: #333;
          border-color: #ddd;
        }
        .cluster-gallery {
          display: grid;
          grid-template-columns: repeat(6, 1fr);
          grid-gap: 5px;
        }
      "))
    ),
    tags$div(
      style = "padding: 10px;",
      
      # Navegação de Imagens
      box(
        width = NULL,
        status = "primary",
        title = "Navegação de Imagens",
        solidHeader = TRUE,
        collapsible = TRUE,
        div(style = "display: flex; justify-content: space-between;",
            actionButton("prevImage", "Anterior", icon = icon("arrow-left"), 
                         class = "btn-white", style = "width: 48%;"),
            actionButton("nextImage", "Próximo", icon = icon("arrow-right"), 
                         class = "btn-white", style = "width: 48%;")
        ),
        tags$br(),
        verbatimTextOutput("image_info", placeholder = TRUE)
      ),
      
      # DBSCAN
      box(
        width = NULL,
        status = "info",
        title = "Configuração DBSCAN",
        solidHeader = TRUE,
        collapsible = TRUE,
        sliderInput("eps", "Valor de eps:", min = 0.1, max = 10, value = 2, step = 0.1),
        sliderInput("minPts", "Valor de minPts:", min = 1, max = 200, value = 50, step = 1),
        actionButton("scanButton", "Executar DBSCAN", icon = icon("play"), 
                     class = "btn-white btn-block")
      ),
      
      # Navegação de Clusters
      box(
        width = NULL,
        status = "success",
        title = "Navegação de Clusters",
        solidHeader = TRUE,
        collapsible = TRUE,
        selectInput("selectedCluster", "Cluster:", choices = NULL),
        div(style = "display: flex; justify-content: space-between;",
            actionButton("prevCluster", "Anterior", icon = icon("chevron-left"), 
                         class = "btn-white", style = "width: 48%;"),
            actionButton("nextCluster", "Próximo", icon = icon("chevron-right"), 
                         class = "btn-white", style = "width: 48%;")
        ),
        tags$br(),
        actionButton("plotSelectedCluster", "Visualizar Cluster", icon = icon("eye"), 
                     class = "btn-white btn-block")
      ),
      
      # Banco de Dados
      box(
        width = NULL,
        status = "warning",
        title = "Banco de Dados",
        solidHeader = TRUE,
        collapsible = TRUE,
        actionButton("saveAllClusters", "Salvar no DB", icon = icon("database"), 
                     class = "btn-white btn-block"),
        verbatimTextOutput("dbStatus", placeholder = TRUE)
      )
    )
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Imagem Atual", status = "primary", solidHeader = TRUE,
          width = NULL, height = "400px",
          plotOutput("raster", height = "350px")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Visualização de Clusters", status = "success", solidHeader = TRUE,
          width = NULL, 
          plotOutput("clusterCollage", height = "450px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Carregar imagens
  imgFiles <- loadImageFiles(dirPath)
  
  rValues <- reactiveValues(
    imgFiles = imgFiles,
    currentIndex = 1,
    img = NULL,
    clusters = NULL,
    coords = NULL,
    plotCluster = 0,
    dbStatus = "",
    clusterNumbers = NULL,
    currentClusterIndex = 1
  )
  
  # Inicializar imagem se disponível
  observe({
    if (length(rValues$imgFiles) > 0 && is.null(rValues$img)) {
      rValues$img <- brick(rValues$imgFiles[1])
    }
  })
  
  # Informações da imagem atual
  output$image_info <- renderText({
    req(rValues$imgFiles)
    paste0(
      basename(rValues$imgFiles[rValues$currentIndex]), 
      "\n(", rValues$currentIndex, "/", length(rValues$imgFiles), ")"
    )
  })
  
  # Renderizar imagem
  output$raster <- renderPlot({
    req(rValues$img)
    plotRGB(rValues$img)
    if (!is.null(rValues$clusters) && rValues$plotCluster > 0) {
      I <- rValues$clusters == rValues$plotCluster
      points(rValues$coords[I, ], col = "purple", pch = 20, cex = 0.05)
    }
  })
  
  # Navegação entre imagens
  observeEvent(input$nextImage, {
    req(rValues$imgFiles)
    if (rValues$currentIndex < length(rValues$imgFiles)) {
      rValues$currentIndex <- rValues$currentIndex + 1
      rValues$img <- brick(rValues$imgFiles[rValues$currentIndex])
      # Resetar clusters
      rValues$clusters <- NULL
      rValues$plotCluster <- 0
      updateSelectInput(session, "selectedCluster", choices = NULL)
    }
  })
  
  observeEvent(input$prevImage, {
    req(rValues$imgFiles)
    if (rValues$currentIndex > 1) {
      rValues$currentIndex <- rValues$currentIndex - 1
      rValues$img <- brick(rValues$imgFiles[rValues$currentIndex])
      # Resetar clusters
      rValues$clusters <- NULL
      rValues$plotCluster <- 0
      updateSelectInput(session, "selectedCluster", choices = NULL)
    }
  })
  
  # Executar DBSCAN
  observeEvent(input$scanButton, {
    req(rValues$img)
    
    withProgress(message = 'Executando DBSCAN...', value = 0, {
      # Executar DBSCAN usando o módulo
      result <- runDBSCAN(rValues$img, input$eps, input$minPts)
      
      rValues$clusters <- result$clusters
      rValues$coords <- result$coords
      rValues$plotCluster <- 0
      
      # Processar clusters
      clusterNumbers <- sort(unique(rValues$clusters))
      clusterNumbers <- clusterNumbers[clusterNumbers != 0] # Remover outliers
      rValues$clusterNumbers <- clusterNumbers
      rValues$currentClusterIndex <- ifelse(length(clusterNumbers) > 0, 1, 0)
      
      if (length(clusterNumbers) > 0) {
        updateSelectInput(session, "selectedCluster", choices = clusterNumbers)
      } else {
        updateSelectInput(session, "selectedCluster", choices = NULL)
      }
      
      # Atualizar visualização
      updateClusterCollage()
    })
  })
  
  # Atualizar colagem de clusters
  updateClusterCollage <- function() {
    output$clusterCollage <- renderPlot({
      req(rValues$clusters, rValues$clusterNumbers)
      
      if (length(rValues$clusterNumbers) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Nenhum cluster encontrado") + 
                 theme_void())
      }
      
      # Exibir até 30 clusters por vez
      maxClusters <- min(30, length(rValues$clusterNumbers))
      
      # Cálculo do layout: tentamos manter uma relação de aspecto razoável
      ncols <- ceiling(sqrt(maxClusters))
      
      clusterPlots <- lapply(rValues$clusterNumbers[1:maxClusters], function(clust) {
        I <- rValues$clusters == clust
        pointCount <- sum(I)
        
        ggplot(data = as.data.frame(rValues$coords[I, ]), aes(x = x, y = y)) +
          geom_point(color = ifelse(clust == rValues$plotCluster, "red", "purple"), size = 0.5) +
          ggtitle(paste0("Cluster ", clust, " (", pointCount, ")")) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 8, face = "bold"),
            axis.text = element_text(size = 6),
            plot.margin = margin(2, 2, 2, 2)
          )
      })
      
      do.call(grid.arrange, c(clusterPlots, ncol = ncols))
    })
  }
  
  # Visualizar cluster selecionado
  observeEvent(input$plotSelectedCluster, {
    req(rValues$clusters)
    selectedCluster <- as.numeric(input$selectedCluster)
    if (!is.na(selectedCluster)) {
      rValues$plotCluster <- selectedCluster
      rValues$currentClusterIndex <- which(rValues$clusterNumbers == selectedCluster)
      updateClusterCollage() # Atualizar visualização para destacar o cluster selecionado
    }
  })
  
  # Botão para cluster anterior
  observeEvent(input$prevCluster, {
    req(rValues$clusterNumbers)
    if (length(rValues$clusterNumbers) > 0) {
      if (rValues$currentClusterIndex > 1) {
        rValues$currentClusterIndex <- rValues$currentClusterIndex - 1
      } else {
        rValues$currentClusterIndex <- length(rValues$clusterNumbers)
      }
      
      selectedCluster <- rValues$clusterNumbers[rValues$currentClusterIndex]
      updateSelectInput(session, "selectedCluster", selected = selectedCluster)
      rValues$plotCluster <- selectedCluster
      updateClusterCollage()
    }
  })
  
  # Botão para próximo cluster
  observeEvent(input$nextCluster, {
    req(rValues$clusterNumbers)
    if (length(rValues$clusterNumbers) > 0) {
      if (rValues$currentClusterIndex < length(rValues$clusterNumbers)) {
        rValues$currentClusterIndex <- rValues$currentClusterIndex + 1
      } else {
        rValues$currentClusterIndex <- 1
      }
      
      selectedCluster <- rValues$clusterNumbers[rValues$currentClusterIndex]
      updateSelectInput(session, "selectedCluster", selected = selectedCluster)
      rValues$plotCluster <- selectedCluster
      updateClusterCollage()
    }
  })
  
  # Salvar clusters no banco de dados
  observeEvent(input$saveAllClusters, {
    req(rValues$clusters)
    
    withProgress(message = 'Salvando clusters...', value = 0, {
      tryCatch({
        # Obter nome da imagem
        imageName <- basename(rValues$imgFiles[rValues$currentIndex])
        
        # Obter clusters (exceto outliers)
        clusterNumbers <- sort(unique(rValues$clusters))
        clusterNumbers <- clusterNumbers[clusterNumbers != 0]
        
        if(length(clusterNumbers) == 0) {
          rValues$dbStatus <- "Erro: Não há clusters para salvar"
          return()
        }
        
        # Usar módulo para salvar
        insertedCount <- saveClustersToDB(
          imageName, 
          clusterNumbers, 
          rValues$clusters, 
          rValues$coords, 
          input$eps, 
          input$minPts, 
          rValues$plotCluster,
          db_config$host,
          db_config$name,
          db_config$user,
          db_config$password
        )
        
        # Atualizar status
        rValues$dbStatus <- paste0(
          "Sucesso! ", insertedCount, " clusters salvos.\n",
          "Cluster ", rValues$plotCluster, " marcado como is_solar=1"
        )
        
      }, error = function(e) {
        rValues$dbStatus <- paste0("Erro: ", e$message)
      })
    })
  })
  
  # Renderizar status do DB
  output$dbStatus <- renderText({
    rValues$dbStatus
  })
}

shinyApp(ui, server)
