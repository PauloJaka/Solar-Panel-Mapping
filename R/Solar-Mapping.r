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
  
  # Extrair valores RGB
  rgb_values <- NULL
  if (nlayers(img) >= 3) {
    rgb_values <- cbind(
      red = getValues(img[[1]]),
      green = getValues(img[[2]]),
      blue = getValues(img[[3]])
    )
  }
  
  val <- cbind(val, coords)
  
  # Executar DBSCAN
  dbScanResult <- dbscan(val, eps = eps_val, minPts = minPts_val)
  
  # Resultados
  return(list(
    clusters = dbScanResult$cluster,
    coords = coords,
    rgb_values = rgb_values
  ))
}

# Módulo para salvar clusters no banco
saveClustersToDB <- function(imgName, clusterNumbers, clusters, coords, rgb_values, eps, minPts, solarClusters, 
                             dbHost, dbName, dbUser, dbPassword) {
  # Conectar ao banco de dados
  conn <- getDBConnection(dbHost, dbName, dbUser, dbPassword)
  insertedCount <- 0
  
  # Adicionar coluna RGB se não existir
  tryCatch({
    dbExecute(conn, "ALTER TABLE clusters_detectados_solar_rec ADD COLUMN IF NOT EXISTS rgb_values jsonb")
  }, error = function(e) {
    # Ignora erro se a coluna já existir
  })
  
  # Processar cada cluster
  for(clusterId in clusterNumbers) {
    # Determinar quais pontos pertencem a este cluster
    I <- clusters == clusterId
    clusterCoords <- coords[I, ]
    
    # Converter coordenadas para JSON
    coordsJSON <- toJSON(clusterCoords)
    
    # Extrair valores RGB para este cluster
    clusterRGB <- NULL
    if (!is.null(rgb_values)) {
      clusterRGB <- rgb_values[I, ]
      rgbJSON <- toJSON(clusterRGB)
    } else {
      rgbJSON <- "null"
    }
    
    # Verificar se é um cluster solar marcado
    isSolar <- ifelse(clusterId %in% solarClusters, 1, 0)
    
    # Preparar query SQL de inserção
    query <- "INSERT INTO clusters_detectados_solar_rec 
              (image_name, id_cluster, is_solar, eps, minPts, coords, rgb_values) 
              VALUES ($1, $2, $3, $4, $5, $6, $7)"
    
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
        coordsJSON,
        rgbJSON
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
  host = "",
  name = "",
  user = "",
  password = "."
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
        .scrollable-container {
          max-height: 450px;
          overflow-y: auto;
        }
        .selected-solar {
          color: #28a745;
          font-weight: bold;
        }
        .solar-cluster-item {
          margin: 5px 0;
          padding: 5px;
          background-color: #f8f9fa;
          border-radius: 4px;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .solar-cluster-item button {
          padding: 2px 5px;
          margin-left: 5px;
        }
        .solar-clusters-container {
          max-height: 150px;
          overflow-y: auto;
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
        tags$style(HTML("
    #eps-label, #minPts-label {
      color: black !important;
    }
  ")),
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
                     class = "btn-white btn-block"),
        tags$br(),
        actionButton("addSolarCluster", "Adicionar como Painel Solar", icon = icon("plus"), 
                     class = "btn-white btn-block")
      ),
      
      # Clusters Solares Selecionados
      box(
        width = NULL,
        status = "warning",
        title = "Painéis Solares Selecionados",
        solidHeader = TRUE,
        collapsible = TRUE,
        div(
          class = "solar-clusters-container", 
          style = "color: black;",  # Adiciona cor preta ao texto
          uiOutput("solarClustersList")
        ),
        tags$br(),
        div(
          style = "display: flex; justify-content: space-between; color: black;",  # Adiciona cor preta aqui também
          actionButton("clearSolarClusters", "Limpar Todos", icon = icon("trash"),
                       class = "btn-white", style = "width: 100%")
        )
      ),
      
      # Banco de Dados
      box(
        width = NULL,
        status = "danger",
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
          width = NULL, height = "500px",
          div(class = "scrollable-container",
              plotOutput("clusterCollage", height = "auto")
          )
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
    rgb_values = NULL,
    plotCluster = 0,
    dbStatus = "",
    clusterNumbers = NULL,
    currentClusterIndex = 1,
    solarClusters = c(), # Lista para armazenar clusters marcados como painéis solares
    cachedClusterPlots = list(), # Cache para plots de clusters
    clustersProcessed = FALSE    # Flag para indicar se os clusters já foram processados
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
    
    # Plotar todos os clusters solares selecionados
    if (!is.null(rValues$clusters) && length(rValues$solarClusters) > 0) {
      for (solar_cluster in rValues$solarClusters) {
        I <- rValues$clusters == solar_cluster
        points(rValues$coords[I, ], col = "green", pch = 20, cex = 0.05)
      }
    }
    
    # Plotar o cluster atualmente visualizado
    if (!is.null(rValues$clusters) && rValues$plotCluster > 0 && 
        !(rValues$plotCluster %in% rValues$solarClusters)) {
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
      rValues$solarClusters <- c() # Reset solar clusters
      rValues$cachedClusterPlots <- list() # Limpar cache
      rValues$clustersProcessed <- FALSE # Resetar flag de processamento
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
      rValues$solarClusters <- c() # Reset solar clusters
      rValues$cachedClusterPlots <- list() # Limpar cache
      rValues$clustersProcessed <- FALSE # Resetar flag de processamento
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
      rValues$rgb_values <- result$rgb_values
      rValues$plotCluster <- 0
      rValues$solarClusters <- c() # Reset solar clusters
      
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
      
      # Limpar cache e gerar novos plots de clusters
      rValues$cachedClusterPlots <- list()
      rValues$clustersProcessed <- TRUE
      
      # Criar cache de plots para cada cluster (para melhorar performance)
      incProgress(0.2, detail = "Gerando visualizações...")
      
      # Gerar plots de clusters em lotes para evitar travamento da UI
      total_clusters <- length(clusterNumbers)
      if (total_clusters > 0) {
        batch_size <- min(20, total_clusters)  # Processa no máximo 20 clusters por vez
        batches <- ceiling(total_clusters / batch_size)
        
        for (b in 1:batches) {
          start_idx <- (b-1) * batch_size + 1
          end_idx <- min(b * batch_size, total_clusters)
          
          for (i in start_idx:end_idx) {
            clust <- clusterNumbers[i]
            I <- rValues$clusters == clust
            pointCount <- sum(I)
            
            # Gerar e armazenar o plot em cache
            rValues$cachedClusterPlots[[as.character(clust)]] <- list(
              coords = rValues$coords[I, ],
              count = pointCount
            )
          }
          
          incProgress(0.7 * (b/batches), 
                      detail = paste0("Processando cluster batch ", b, "/", batches))
        }
      }
      
      # Atualizar visualização
      updateClusterCollage()
    })
  })
  
  # Atualizar colagem de clusters - usando cache
  updateClusterCollage <- function() {
    output$clusterCollage <- renderPlot({
      req(rValues$clustersProcessed, rValues$clusterNumbers)
      
      if (length(rValues$clusterNumbers) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Nenhum cluster encontrado") + 
                 theme_void())
      }
      
      # Exibir todos os clusters usando os dados em cache
      numClusters <- length(rValues$clusterNumbers)
      
      # Cálculo do layout
      ncols <- ceiling(sqrt(min(numClusters, 30)))
      
      clusterPlots <- lapply(rValues$clusterNumbers, function(clust) {
        clust_str <- as.character(clust)
        
        if (!is.null(rValues$cachedClusterPlots[[clust_str]])) {
          cached_data <- rValues$cachedClusterPlots[[clust_str]]
          coords_df <- as.data.frame(cached_data$coords)
          pointCount <- cached_data$count
          
          # Define cores diferentes para clusters solares selecionados
          if (clust %in% rValues$solarClusters) {
            pointColor <- "green"
            titlePrefix <- "✓ Solar: "
          } else if (clust == rValues$plotCluster) {
            pointColor <- "red"
            titlePrefix <- ""
          } else {
            pointColor <- "purple"
            titlePrefix <- ""
          }
          
          ggplot(data = coords_df, aes(x = x, y = y)) +
            geom_point(color = pointColor, size = 0.5) +
            ggtitle(paste0(titlePrefix, "Cluster ", clust, " (", pointCount, ")")) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 8, face = "bold"),
              axis.text = element_text(size = 6),
              plot.margin = margin(2, 2, 2, 2)
            )
        } else {
          # Fallback se o cache não estiver disponível
          I <- rValues$clusters == clust
          pointCount <- sum(I)
          
          if (clust %in% rValues$solarClusters) {
            pointColor <- "green"
            titlePrefix <- "✓ Solar: "
          } else if (clust == rValues$plotCluster) {
            pointColor <- "red"
            titlePrefix <- ""
          } else {
            pointColor <- "purple"
            titlePrefix <- ""
          }
          
          ggplot(data = as.data.frame(rValues$coords[I, ]), aes(x = x, y = y)) +
            geom_point(color = pointColor, size = 0.5) +
            ggtitle(paste0(titlePrefix, "Cluster ", clust, " (", pointCount, ")")) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 8, face = "bold"),
              axis.text = element_text(size = 6),
              plot.margin = margin(2, 2, 2, 2)
            )
        }
      })
      
      # Ajusta a altura do plot para acomodar todos os clusters
      p <- do.call(grid.arrange, c(clusterPlots, ncol = ncols))
      return(p)
    }, height = function() {
      req(rValues$clusterNumbers)
      # Calcular altura baseada no número de clusters
      numClusters <- length(rValues$clusterNumbers)
      if (numClusters == 0) return(300)
      
      ncols <- ceiling(sqrt(min(numClusters, 30)))
      nrows <- ceiling(numClusters / ncols)
      
      # Altura mínima de 300px, depois escala proporcionalmente
      return(max(300, nrows * 150))
    })
  }
  
  # Adicionar cluster como painel solar
  observeEvent(input$addSolarCluster, {
    req(rValues$clusters)
    selectedCluster <- as.numeric(input$selectedCluster)
    
    if (!is.na(selectedCluster) && !(selectedCluster %in% rValues$solarClusters)) {
      rValues$solarClusters <- c(rValues$solarClusters, selectedCluster)
      updateClusterCollage() # Atualizar visualização para mostrar seleção
    }
  })
  
  # Remover cluster individual da seleção de painéis solares
  observeEvent(input$removeSolarCluster, {
    clusterToRemove <- as.numeric(input$removeSolarCluster)
    
    if (!is.na(clusterToRemove) && clusterToRemove %in% rValues$solarClusters) {
      rValues$solarClusters <- rValues$solarClusters[rValues$solarClusters != clusterToRemove]
      updateClusterCollage() # Atualizar visualização
    }
  })
  
  # Limpar seleção de clusters solares
  observeEvent(input$clearSolarClusters, {
    rValues$solarClusters <- c()
    updateClusterCollage()
  })
  
  # Interface para lista de clusters solares selecionados
  output$solarClustersList <- renderUI({
    if (length(rValues$solarClusters) == 0) {
      return(p("Nenhum cluster selecionado como painel solar."))
    } else {
      clusterItems <- lapply(sort(rValues$solarClusters), function(cluster) {
        div(class = "solar-cluster-item",
            span(paste0("Cluster ", cluster)),
            actionButton(
              inputId = paste0("remove_", cluster),
              label = "",
              icon = icon("times"),
              class = "btn-sm btn-danger",
              onclick = sprintf("Shiny.setInputValue('removeSolarCluster', %d)", cluster)
            )
        )
      })
      
      # Adicionar contagem total
      clusterItems <- c(
        clusterItems,
        div(style = "margin-top: 10px; font-weight: bold;",
            paste0("Total: ", length(rValues$solarClusters), " clusters"))
      )
      
      return(div(class = "solar-clusters-list", clusterItems))
    }
  })
  
  # Visualizar cluster selecionado
  observeEvent(input$plotSelectedCluster, {
    req(rValues$clusters)
    selectedCluster <- as.numeric(input$selectedCluster)
    if (!is.na(selectedCluster)) {
      rValues$plotCluster <- selectedCluster
      rValues$currentClusterIndex <- which(rValues$clusterNumbers == selectedCluster)
      # Não reprocessamos toda a colagem, apenas atualizamos a visualização
      updateClusterCollage()
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
        
        # Usar módulo para salvar com a lista de clusters solares
        insertedCount <- saveClustersToDB(
          imageName, 
          clusterNumbers, 
          rValues$clusters, 
          rValues$coords,
          rValues$rgb_values,
          input$eps, 
          input$minPts, 
          rValues$solarClusters,
          db_config$host,
          db_config$name,
          db_config$user,
          db_config$password
        )
        
        # Atualizar status
        solarCount <- length(rValues$solarClusters)
        rValues$dbStatus <- paste0(
          "Sucesso! ", insertedCount, " clusters salvos.\n",
          solarCount, " clusters marcados como painéis solares"
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
