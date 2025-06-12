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

loadImageFiles <- function(dirPath) {
  list.files(dirPath, pattern = "\\.tif$", full.names = TRUE)
}

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

resizeImage <- function(img, maxSize) {
  imgDims <- c(nrow(img), ncol(img))
  maxDim <- max(imgDims)
  
  if (maxDim > maxSize) {
    scaleFactor <- maxSize / maxDim
    newRows <- round(nrow(img) * scaleFactor)
    newCols <- round(ncol(img) * scaleFactor)
    
    img <- aggregate(img, fact = c(nrow(img)/newRows, ncol(img)/newCols), fun = mean)
  }
  
  return(img)
}

runDBSCAN <- function(img, eps_val, minPts_val) {
  val <- getValues(img)
  coords <- coordinates(img)
  
  rgb_values <- NULL
  if (nlayers(img) >= 3) {
    rgb_values <- cbind(
      red = getValues(img[[1]]),
      green = getValues(img[[2]]),
      blue = getValues(img[[3]])
    )
  }
  
  val <- cbind(val, coords)
  
  dbScanResult <- dbscan(val, eps = eps_val, minPts = minPts_val)
  
  return(list(
    clusters = dbScanResult$cluster,
    coords = coords,
    rgb_values = rgb_values
  ))
}

saveClustersToDB <- function(imgName, clusterNumbers, clusters, coords, rgb_values, eps, minPts, solarClusters, 
                             dbHost, dbName, dbUser, dbPassword) {
  conn <- getDBConnection(dbHost, dbName, dbUser, dbPassword)
  insertedCount <- 0
  
  tryCatch({
    dbExecute(conn, "ALTER TABLE clusters_detectados_solar_rec ADD COLUMN IF NOT EXISTS rgb_values jsonb")
  }, error = function(e) {
  })
  
  for(clusterId in clusterNumbers) {
    I <- clusters == clusterId
    clusterCoords <- coords[I, ]
    
    coordsJSON <- toJSON(clusterCoords)
    
    clusterRGB <- NULL
    if (!is.null(rgb_values)) {
      clusterRGB <- rgb_values[I, ]
      rgbJSON <- toJSON(clusterRGB)
    } else {
      rgbJSON <- "null"
    }
    
    isSolar <- ifelse(clusterId %in% solarClusters, 1, 0)
    
    query <- "INSERT INTO clusters_detectados_solar_rec 
              (image_name, id_cluster, is_solar, eps, minPts, coords, rgb_values) 
              VALUES ($1, $2, $3, $4, $5, $6, $7)"
    
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
  
  dbDisconnect(conn)
  
  return(insertedCount)
}

dirPath <- "~/Documents/TCC/SC_24_Z_D_I_2_NE_A_II_1_D_rescaled"

db_config <- list(
  host = "",
  name = "",
  user = "",
  password = "."
)

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
      
      box(
        width = NULL,
        status = "warning",
        title = "Configuração de Imagem",
        solidHeader = TRUE,
        collapsible = TRUE,
        sliderInput("maxImageSize", "Tamanho máximo da imagem (pixels):", 
                    min = 200, max = 3000, value = 1000, step = 50),
        tags$small("Imagens maiores serão redimensionadas para melhor performance")
      ),
      
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
        sliderInput("minPts", "Valor de minPts:", min = 5, max = 200, value = 50, step = 1),
        tags$small("MinPts mínimo: 5 (valores menores tornam o processamento mais pesado)"),
        actionButton("scanButton", "Executar DBSCAN", icon = icon("play"), 
                     class = "btn-white btn-block")
      ),
      
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
      
      box(
        width = NULL,
        status = "warning",
        title = "Painéis Solares Selecionados",
        solidHeader = TRUE,
        collapsible = TRUE,
        div(
          class = "solar-clusters-container", 
          style = "color: black;",
          uiOutput("solarClustersList")
        ),
        tags$br(),
        div(
          style = "display: flex; justify-content: space-between; color: black;",
          actionButton("clearSolarClusters", "Limpar Todos", icon = icon("trash"),
                       class = "btn-white", style = "width: 100%")
        )
      ),
      
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
  imgFiles <- loadImageFiles(dirPath)
  
  rValues <- reactiveValues(
    imgFiles = imgFiles,
    currentIndex = 1,
    img = NULL,
    originalImg = NULL,
    clusters = NULL,
    coords = NULL,
    rgb_values = NULL,
    plotCluster = 0,
    dbStatus = "",
    clusterNumbers = NULL,
    currentClusterIndex = 1,
    solarClusters = c(),
    cachedClusterPlots = list(),
    clustersProcessed = FALSE
  )
  
  observe({
    if (length(rValues$imgFiles) > 0 && is.null(rValues$img)) {
      rValues$originalImg <- brick(rValues$imgFiles[1])
      rValues$img <- resizeImage(rValues$originalImg, input$maxImageSize)
    }
  })
  
  observeEvent(input$maxImageSize, {
    if (!is.null(rValues$originalImg)) {
      rValues$img <- resizeImage(rValues$originalImg, input$maxImageSize)
      rValues$clusters <- NULL
      rValues$plotCluster <- 0
      rValues$solarClusters <- c()
      rValues$cachedClusterPlots <- list()
      rValues$clustersProcessed <- FALSE
      updateSelectInput(session, "selectedCluster", choices = NULL)
    }
  })
  
  output$image_info <- renderText({
    req(rValues$imgFiles)
    imgInfo <- paste0(
      basename(rValues$imgFiles[rValues$currentIndex]), 
      "\n(", rValues$currentIndex, "/", length(rValues$imgFiles), ")"
    )
    
    if (!is.null(rValues$img)) {
      imgInfo <- paste0(
        imgInfo,
        "\nDimensões: ", ncol(rValues$img), "x", nrow(rValues$img)
      )
    }
    
    return(imgInfo)
  })
  
  output$raster <- renderPlot({
    req(rValues$img)
    plotRGB(rValues$img)
    
    if (!is.null(rValues$clusters) && length(rValues$solarClusters) > 0) {
      for (solar_cluster in rValues$solarClusters) {
        I <- rValues$clusters == solar_cluster
        points(rValues$coords[I, ], col = "green", pch = 20, cex = 0.05)
      }
    }
    
    if (!is.null(rValues$clusters) && rValues$plotCluster > 0 && 
        !(rValues$plotCluster %in% rValues$solarClusters)) {
      I <- rValues$clusters == rValues$plotCluster
      points(rValues$coords[I, ], col = "purple", pch = 20, cex = 0.05)
    }
  })
  
  observeEvent(input$nextImage, {
    req(rValues$imgFiles)
    if (rValues$currentIndex < length(rValues$imgFiles)) {
      rValues$currentIndex <- rValues$currentIndex + 1
      rValues$originalImg <- brick(rValues$imgFiles[rValues$currentIndex])
      rValues$img <- resizeImage(rValues$originalImg, input$maxImageSize)
      rValues$clusters <- NULL
      rValues$plotCluster <- 0
      rValues$solarClusters <- c()
      rValues$cachedClusterPlots <- list()
      rValues$clustersProcessed <- FALSE
      updateSelectInput(session, "selectedCluster", choices = NULL)
    }
  })
  
  observeEvent(input$prevImage, {
    req(rValues$imgFiles)
    if (rValues$currentIndex > 1) {
      rValues$currentIndex <- rValues$currentIndex - 1
      rValues$originalImg <- brick(rValues$imgFiles[rValues$currentIndex])
      rValues$img <- resizeImage(rValues$originalImg, input$maxImageSize)
      rValues$clusters <- NULL
      rValues$plotCluster <- 0
      rValues$solarClusters <- c()
      rValues$cachedClusterPlots <- list()
      rValues$clustersProcessed <- FALSE
      updateSelectInput(session, "selectedCluster", choices = NULL)
    }
  })
  
  observeEvent(input$scanButton, {
    req(rValues$img)
    
    withProgress(message = 'Executando DBSCAN...', value = 0, {
      result <- runDBSCAN(rValues$img, input$eps, input$minPts)
      
      rValues$clusters <- result$clusters
      rValues$coords <- result$coords
      rValues$rgb_values <- result$rgb_values
      rValues$plotCluster <- 0
      rValues$solarClusters <- c()
      
      clusterNumbers <- sort(unique(rValues$clusters))
      clusterNumbers <- clusterNumbers[clusterNumbers != 0]
      rValues$clusterNumbers <- clusterNumbers
      rValues$currentClusterIndex <- ifelse(length(clusterNumbers) > 0, 1, 0)
      
      if (length(clusterNumbers) > 0) {
        updateSelectInput(session, "selectedCluster", choices = clusterNumbers)
      } else {
        updateSelectInput(session, "selectedCluster", choices = NULL)
      }
      
      rValues$cachedClusterPlots <- list()
      rValues$clustersProcessed <- TRUE
      
      incProgress(0.2, detail = "Gerando visualizações...")
      
      total_clusters <- length(clusterNumbers)
      if (total_clusters > 0) {
        batch_size <- min(20, total_clusters)
        batches <- ceiling(total_clusters / batch_size)
        
        for (b in 1:batches) {
          start_idx <- (b-1) * batch_size + 1
          end_idx <- min(b * batch_size, total_clusters)
          
          for (i in start_idx:end_idx) {
            clust <- clusterNumbers[i]
            I <- rValues$clusters == clust
            pointCount <- sum(I)
            
            rValues$cachedClusterPlots[[as.character(clust)]] <- list(
              coords = rValues$coords[I, ],
              count = pointCount
            )
          }
          
          incProgress(0.7 * (b/batches), 
                      detail = paste0("Processando cluster batch ", b, "/", batches))
        }
      }
      
      updateClusterCollage()
    })
  })
  
  updateClusterCollage <- function() {
    output$clusterCollage <- renderPlot({
      req(rValues$clustersProcessed, rValues$clusterNumbers)
      
      if (length(rValues$clusterNumbers) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Nenhum cluster encontrado") + 
                 theme_void())
      }
      
      numClusters <- length(rValues$clusterNumbers)
      
      ncols <- ceiling(sqrt(min(numClusters, 30)))
      
      clusterPlots <- lapply(rValues$clusterNumbers, function(clust) {
        clust_str <- as.character(clust)
        
        if (!is.null(rValues$cachedClusterPlots[[clust_str]])) {
          cached_data <- rValues$cachedClusterPlots[[clust_str]]
          coords_df <- as.data.frame(cached_data$coords)
          pointCount <- cached_data$count
          
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
      
      p <- do.call(grid.arrange, c(clusterPlots, ncol = ncols))
      return(p)
    }, height = function() {
      req(rValues$clusterNumbers)
      numClusters <- length(rValues$clusterNumbers)
      if (numClusters == 0) return(300)
      
      ncols <- ceiling(sqrt(min(numClusters, 30)))
      nrows <- ceiling(numClusters / ncols)
      
      return(max(300, nrows * 150))
    })
  }
  
  observeEvent(input$addSolarCluster, {
    req(rValues$clusters)
    selectedCluster <- as.numeric(input$selectedCluster)
    
    if (!is.na(selectedCluster) && !(selectedCluster %in% rValues$solarClusters)) {
      rValues$solarClusters <- c(rValues$solarClusters, selectedCluster)
      updateClusterCollage()
    }
  })
  
  observeEvent(input$removeSolarCluster, {
    clusterToRemove <- as.numeric(input$removeSolarCluster)
    
    if (!is.na(clusterToRemove) && clusterToRemove %in% rValues$solarClusters) {
      rValues$solarClusters <- rValues$solarClusters[rValues$solarClusters != clusterToRemove]
      updateClusterCollage()
    }
  })
  
  observeEvent(input$clearSolarClusters, {
    rValues$solarClusters <- c()
    updateClusterCollage()
  })
  
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
      
      clusterItems <- c(
        clusterItems,
        div(style = "margin-top: 10px; font-weight: bold;",
            paste0("Total: ", length(rValues$solarClusters), " clusters"))
      )
      
      return(div(class = "solar-clusters-list", clusterItems))
    }
  })
  
  observeEvent(input$plotSelectedCluster, {
    req(rValues$clusters)
    selectedCluster <- as.numeric(input$selectedCluster)
    if (!is.na(selectedCluster)) {
      rValues$plotCluster <- selectedCluster
      rValues$currentClusterIndex <- which(rValues$clusterNumbers == selectedCluster)
      updateClusterCollage()
    }
  })
  
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
  
  observeEvent(input$saveAllClusters, {
    req(rValues$clusters)
    
    withProgress(message = 'Salvando clusters...', value = 0, {
      tryCatch({
        imageName <- basename(rValues$imgFiles[rValues$currentIndex])
        
        clusterNumbers <- sort(unique(rValues$clusters))
        clusterNumbers <- clusterNumbers[clusterNumbers != 0]
        
        if(length(clusterNumbers) == 0) {
          rValues$dbStatus <- "Erro: Não há clusters para salvar"
          return()
        }
        
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
  
  output$dbStatus <- renderText({
    rValues$dbStatus
  })
}

shinyApp(ui, server)
