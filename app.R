library(shiny)
library(tidyverse)
library(maptools)
library(openxlsx)
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

ui <- fluidPage(
  
  titlePanel("Коды ОКТМО по административным границам"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("shp_file", label = h4("Выберите shp и сопутствующие ему файлы"), multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")),
      hr(),
      fluidRow(column(4)),
      fileInput("xlsx_file", label = h4("Выберите xlsx-файл"), accept = ".xlsx"),
      hr(),
      fluidRow(column(4)),
      checkboxInput("checkbox", label = "Сохранить исходный файл полностью", value = TRUE),
      hr(),
      fluidRow(column(3)),
      downloadButton("Download", label = "Скачать результат")
    ),
  
  mainPanel(
    textOutput("Dataset_check")
  )
 )
)


server <- function(input, output) {
  
  full_dataset <- reactive({
    input$checkbox
  })
    
  region_ds <- reactive({
    inFile <- input$xlsx_file
    if (is.null(inFile) == T) {
      return(NULL)
    }
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    
    region_dataset <- suppressWarnings(read.xlsx(paste(inFile$datapath, ".xlsx", sep = ""), sheet = 1, colNames = T, skipEmptyRows = F, skipEmptyCols = F) %>% 
                                         separate(point, into = c("lon", "lat"), sep = "[[:space:]]", remove = F, convert = T) %>%  
                                         mutate(lon = as.numeric(gsub(",", ".", lon)), lat = as.numeric(gsub(",", ".", lat))))
    return(region_dataset)  
  })

  output$Dataset_check <- renderText({
    inFile <- input$xlsx_file
    if (is.null(inFile) == T) {
      return(NULL)
    }
    if (any(is.na(region_ds()$lon) == T) || any(is.na(region_ds()$lat) == T)) {
      bad_rows <- unique(c(which(is.na(region_ds()$lon) == T), which(is.na(region_ds()$lat) == T)))
      print(paste("Incorrect coordinates in this rows:", paste(sort(bad_rows + 1), collapse = ", ")))
      } else {
      print("Correct coordinates.")
    }
  })
  
  map_shp <- reactive({
    req(input$shp_file)
    shpdf <- input$shp_file
    tempdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    region <- suppressWarnings(readShapePoly(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"), proj4string=crswgs84, verbose = TRUE, delete_null_obj = T))
    region@data$NAME <- unlist(lapply(region@data$NAME, function(x) iconv(x, 'UTF-8')))
    return(region)
  })
  

  result <- reactive({
    if (is.null(map_shp) == T) {
      return(NULL)
    }
    withProgress({
      setProgress(message = "Идет обработка...")
      m <- matrix(c(region_ds()$lon, region_ds()$lat), ncol = 2)
      points_polygon <- SpatialPoints(m, proj4string = crswgs84)
      if (full_dataset() == F) {
        compared <- cbind(region_ds()$point,(over(points_polygon, map_shp())))
      } else {
        compared <- cbind(region_ds(),(over(points_polygon, map_shp())))
      }
      return(compared)
    })
  })
  
  output$Download <- downloadHandler(
    filename <- function() {
      "Comparison.xlsx"
    },
    content <- function(file) {
      write.xlsx(result(), file)
    }
  )
}

shinyApp(ui, server)