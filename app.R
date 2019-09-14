library(shiny)
library(tidyverse)
library(maptools)
library(openxlsx)
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#Элементы пользовательского интерфейса
ui <- fluidPage(
  #Заголовок
  titlePanel("Коды ОКТМО по административным границам"),
  
  sidebarLayout(
  #Панель меню
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
  #Панель вывода резульатов (пока проверка координат на корректность написания)
  mainPanel(
    textOutput("Dataset_check")
  )
 )
)

#Серверная часть приложения
server <- function(input, output) {
  #Чекбокс для вывода полного или урезанного (только координаты и результат) файла
  full_dataset <- reactive({
    input$checkbox
  })
  #Чтение xlsx-файла и преобразование его координат в колонки с долготой и широтой  
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
  #Проверка координат на корректность написания (если координаты некорректны, дальнейшее исполнение не имеет смысла)
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
  #Сначала переименуем shp и связанные с ним файлы, поскольку в shiny они автоматически переименовываются при импорте.
  #Затем ставим нужную кодировку атрибутам.
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
  #Прогоняем датафрейм с координатами (предварительно превратив его в матрицу) по границам shp-файла и сохраняем результат
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
  #Вывод результата в формате xlsx через кнопку загрузки
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