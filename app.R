library(shiny)
library(shinythemes)
library(tidyverse)
library(maptools)
library(jsonlite)
library(openxlsx)
library(xml2)
library(curl)
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
yandex_geosearch_bb <- function(search_req, coords, apikey) {
  coords <- unlist(strsplit(trimws(coords), split = "_"))
  #First, prepare request phrase and convert coordinates from 'lat, lon' into 'lon, lat' format
  request <- URLencode(enc2utf8(search_req))
  coord1 <- unlist(strsplit(trimws(coords[1]), split = ","))
  coord1 <- paste(coord1[2], coord1[1], sep = ",")
  coord2 <- unlist(strsplit(trimws(coords[2]), split = ","))
  coord2 <- paste(coord2[2], coord2[1], sep = ",")
  #Combine a complete url for request  
  first_part <- "https://search-maps.yandex.ru/v1/?apikey="
  url_compl <- gsub(" ", "", paste(first_part, apikey, "&text=", request, "&type=biz&lang=ru_RU&", "bbox=", coord1, "~", coord2, "&results=500", collapse = ""))
  #Obtain a request results in json format  
  full_req <- suppressWarnings(fromJSON(paste(readLines(url_compl, encoding = 'UTF-8'), collapse="")))
  #Desired results are stored in element, called 'feautures'. Here we take from there only name, address, url and coordinates of an object 
  req_data <- full_req$features
  if (length(req_data) < 1) {
    return(NULL)
  }
  prop <- req_data$properties
  geo <- req_data$geometry
  vec_geo <- unlist(geo$coordinates)
  geo_df <- data.frame(lat = vec_geo[seq(2,length(vec_geo), by = 2)], lon = vec_geo[seq(1,length(vec_geo), by = 2)])
  #Combine our vectors in a dataframe
  total <- cbind(prop$name, prop$description, ifelse(is.null(prop$CompanyMetaData$url) == TRUE, rep(NA, length(prop$name)), prop$CompanyMetaData$url), geo_df)
  colnames(total) <- c("Name", "Address", "URL", "Lat", "Lon")
  return(total)
}

geo_find <- function(geocode, apikey, rspn, coord_left_low, coord_right_up) {
  #Combine a complete url for request
  geocode <- paste(unlist(strsplit(geocode, split = " ")), collapse = "+")
  if (rspn == TRUE) {
    coord1 <- unlist(strsplit(coord_left_low, split = ", "))
    coord1 <- paste(coord1[2], coord1[1], sep = ",")
    coord2 <- unlist(strsplit(coord_right_up, split = ", "))
    coord2 <- paste(coord2[2], coord2[1], sep = ",")
    url <- gsub(" ", "", paste('https://geocode-maps.yandex.ru/1.x?apikey=', apikey, "&geocode=", curl_escape(iconv(geocode,"UTF-8")), "&rspn=1", "&bbox=", coord1, "~", coord2, collapse = ""))
  } else {
    url <- gsub(" ", "", paste('https://geocode-maps.yandex.ru/1.x?apikey=', apikey, "&geocode=", curl_escape(iconv(geocode,"UTF-8")), collapse = ""))
  }
  result <- as_list(read_xml(url))
  result_to_parse <- result$ymaps$GeoObjectCollection$featureMember$GeoObject$metaDataProperty$GeocoderMetaData
  found_add <- unlist(lapply(list(result_to_parse$text,
                                  result[["ymaps"]][["GeoObjectCollection"]][["featureMember"]][["GeoObject"]][["Point"]][["pos"]][[1]],
                                  result_to_parse$kind,
                                  result_to_parse$precision,
                                  result_to_parse$AddressDetails$Country$CountryName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$AdministrativeAreaName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$LocalityName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$Thoroughfare$ThoroughfareName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$Thoroughfare$Premise$PremiseNumber), function(x) ifelse(is.null(x) == T, NA, x)))
  return(found_add)
}
  
make_grid <- function(ru, ld, height, width) {
  if (height > 1 || width > 1) {
    coord1 <- as.numeric(unlist(strsplit(trimws(ru), split = ",")))
    coord2 <- as.numeric(unlist(strsplit(trimws(ld), split = ",")))
    s1 <- seq(from = coord2[1], to = coord1[1], length.out = height + 1)
    s2 <- seq(from = coord2[2], to = coord1[2], length.out = width + 1)
    ss <- expand.grid(s1, s2)
    ss <- ss %>% 
      unite(col = coords, Var1, Var2, sep = ", ") %>% 
      mutate(n = rep(1:(length(s1)), length(s2)))
    coords <- matrix(nrow = width, ncol = height)
    for(i in 1:max(ss[, 2])) {
      if (i + 1 < max(ss[, 2]) + 1) {
        lcv <- ss[which(ss[, 2] == i), 1]
        lcv <- lcv[-length(lcv)]
        rcv <- ss[which(ss[, 2] == (i + 1)), 1]
        rcv <- rcv[-1]
        n <- i
        coords[, i] <- paste(lcv, rcv, sep = "_")
      }
    }
    coords <- matrix(coords, ncol = 1)
    } else {
    coords <- paste(ld, ru, sep = "_")
  }
  return(coords)
}

#Элементы пользовательского интерфейса
ui <- tagList(
  navbarPage(
    theme = shinythemes::shinytheme("cerulean"),
    title = "Всякие приложения",
    tabPanel(title = "Коды ОКТМО",
      sidebarPanel(
        h4("На заметку*"),
        helpText("Shp и связанные с ним файлы должны иметь одинаковые названия.
                  В xlsx-файле обязательно должна присутствовать колонка 'point',
                  где координаты должны быть записаны в формате 'долгота широта' с пробелом между ними без каких-либо лишних знаков."),
        hr(),
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
        #Панель вывода результатов (пока проверка координат на корректность написания)
      mainPanel(
      textOutput("Dataset_check")
      )
    ),
    tabPanel(title = "Поиск организаций",
      sidebarPanel(
        fluidRow(column(4)),
        h4("На заметку*"),
        helpText("Для проверки результата запроса на попадание в границы региона и автоматического присвоения кодов ОКТМО,
                 загрузите через меню вкладки 'Коды ОКТМО' и связанные с ним файлы.
                 Координаты должны передаваться в том же виде, в котором их выдают Яндекс-карты.
                 Высота и ширина области поиска подбираются интуитивно."),
        hr(),
        textInput("search_line", label = h5("Введите запрос"), value = "Аптека"),
        textInput("key_line", label = h5("Введите api-ключ"), value = NA),
        textInput("coordru_line", label = h5("Введите координаты области поиска: сначала верхнюю правую, затем нижнюю левую"), value = '58.622468, 31.406503'),
        textInput("coordld_line", label = h5(), value = '58.461637, 31.118112'),
        numericInput("num_line_h", label = h5("Укажите высоту разбивки области поиска"), value = 1, min = 1, max = 10, step = 1),
        numericInput("num_line_w", label = h5("Укажите ширину разбивки области поиска"), value = 1, min = 1, max = 10, step = 1),
        checkboxInput("checkbox_oktmo", label = h5("Проверять координаты на попадание в границы региона"), value = FALSE)
      ),
        #Панель вывода результатов
      mainPanel(
        div(style = "position:absolute;right:1em;", 
            actionButton("Load_yandex_search", "Получить список"),
            downloadButton("Download_yandex_search", label = "Скачать результат")
        ),
        hr(),
        textOutput("Result_check"),
        hr(),
        tableOutput("ya_table")
      )
    ),
    tabPanel(title = "Мини-геокодер",
      sidebarPanel(
        fluidRow(column(4)),
        h4("На заметку*"),
        helpText("Файл с адресами может содержать любое количество столбцов с любыми названиями. Главное - выбрать нужный.
        Результат геокодирования сильно зависит от корректности написания адреса и от наличия в нем посторонней информации."),
        hr(),
        fileInput("address_file", label = h5("Выберите xlsx-файл"), accept = ".xlsx"),
        hr(),
        selectInput("select_col", label = h5("Укажите столбец с адресом"),  choices = "", selected = NULL, multiple = FALSE),
        hr(),
        textInput("geo_key_line", label = h5("Введите api-ключ (лучше использовать свой)"), value = '2ed244eb-29c9-49fa-8508-80a84c1d69b0'),
        hr(),
        checkboxInput("bbox", label = h5("Ограничить область поиска координат"), value = FALSE),
        hr(),
        helpText("При ограничении области поиска, укажите координаты области поиска: сначала верхнюю правую, затем нижнюю левую"),
        textInput("coordru_line_2", label = h5(), value = NULL),
        textInput("coordld_line_2", label = h5(), value = NULL)
      ),
      #Панель вывода результатов
      mainPanel(
        div(style = "position:absolute;right:1em;", 
            actionButton("Start_geocoding", "Получить координаты"),
            downloadButton("Download_yandex_geocode", label = "Скачать результат")
        ),
        hr(),
        tableOutput("Geocode_results")
      )
    )
  )
)

#Серверная часть приложения
server <- function(input, output, session) {
#Устанавливаем максимальный размер загружаемого файла равным 30 Мб
  options(shiny.maxRequestSize = 30*1024^2)
  
#Простановка кодов ОКТМО
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
    showModal(modalDialog("Проверяем координаты на попадание в границы"))
    m <- matrix(c(region_ds()$lon, region_ds()$lat), ncol = 2)
    points_polygon <- SpatialPoints(m, proj4string = crswgs84)
    if (full_dataset() == F) {
      compared <- cbind(region_ds()$point,(over(points_polygon, map_shp())))
    } else {
      compared <- cbind(region_ds(),(over(points_polygon, map_shp())))
    }
    removeModal()
    return(compared)
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

#API Яндекса для поиска организаций
  #Записываем входные параметры
  ru <- reactive({
    input$coordru_line
  })
  ld <- reactive({
    input$coordld_line
  })
  height <- reactive({
    input$num_line_h
  })
  width <- reactive({
    input$num_line_w
  })
  search_req <- reactive({
    input$search_line
  })
  apikey <- reactive({
    input$key_line
  })
  check_oktmo <- reactive({
    input$checkbox_oktmo
  })
  #Разбиваем область поиска на блоки
  coords <- reactive({make_grid(ru(), ld(), height(), width())})
  #Сохраняем результаты группы запросов в один датафрейм
  result_ya <- eventReactive(input$Load_yandex_search, {
    result <- data.frame()
    for (i in 1:length(coords())) {
      res <- yandex_geosearch_bb(search_req(), coords()[[i]], apikey())
      result <- rbind(result, res)
      rm(res)
    }
  #Проверяем (или нет) координаты на попадание в границы и проставляем ОКТМО
    if (check_oktmo() == TRUE) {
      m <- matrix(c(result$Lon, result$Lat), ncol = 2)
      points_polygon <- SpatialPoints(m, proj4string = crswgs84)
      result <- cbind(result,(over(points_polygon, map_shp())))
      result <- result[which(is.na(result[ ,ncol(result)]) == FALSE), ]
      return(result)
    } else {
      return(result)
    }
  })
  #Вывод таблицы с результатами
  output$Result_check <- renderText({
    print(paste("Всего найдено ", nrow(result_ya()), " организаций по запросу.", sep = ""))
  })
  output$ya_table <- renderTable({
    result_ya()
  })
  #Вывод результата в формате xlsx через кнопку загрузки
  output$Download_yandex_search <- downloadHandler(
    filename <- function() {
      "Yandex_search.xlsx"
    },
    content <- function(file) {
      write.xlsx(result_ya(), file)
    }
  )
  
  #API геокодера Яндекса
  #Чтение xlsx-файла и входных параметров
  to_geo <- reactive({
    req(input$address_file)
    input_file <- input$address_file
    file.rename(input_file$datapath, paste(input_file$datapath, ".xlsx", sep=""))
    to_geo <- read.xlsx(paste(input_file$datapath, ".xlsx", sep = ""), sheet = 1, colNames = T, skipEmptyRows = F, skipEmptyCols = F)
    return(to_geo)
  })
  geo_apikey <- reactive({
    input$geo_key_line
  })
  rspn <- reactive({
    input$bbox
  })
  ru_2 <- reactive({
    input$coordru_line_2
  })
  ld_2 <- reactive({
    input$coordld_line_2
  })
  #Передаем названия столбцов для выбора столбца с адресом
  observe({
    req(to_geo())
    updateSelectInput(session = session, inputId = "select_col", choices = colnames(to_geo()))
  })
  #Ищем координаты и ставим шкалу прогресса
  result_geo <- eventReactive(input$Start_geocoding, {
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Ищем координату", value = 0)
    inc <- 0
    geocode_result <- lapply(to_geo()[[input$select_col]], function(x) {
                                                              res <- tryCatch(geo_find(x, rspn = rspn(), apikey = geo_apikey(), coord_left_low = ld_2(), coord_right_up = ru_2()),
                                                              error = function(e) 'error')
                                                              inc <<- inc + 1
                                                              progress$inc(1 / nrow(to_geo()), detail = paste("Найдена", inc, "из", nrow(to_geo())))
                                                              return(res)
                                                           }
    )
    geocode_result <- as.data.frame(do.call(rbind, geocode_result))
    geocode_result <- cbind(to_geo()[[input$select_col]], geocode_result)
    colnames(geocode_result) <- c('Request', 'AddressLine', 'point',	'kind', 'precision', 'Country', 'AdministrativeAreaName',	'LocalityName',	'ThoroughfareName',	'PremiseNumber')
    return(geocode_result)
  })
  #Вывод таблицы с результатами и сохранение их в xlsx-файл
  output$Geocode_results <- renderTable({
    result_geo()
  })
  output$Download_yandex_geocode <- downloadHandler(
    filename <- function() {
      "Yandex_geocode.xlsx"
  },
  content <- function(file) {
    write.xlsx(result_geo(), file)
  }
 )
}

shinyApp(ui, server)