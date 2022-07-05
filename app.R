options(shiny.sanitize.errors = FALSE, shiny.maxRequestSize = 120*1024^2)

require(shiny)
require(shinythemes)
require(tidyverse)
require(jsonlite)
require(openxlsx)
require(curl)
require(xml2)
require(sf)
require(leaflet)
require(RColorBrewer)
require(memoise)
require(cachem)

source("ya_api_functions.R")
source("make_grid_function.R")
source("read_shp_module.R")
source("point_over_map_function.R")
source("plot_flows_functions.R", encoding = "UTF-8")

#Элементы пользовательского интерфейса
ui <- tagList(
  tags$style(type = "text/css", "#map_flows {height: calc(100vh - 80px) !important;}"),
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
        shp_input_UI("shp_file", label = h4("Выберите shp и сопутствующие ему файлы")),
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
        checkboxInput("checkbox_oktmo", label = h5("Проверять координаты на попадание в границы региона"), value = FALSE),
        p("Для каждой области поиска введите координаты: сначала верхнюю правую, затем нижнюю левую. После этого установите высоту и ширину
          разбивки области поиска на прямоугольники."),
        actionButton("add", "Добавить область"),
        actionButton("remove", "Удалить область"),
        div(id = "searcharea", style = "padding: 22px; border: 1px solid silver;")
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
    ),
    tabPanel(title = "Потоки",
      #Панель вывода карты
      leafletOutput("map_flows", height = "100%", width = "100%"),
      absolutePanel(
        id = "controls", fixed = TRUE, draggable = TRUE,
        top = 60, left = "auto", right = 10, bottom = "auto",
        width = 330, height = "auto",
        style = "background-color: rgba(205,205,205,0.7)",
        h4("На заметку*"),
        p("Shp и связанные с ним файлы должны иметь одинаковые названия.
        Содержимое и названия xlsx-файлов менять не следует.",  align = "center"),
        hr(),
        shp_input_UI("shp_file_flows", label = h5("Выберите shp и сопутствующие ему файлы")),
        hr(),
        fluidRow(column(4)),
        fileInput("xlsx_file_flows", label = h5("Выберите xlsx-файлы"), multiple = TRUE, accept = ".xlsx"),
        selectInput("select_col_periods", label = h5("Укажите номер периода"),  choices = "", selected = 1, multiple = FALSE),
        checkboxInput("checkbox_aggregate", label = h5("Объединять источники по муниципальным образованиям"), value = TRUE),
        textInput("zone_name", label = h5("Введите название группы зон"), value = "Зоны транспортирования"),
        hr(),
        fluidRow(column(3)),
        downloadButton("Download_flows", label = "Скачать файл потоков")
      )
    )
  )
)

#Серверная часть приложения
server <- function(input, output, session) {
  
#Простановка кодов ОКТМО
  #Чекбокс для вывода полного или урезанного (только координаты и результат) файла
  full_dataset <- reactive({
    input$checkbox
  })
  #Чтение xlsx-файла и преобразование его координат в колонки с долготой и широтой  
  region_ds <- reactive({
    inFile <- input$xlsx_file
    if (is.null(inFile) == TRUE) {
      return(NULL)
    }
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep = ""))
    region_dataset <- suppressWarnings(read.xlsx(paste(inFile$datapath, ".xlsx", sep = ""), sheet = 1, colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE) %>% 
                                         separate(point, into = c("lon", "lat"), sep = "[[:space:]]", remove = FALSE, convert = TRUE) %>%  
                                         mutate(lon = as.numeric(gsub(",", ".", lon)), lat = as.numeric(gsub(",", ".", lat))))
    return(region_dataset)  
  })
  #Проверка координат на корректность написания (если координаты некорректны, дальнейшее исполнение не имеет смысла)
  output$Dataset_check <- renderText({
    inFile <- input$xlsx_file
    if (is.null(inFile) == TRUE) {
      return(NULL)
    }
    if (any(is.na(region_ds()$lon) == TRUE) || any(is.na(region_ds()$lat) == TRUE)) {
      bad_rows <- unique(c(which(is.na(region_ds()$lon) == TRUE), which(is.na(region_ds()$lat) == TRUE)))
      print(paste("Incorrect coordinates in this rows:", paste(sort(bad_rows + 1), collapse = ", ")))
      } else {
        print("Correct coordinates.")
    }
  })
  #Сначала переименуем shp и связанные с ним файлы, поскольку в shiny они автоматически переименовываются при импорте.
  #Затем ставим нужную кодировку атрибутам.
  map_shp <- shp_input_server("shp_file")
  #Прогоняем датафрейм с координатами (предварительно превратив его в матрицу) по границам shp-файла и сохраняем результат
  result <- reactive({
    if (is.null(map_shp()) == TRUE) {
      return(NULL)
    }
    showModal(modalDialog("Проверяем координаты на попадание в границы"))
    compared <- point_over_map(input_df = region_ds(), var_lon = "lon", var_lat = "lat",
                               map_over = map_shp(), full_result = full_dataset())
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
  values <- reactiveValues(num_areas = 0)
  observeEvent(input$add, ignoreNULL = FALSE, {
    Sys.sleep(0.5)
    values$num_areas <- values$num_areas + 1
    area_num <<- values$num_areas
    insertUI(
      selector = "#searcharea", where = "beforeEnd",
      fluidRow(
        splitLayout(cellWidths = c("50%","50%"),
                    textInput(paste0("coordru_line", area_num),
                              label = h5(strong(paste0(area_num, ") Верхняя правая"))),
                              value = '58.622468, 31.406503'),
                    textInput(paste0("coordld_line", area_num),
                              label = h5(strong("Нижняя левая")),
                              value = '58.461637, 31.118112')),
        splitLayout(cellWidths = c("50%","50%"),
                    numericInput(paste0("num_line_h", area_num),
                                 label = h5("Высота разбивки"),
                                 value = 1, min = 1, max = 10, step = 1),
                    numericInput(paste0("num_line_w", area_num),
                                 label = h5("Ширина разбивки"),
                                 value = 1, min = 1, max = 10, step = 1))
      )
    )
  })
  observeEvent(input$remove, {
    Sys.sleep(0.5)
    if (area_num > 1) {
      removeUI(
        selector = "div.row:last-child"
      )
      values$num_areas <- values$num_areas - 1
      area_num <<- values$num_areas
    }
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
  #Сначала разбиваем область поиска на блоки.
  #Затем сохраняем результаты группы запросов в один датафрейм
  result_ya <- eventReactive(input$Load_yandex_search, {
    coords <- make_grid(input[[paste0("coordru_line", 1)]], input[[paste0("coordld_line", 1)]], input[[paste0("num_line_h", 1)]], input[[paste0("num_line_w", 1)]])
    if (area_num > 1) {
      for (i in 2:area_num) {
        coords <- rbind(coords, make_grid(input[[paste0("coordru_line", i)]],
                                          input[[paste0("coordld_line", i)]],
                                          input[[paste0("num_line_h", i)]],
                                          input[[paste0("num_line_w", i)]]))
      }
    }
    result <- data.frame()
    for (i in 1:length(coords)) {
      res <- yandex_geosearch_bb(search_req(), coords[i], apikey())
      result <- rbind(result, res)
      rm(res)
    }
    result <- distinct_at(result, c(1, 2), .keep_all = TRUE)
  #Проверяем (или нет) координаты на попадание в границы и проставляем ОКТМО
    if (check_oktmo() == TRUE) {
      result <- point_over_map(input_df = result, var_lon = "Lon", var_lat = "Lat", map_over = map_shp())
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
    to_geo <- read.xlsx(paste(input_file$datapath, ".xlsx", sep = ""), sheet = 1, colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
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
    geocode_result <- lapply(to_geo()[[input$select_col]],
                             function(x) {
                              res <- tryCatch(yandex_geocode(x, rspn = rspn(), apikey = geo_apikey(), coord_left_low = ld_2(), coord_right_up = ru_2()),
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
  
  ##Просмотр потоков
  map_base <- shp_input_server("shp_file_flows")
  output$map_flows <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(37.601147, 55.775249, zoom = 17)
  })
  #Читаем список названий xlsx-файлов и переименовываем их
  #Затем читаем сами файлы
  debug_data <- reactive({
    req(input$xlsx_file_flows, map_base())
    debug_files <- input$xlsx_file_flows
    tempdirname <- dirname(debug_files$datapath[1])
    for (i in 1:nrow(debug_files)) {
      file.rename(debug_files$datapath[i], paste0(tempdirname, "/", debug_files$name[i]))
    }
    parsed_files <- paste0(tempdirname, "/", debug_files$name)
    inc <<- 0
    progress <<- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Обработка файлов", value = 0)
    debug_files <- get_flows_df(flows_files = parsed_files,
                                         region = map_base(),
                                         zone_name = input$zone_name)
    return(debug_files)
  })
  observe({
    req(map_base(), debug_data())
    updateSelectInput(session = session, 
                      inputId = "select_col_periods", 
                      choices = as.numeric(unlist(str_extract_all(debug_data()$periods, "[[:digit:]]"))))
    b <- unname(st_bbox(map_base()))
    leafletProxy("map_flows", data = map_base()) %>%
      addPolygons(fillOpacity = 0.4,
                  fillColor = "lightblue",
                  color = "black",
                  weight = 1) %>%
      addScaleBar(position = "bottomleft") %>% 
      flyToBounds(lng1 = b[1], lat1 = b[2], lng2 = b[3], lat2 = b[4])
  })
  # Ставим цвета потокам и выводим потоки на экран
  flow_col <- reactive({
    req(debug_data())
    cf <- colorFactor(palette = "Set1", domain = unique(debug_data()$flows_aggr$output_treat_name))
    Sys.sleep(2)
    return(cf)
  })
  observeEvent(input$select_col_periods, ignoreNULL = TRUE, {
    req(flow_col())
    get_flows_period(flows_result = debug_data(),
                     flow_map = map_base(),
                     flow_col = flow_col(),
                     set_period = input$select_col_periods,
                     show_aggregated_flows = input$checkbox_aggregate,
                     leafletmap_name = "map_flows")
  })
  
  output$Download_flows <- downloadHandler(
    filename <- function() {
      "mw_flows.xlsx"
    },
    content <- function(file) {
      out_xlsx <- st_drop_geometry(debug_data()$flows_aggr) %>% 
        select(-c(original_id, treatment_type, treat_id))
      out_xlsx_long <- st_drop_geometry(debug_data()$flows) %>% 
        select(-c(original_id, treatment_type, treat_id))
      out_xlsx_names <- c("ID начальной точки", "ID конечной точки", "Масса, тонн",
                          "Длина потока, км.", "Объем, куб. м", "Тип обращения",
                          "Период", "Тип потока", "ID административной территории",
                          "Начальная точка", "Конечная точка")
      colnames(out_xlsx) <- out_xlsx_names
      colnames(out_xlsx_long) <- out_xlsx_names
      write.xlsx(list("Потоки от территорий" = out_xlsx, 
                      "Потоки от источников" = out_xlsx_long), file)
    }
  )
}

shinyApp(ui, server)
