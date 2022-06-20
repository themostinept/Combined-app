options(shiny.sanitize.errors = FALSE, shiny.maxRequestSize = 30*1024^2)

require(shiny)
require(shinythemes)
require(tidyverse)
require(jsonlite)
require(openxlsx)
require(curl)
require(sf)
require(leaflet)
require(RColorBrewer)

source("ya_api_functions.R")
source("make_grid_function.R")
source("read_shp_module.R")
source("point_over_map_function.R")

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
    )
    # ),
    # tabPanel(title = "Потоки",
    #   sidebarPanel(
    #     h4("На заметку*"),
    #     helpText("Shp и связанные с ним файлы должны иметь одинаковые названия.
    #     Содержимое и названия xlsx-файлов менять не следует."),
    #     hr(),
    #     fileInput("shp_file", label = h4("Выберите shp и сопутствующие ему файлы"), multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")),
    #     hr(),
    #     fluidRow(column(4)),
    #     fileInput("xlsx_file_flows", label = h4("Выберите xlsx-файлы"), multiple = TRUE, accept = ".xlsx"),
    #     hr(),
    #     fluidRow(column(4)),
    #     selectInput("select_col_periods", label = h5("Укажите номер периода"),  choices = "", selected = 1, multiple = FALSE),
    #     checkboxInput("checkbox", label = "Показать без карты", value = TRUE),
    #     hr(),
    #     fluidRow(column(3)),
    #     downloadButton("Download", label = "Скачать файл потоков")
    #   ),
    #   #Панель вывода результатов (пока проверка координат на корректность написания)
    #   mainPanel(
    #     textOutput("Dataset_check")
    #   )
    # )
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
                    textInput(paste0("coordru_line", area_num), label = h5(strong(paste0(area_num, ") Верхняя правая"))), value = '58.622468, 31.406503'),
                    textInput(paste0("coordld_line", area_num), label = h5(strong("Нижняя левая")), value = '58.461637, 31.118112')),
        splitLayout(cellWidths = c("50%","50%"),
                    numericInput(paste0("num_line_h", area_num), label = h5("Высота разбивки"), value = 1, min = 1, max = 10, step = 1),
                    numericInput(paste0("num_line_w", area_num), label = h5("Ширина разбивки"), value = 1, min = 1, max = 10, step = 1))
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
        coords <- rbind(coords, make_grid(input[[paste0("coordru_line", i)]], input[[paste0("coordld_line", i)]], input[[paste0("num_line_h", i)]], input[[paste0("num_line_w", i)]]))
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
      result <- point_over_map(input_df = result, var_lon = "Lon", var_lat = "Lat",
                               map_over = map_shp())
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
}

shinyApp(ui, server)
