options(shiny.sanitize.errors = FALSE, shiny.maxRequestSize = 120*1024^2)

require(shiny)
require(shinythemes)
require(tidyverse)
require(jsonlite)
require(openxlsx)
require(curl)
require(xml2)
require(sf)
# require(leaflet)
require(RColorBrewer)
# require(memoise)
# require(cachem)
require(geosphere)

source("ya_api_functions.R")
source("make_grid_function.R")
source("read_shp_module.R")
source("point_over_map_function.R")
source("lonlat2UTM.R")
source("parsing_files_for_merging.R")
source("geo_merging.R")
sf_use_s2(FALSE)

# Элементы пользовательского интерфейса
ui <- tagList(
  tags$style(HTML(".shiny-notification {
              position:fixed;
              top: calc(50%);
              left: calc(50%);
              }")
  ),
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
                 загрузите через меню вкладки 'Коды ОКТМО' shp и связанные с ним файлы.
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
        textInput("geo_key_line", label = h5("Введите api-ключ (лучше использовать свой)"), value = '3a5bc8be-5f3a-40c8-8bee-8089834d5bff'),
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
    tabPanel(title = "Объединение списков",
      sidebarPanel(
        h4("На заметку*"),
        helpText("Shp и связанные с ним файлы должны иметь одинаковые названия.
                  В xlsx-файлах обязательно должна присутствовать колонка 'point',
                  где координаты должны быть записаны в формате 'долгота широта' с пробелом между ними без каких-либо лишних знаков.
                  Также должны быть колонки 'id', где записаны уникальные id записей, и 
                  'dist', где указано расстояние в метрах, в пределах которого может быть
                  привязана точка из другого списка.
                  Для записей, у которых отсутствует значение dist, будет проставлено расстояние из настройки ниже.
                  Если не выбран основной файл, то основным будет считаться наибольший по числу записей.
                  Если выключен параметр 'Учитывать совпадающие атрибуты', то объединение будет производиться исключительно по степени близости.
                 НЕ стоит сопоставлять точки с полностью одинаковыми координатами и атрибутами. Результат будет равен
                 квадрату числа таких точек и на больших файлах можно получить объем строк, превышающий
                 возможности формата xlsx"),
        hr(),
        shp_input_UI("shp_file_2", label = h4("Выберите shp и сопутствующие ему файлы")),
        hr(),
        fileInput("xlsx_files", label = h4("Выберите xlsx-файлы"), accept = ".xlsx", multiple = TRUE),
        hr(),
        selectInput("select_master_set", label = h5("Укажите основной файл для сравнения (опционально)"),  choices = "", selected = NULL, multiple = FALSE),
        checkboxInput("check_attributes", label = "Учитывать совпадающие атрибуты", value = TRUE),
        hr(),
        numericInput("max_distance", label = h5("Максимальный радиус объединения (в метрах)"), value = 25, min = 1, max = 500, step = 1),
        fluidRow(column(3))
      ),
        #Панель вывода результатов
      mainPanel(
        div(style = "position:absolute;right:1em;", 
            actionButton("Start_matching", "Сопоставить файлы"),
            downloadButton("Download_merging", label = "Получить файл")
        ),
        tags$style(HTML(".shiny-notification {
              height: 80px;
              width: 400px;
            }")),
        textOutput("Check_input")
      )
    )
  )
)

# Серверная часть приложения
server <- function(input, output, session) {
# Простановка кодов ОКТМО-------------------------------------------------------
# Чекбокс для вывода полного или урезанного (только координаты и результат) файла
  full_dataset <- reactive({
    input$checkbox
  })
# Чтение xlsx-файла и преобразование его координат в колонки с долготой и широтой  
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
# Проверка координат на корректность написания (если координаты некорректны, дальнейшее исполнение не имеет смысла)
  output$Dataset_check <- renderText({
    inFile <- input$xlsx_file
    if (is.null(inFile) == TRUE) {
      return(NULL)
    } else {
      showModal(modalDialog("Проверяем координаты из файла"))
      if (any(is.na(region_ds()$lon) == TRUE) || any(is.na(region_ds()$lat) == TRUE)) {
        bad_rows <- unique(c(which(is.na(region_ds()$lon) == TRUE), which(is.na(region_ds()$lat) == TRUE)))
        removeModal()
        print(paste("Incorrect coordinates in this rows:", paste(sort(bad_rows + 1), collapse = ", ")))
      } else {
        removeModal()
        print("Correct coordinates.")
      }
    }
  })
# Сначала переименуем shp и связанные с ним файлы, поскольку в shiny они автоматически переименовываются при импорте.
# Затем ставим нужную кодировку атрибутам.
  map_shp <- shp_input_server("shp_file")
# Прогоняем датафрейм с координатами (предварительно превратив его в матрицу) по границам shp-файла и сохраняем результат
  result <- reactive({
    if (is.null(map_shp())) {
      return(NULL)
    }
    showModal(modalDialog("Проверяем координаты на попадание в границы"))
    compared <- tryCatch(point_over_map(
        input_df = region_ds(), var_lon = "lon", var_lat = "lat",
        map_over = map_shp(), full_result = full_dataset()
    ), error = function(e) {
        removeModal()
        showNotification("Possibly invalid geometry of polygons! Returning an empty file!", 
                         type = "error", duration = 15
                         )
        return(NULL)
      }
    )
    removeModal()
    if (is.null(compared)) {
      empty_df <- data.frame()
      return(empty_df)
    }
    return(compared)
  })
# Вывод результата в формате xlsx через кнопку загрузки
  output$Download <- downloadHandler(
    filename <- function() {
      "Comparison.xlsx"
    },
    content <- function(file) {
      write.xlsx(result(), file)
    }
  )

# API Яндекса для поиска организаций--------------------------------------------
# Записываем входные параметры
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
# Сначала разбиваем область поиска на блоки.
# Затем сохраняем результаты группы запросов в один датафрейм
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
# Проверяем (или нет) координаты на попадание в границы и проставляем ОКТМО
    if (check_oktmo() == TRUE) {
      result <- point_over_map(
          input_df = result, var_lon = "Lon", var_lat = "Lat", map_over = map_shp()
      )
      return(result)
    } else {
      return(result)
    }
  })
# Вывод таблицы с результатами
  output$Result_check <- renderText({
    print(paste("Всего найдено ", nrow(result_ya()), " организаций по запросу.", sep = ""))
  })
  output$ya_table <- renderTable({
    result_ya()
  })
# Вывод результата в формате xlsx через кнопку загрузки
  output$Download_yandex_search <- downloadHandler(
    filename <- function() {
      "Yandex_search.xlsx"
    },
    content <- function(file) {
      write.xlsx(result_ya(), file)
    }
  )
  
# API геокодера Яндекса---------------------------------------------------------
# Чтение xlsx-файла и входных параметров
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
# Передаем названия столбцов для выбора столбца с адресом
  observe({
    req(to_geo())
    updateSelectInput(session = session, inputId = "select_col", choices = colnames(to_geo()))
  })
# Ищем координаты и ставим шкалу прогресса
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
# Вывод таблицы с результатами и сохранение их в xlsx-файл
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
  
# Объединение списков-----------------------------------------------------------
# Читаем шейпы
  region_shp <- shp_input_server("shp_file_2")
# Читаем xlsx-файлы
  input_dfs <- reactive({
    req(input$xlsx_files)
    input_files <- input$xlsx_files
    tempdirname <- dirname(input_files$datapath[1])
    list_df <- list()
    showModal(modalDialog("Сохраняем файлы"))
    for (i in 1:nrow(input_files)) {
      file.rename(input_files$datapath[i], paste0(tempdirname, "/", input_files$name[i]))
      list_df[[i]] <- suppressWarnings(
        read.xlsx(paste0(tempdirname, "/", input_files$name[i]), sheet = 1, colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)
      )
    }
    names(list_df) <- str_remove_all(input_files$name, ".xlsx")
    removeModal()
    return(list_df)
  })
  observe({
    req(input_dfs())
    updateSelectInput(session = session, inputId = "select_master_set", choices = c("", names(input_dfs())))
  })
  master_set <- reactive({
    input$select_master_set
  })
  check_attrs <- reactive({
    input$check_attributes
  })
  max_dist <- reactive({
    input$max_distance
  })
  # Проверяем файлы специальной функцией
  clear_dfs <- reactive({
    req(input_dfs(), region_shp())
    parsing_dfs(region_shp(), input_dfs())
  })
  observe({
    req(clear_dfs())
    if (clear_dfs()$status == 'err') {
      showNotification(paste0(clear_dfs()$message, collapse = ""),
                       type = "warning", duration = 10)
    }
    if (clear_dfs()$status == 'ok') {
      output$Check_input <- renderPrint(clear_dfs()[c("status", "Common attributes:")])
    }
  })
  # Сопоставляем точки из файлов
  result_merging <- eventReactive(input$Start_matching, {
    req(clear_dfs())
    result <- geo_merging(master_set = master_set(), d_max = max_dist(),
                check_attributes = check_attrs(), list_df = clear_dfs()$list_df,
                list_attr = clear_dfs()$list_attr, region_name = clear_dfs()$region_name,
                region_name_column = clear_dfs()$region_name_column)
    return(result)
  })
  observeEvent(input$Start_matching, {
    req(result_merging())
    output$Check_input <- renderPrint("Можно скачать!")
  })
  # Сохраняем результат
  output$Download_merging <- downloadHandler(
    filename <- function() {
      "Merged_files.xlsx"
    },
    content <- function(file) {
      write.xlsx(result_merging(), file)
    }
  )
}

shinyApp(ui, server)
