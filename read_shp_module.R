# Модуль для импорта shp-файлов.  
# Сначала переименуем shp и связанные с ним файлы, поскольку в shiny они автоматически переименовываются/ при импорте.
# Затем ставим нужную кодировку атрибутам.

shp_input_UI <- function(id, label) {
  ns <- NS(id)
  fileInput(ns("shp_file"), label, multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"))
}

shp_input_server <- function(id) {
  moduleServer(id,
    function(input, output, session) {
      shpdf <- reactive({
        req(input$shp_file)
        input$shp_file
      })
      region <- reactive({
        tempdirname <- dirname(shpdf()$datapath[1])
        for (i in 1:nrow(shpdf())) {
          file.rename(shpdf()$datapath[i], paste0(tempdirname, "/", shpdf()$name[i]))
        }
        suppressWarnings(st_read(paste(tempdirname, shpdf()$name[grep(pattern = "*.shp$", shpdf()$name)], sep = "/")) %>% 
                           st_set_crs(4326))
      })
    return(region)
    }
  )
}
