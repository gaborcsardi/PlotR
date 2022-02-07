#' @export
#' @rdname shinyModule
uploadFilesUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    useShinyalert(),
    fluidRow(
      sidebarPanel(width = 2,
                   importDataUI(ns("data"), "Import Data"),
                   tags$hr(),
                   selectInput(ns("activeFile"),
                               label = "View the uploaded file",
                               choices = NULL),
                   tags$hr()
      ),
      mainPanel(width = 8,
                DTOutput(ns("preview"))
      )
    )
  )
}


#' @export
#' @rdname shinyModule
uploadFiles <- function(input, output, session) {

  loadedFiles <- reactiveVal(list())
  activeFile <- reactiveVal(NULL)

  importedData <- callModule(importData, "data",
                             customWarningChecks = list(
                               checkWarningEmptyValues
                             ),
                             customErrorChecks = list(
                               checkErrorNoNumericColumns
                             ))

  observe({
    req(names(loadedFiles()))
    updateSelectInput(session, "activeFile", choices = names(loadedFiles()),
                      selected = names(loadedFiles())[length(loadedFiles())])
  })

  activeFile <- reactive({
    req(names(loadedFiles()))
    loadedFiles()[[input$activeFile]]
  })

  observeEvent(importedData(), {
    req(names(importedData()))

    if (names(importedData()) %in% names(loadedFiles())) {
      loadedFiles(loadedFiles()[names(loadedFiles()) != names(importedData())])
    }

    loadedFiles(c(loadedFiles(), importedData()))

  })

  output$preview <- renderDT({
    req(activeFile())
    activeFile()
  })

  return(loadedFiles)
}
