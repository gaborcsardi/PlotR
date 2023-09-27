#' @export
#' @rdname shinyModule
uploadFilesUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(title,
           id = id,
           value = id,
           fluidRow(
             sidebarPanel(
               style = "position:fixed; width:23%; max-width:500px; overflow-y:auto; height:88%",
               width = 3,
               importDataUI(ns("data"), "Import Data"),
               tags$hr(),
               selectInput(
                 ns("activeFile"),
                 label = "View the imported file",
                 choices = c("Import a file ..." = "")
               ),
               tags$hr()
             ),
             mainPanel(width = 8,
                       DTOutput(ns("preview")))
           ))
}


#' @export
#' @rdname shinyModule
#' @param config (list) list with configuration parameters
uploadFiles <- function(input, output, session, config) {
  loadedFiles <- reactiveVal(list())
  activeFile <- reactiveVal(NULL)

  importedData <- importDataServer(
    "data",
    customWarningChecks = list(reactive(checkWarningEmptyValues)),
    customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
    ignoreWarnings = TRUE,
    defaultSource = config$defaultSourceData
  )

  observe({
    req(names(loadedFiles()))
    updateSelectInput(
      session,
      "activeFile",
      choices = names(loadedFiles()),
      selected = names(loadedFiles())[length(loadedFiles())]
    )
  })

  activeFile <- reactive({
    req(names(loadedFiles()))
    loadedFiles()[[input$activeFile]]
  })

  observeEvent(importedData(), {
    req(names(importedData()))

    tmpImport <- importedData()
    newFileName <- names(tmpImport)

    # rename duplicated files
    while (any(newFileName == names(loadedFiles()))) {
      newFileName <- incIndexOfFile(newFileName)
      names(tmpImport) <- newFileName
    }

    loadedFiles(c(loadedFiles(), tmpImport))
  })

  output$preview <- renderDT({
    req(activeFile())
    activeFile()
  })

  return(loadedFiles)
}


#' Inc Index Of File
#'
#' If the file has no index, add a new index: "(1)". If an index already exists, increase it by one.
#'
#' @param fileName (character) filename
incIndexOfFile <- function(fileName) {
  # extract type
  fileType <-
    regmatches(fileName, regexpr(".[[:alnum:]]*$", fileName))

  # remove type
  fileName <- gsub(".[[:alnum:]]*$", "", fileName)

  fileName <- incIndexOfName(fileName)

  paste0(fileName, fileType)
}
