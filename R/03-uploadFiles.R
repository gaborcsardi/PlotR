#' @export
#' @rdname shinyModule
uploadFilesUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      sidebarPanel(width = 3,
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
  fileType <- regmatches(fileName, regexpr(".[[:alnum:]]*$", fileName))

  # remove type
  fileName <- gsub(".[[:alnum:]]*$", "", fileName)

  # extract index
  currentIndex <- regmatches(fileName, regexpr("\\([[:digit:]]+\\)$", fileName))

  # inc index
  if (length(currentIndex) == 0) {
    paste0(fileName, "(1)", fileType)
  } else {
    # get new index
    newIndex <- currentIndex %>%
      gsub(pattern = "\\(|\\)",
           replacement = "") %>%
      as.numeric() + 1

    # replace with new index
    gsub("\\([[:digit:]]+\\)$" , paste0("(", newIndex, ")", fileType) , fileName)
  }
}
