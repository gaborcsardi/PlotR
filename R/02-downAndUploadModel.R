# Download a plot ####

#' @rdname shinyModule
downloadModelUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    tags$hr(),
    tags$h4(label),
    # pickerInput(ns("selectedModels"), "Select saved models",
    #             choices = NULL,
    #             options = list(`actions-box` = TRUE),
    #             multiple = T),
    selectInput(ns("selectedModels"), label = NULL,
                choices = NULL,
                multiple = T),
    textAreaInput(ns("notes"), "Add notes"),
    downloadButton(ns("downloadModelButton"), "Download")
  )
}

#' @rdname shinyModule
#' @param uploadedNotes (reactive) variable that stores content for README.txt
downloadModel <- function(input, output, session, savedData, uploadedNotes){

  # disable the downdload button on page load
  shinyjs::disable("downloadModelButton")

  observe({
    updateSelectInput(session, "selectedModels", choices = names(savedData()),
                      selected = names(savedData())[length(savedData())])

    updateTextAreaInput(session, "notes", value = uploadedNotes())

    if (length(savedData())) {
      # enable the download button
      shinyjs::enable("downloadModelButton")
      return(NULL)
    }

  })

  output$downloadModelButton <- downloadHandler(
    filename = function() {
      gsub("[ ]", "_", paste0(Sys.time(), "_PlotR.zip"))
    },
    content = function(file) {
      zipdir <- tempdir()
      modelfile <- file.path(zipdir, "model.Rdata")
      notesfile <- file.path(zipdir, "README.txt")

      req(savedData(), input$selectedModels)
      model <- savedData()[input$selectedModels]
      save(model, file = modelfile)
      writeLines(input$notes %>% addPackageVersionNo(),
                 notesfile)
      zipr(file, c(modelfile, notesfile))
    }
  )

}

# Upload a plot ####

#' @rdname shinyModule
uploadModelUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    tags$hr(),
    tags$h4(label),
    fileInput(ns("uploadModel"), label = NULL)
  )
}

#' @rdname shinyModule
#' @param uploadedNotes (reactive) variable that stores content for README.txt
uploadModel <- function(input, output, session, loadedFiles, savedData,
                        uploadedNotes){

  observeEvent(input$uploadModel, {
    model <- NULL

    res <- try({
      zip::unzip(input$uploadModel$datapath)
      load("model.Rdata")
    })

    if (inherits(res, "try-error") || !exists("model")) {
      shinyalert("Could not read model from file", type = "error")
      return()
    }

    if (!is.null(model)) {
      savedData(c(savedData(), model))
      updateSelectInput(session, "activePlot", choices = names(savedData()),
                        selected = names(savedData())[length(savedData())])

      uploadedFileNames <- lapply(names(savedData()), function(plot){
        savedData()[[plot]]$plotValues$activeFile
      })
      uploadedFiles <- lapply(names(savedData()), function(plot){
        savedData()[[plot]]$plotValues$activeFileData
      })

      uploadedFiles <- setNames(uploadedFiles, uploadedFileNames)
      uploadedFiles <- uploadedFiles[unique(unlist(uploadedFileNames))]

      loadedFiles(c(loadedFiles(), uploadedFiles[unique(unlist(uploadedFileNames))]))
      updateSelectInput(session, "activeFile", choices = names(loadedFiles()),
                        selected = savedData()[[names(savedData())[length(savedData())]]]$plotValues$activeFile)

      uploadedNotes(readLines("README.txt")[[1]])

      shinyalert("Model loaded", type = "success")
    } else {
      shinyalert("Empty model loaded", type = "warning")
    }

  })
}

addPackageVersionNo <- function(txt){
  versionNo <- packageVersion("PlotR") %>%
    as.character()

  paste0(txt, "\n\n", "PlotR version ", versionNo, " .")
}
