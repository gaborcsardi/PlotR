#' @export
#' @rdname shinyModule
downUploadsUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(title,
           id = id,
           value = id,
           fluidRow(
             sidebarPanel(
               style = "position:fixed; width:23%; max-width:500px; overflow-y:auto; height:88%",
               width = 3,
               selectInput(
                 ns("activePlot"),
                 label = "Select a saved plot",
                 choices = c("Save or upload a plot ..." = "")
               ),
               tags$hr(),
               # Export saved models ####
               importDataUI(ns("modelUpload"), label = "Import Plot"),
               tags$hr(),
               selectInput(ns("selectedModels"), label = "Download plot object(s)",
                           choices = c("Save or upload plots ..." = ""),
                           multiple = T),
               downloadModelUI(ns("modelDownload"), label = "Download")
             ),
             mainPanel(width = 8,
                       fluidRow(
                         column(8, h4("View the Selected Plot")),
                         column(4,
                                align = "right",
                                # dataExportButton(ns("exportData")),
                                plotExportButton(ns("export")))
                       ),
                       plotOutput(ns("styledPlot")))
           ))
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
#' @param loadedFiles (reactive) list of uploaded files
#' @param config (list) list with configuration parameters
downUploads <-
  function(input,
           output,
           session,
           savedData,
           loadedFiles,
           config) {
    activePlotValues <- getPlotValuesDefaults()
    activePlotStyle <- getPlotStyleDefaults()

    observe({
      req(names(savedData()))
      updateSelectInput(
        session,
        "activePlot",
        choices = names(savedData()),
        selected = names(savedData())[length(savedData())]
      )
    })

    # load saved plot ####
    observeEvent(input$activePlot, {
      req(input$activePlot)
      for (i in names(activePlotValues)) {
        activePlotValues[[i]] <-
          savedData()[[input$activePlot]]$plotValues[[i]]
      }

      for (i in names(activePlotStyle)) {
        activePlotStyle[[i]] <-
          savedData()[[input$activePlot]]$plotStyle[[i]]
      }
    })

    # render plot ####
    values <- reactiveValues(plot = NULL)
    output$styledPlot <- renderPlot({
      validate(
        need(input$activePlot, "Select a plot ...")
      )
      req(input$activePlot)
      validate(
        need(!is.null(activePlotValues$defaultXRange), "Data not valid ...")
      )

      makeSinglePlot(
        reactiveValuesToList(activePlotValues),
        reactiveValuesToList(activePlotStyle)
      )
      values$plot <- recordPlot()
    })

    # down/upload saved plot ####
    observe({
      updateSelectInput(session, "selectedModels", choices = names(savedData()),
                        selected = names(savedData())[length(savedData())])
    })

    uploadedNotes <- reactiveVal(NULL)
    downloadModelServer("modelDownload",
                        dat = reactive(savedData()[input$selectedModels] %>%
                                         removeModelOutputs()),
                        inputs = reactiveValues(),
                        model = reactive(savedData()[input$selectedModels] %>%
                                           extractModelOutputs()),
                        rPackageName = config$rPackageName,
                        fileExtension = config$fileExtension,
                        modelNotes = uploadedNotes,
                        triggerUpdate = reactive(TRUE))

    uploadedValues <- importDataServer("modelUpload",
                                       title = "Import Model",
                                       defaultSource = config$defaultSourceModel,
                                       importType = "model",
                                       rPackageName = config$rPackageName,
                                       ignoreWarnings = TRUE,
                                       fileExtension = config$fileExtension)


    observe({
      req(length(uploadedValues()) > 0)
      # update notes in tab down-/upload ----
      uploadedNotes(uploadedValues()[[1]][["notes"]])

      # prepare model object(s)
      uploadedData <- extractSavedModels(upload = uploadedValues()[[1]])

      # rename model if name already exists
      if (any(names(uploadedData) %in% names(savedData()))) {
                nameExists <- which(names(uploadedData) %in% names(savedData()))
                shinyalert(
                  title = "Duplicated plot names",
                  text = paste(
                    "Plot name\n",
                    paste(names(uploadedData)[nameExists], collapse = ", "),
                    "\n already exist and was updated."
                  ),
                  type = "warning"
                )

                # rename duplicated plot names
                newNames <- names(uploadedData)
                while (any(newNames %in% names(savedData()))) {
                  nameExists <- which(newNames %in% names(savedData()))
                  newNames[nameExists] <- lapply(newNames[nameExists], incIndexOfName)
                  names(uploadedData) <- newNames
                }
      }

      # prepare data file(s)
      uploadedFileNames <- c()
      for (plot in names(uploadedData)) {
        newFileName <- uploadedData[[plot]]$plotValues$activeFile

        # rename duplicated files
        while (any(newFileName == names(loadedFiles()))) {
          newFileName <- incIndexOfFile(newFileName)
          uploadedData[[plot]]$plotValues$activeFile <- newFileName
        }

        uploadedFileNames <- c(uploadedFileNames, newFileName)
      }

      uploadedFiles <- lapply(names(uploadedData), function(plot){
        uploadedData[[plot]]$plotValues$activeFileData
      })
      uploadedFiles <- setNames(uploadedFiles, uploadedFileNames)

      # load data files and models ----
      loadedFiles(c(loadedFiles(), uploadedFiles[unique(unlist(uploadedFileNames))]))
      savedData(c(savedData(), uploadedData))
    }) %>%
      bindEvent(uploadedValues())

    # dataFun <- reactive({
    #   req(input$activePlot)
    #   req(activePlotValues$modelData)
    #   function(xVar, quantile) {
    #     savedData <-
    #       predictPipe(
    #         plotRModel = activePlotValues$modelData$modelOutput,
    #         xCol = activePlotValues$prepData$X,
    #         xVar = xVar,
    #         yName = activePlotValues$ySelection$colNames$colName1,
    #         quantile = quantile
    #       )
    #     return(savedData)
    #   }
    # })

    #callModule(dataExport, "exportData", dat = dataFun, filename = "modelData")
    callModule(plotExport, "export", reactive(values$plot))
  }
