#' @export
#' @rdname shinyModule
downUploadsUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      sidebarPanel(width = 3,
                   selectInput(ns("activePlot"), label = "Select a saved plot",
                               choices = c("Save or upload a plot ..." = "")),
                   tags$hr(),
                   # Export saved models ####
                   downloadModelUI(id = ns("modelDownload"),
                                   label = "Download plot object(s)"),
                   uploadModelUI(id = ns("modelUpload"),
                                 label = "Upload plot object(s)")
      ),
      mainPanel(width = 8,
                fluidRow(column(8, h4("View the Selected Plot")),
                         column(4,
                                align = "right",
                                # dataExportButton(ns("exportData")),
                                plotExportButton(ns("export")))),
                plotOutput(ns("styledPlot"))

      )
    )
  )
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
#' @param loadedFiles (reactive) list of uploaded files
downUploads <- function(input, output, session, savedData, loadedFiles) {

  activePlotValues <- getPlotValuesDefaults()
  activePlotStyle <- getPlotStyleDefaults()
  morePoints <- reactiveVal(list())

  oneMorePoint <- callModule(addPoints, id = "morePoints")

  observe({
    req(names(savedData()))
    updateSelectInput(session, "activePlot", choices = names(savedData()),
                      selected = names(savedData())[length(savedData())])
  })

  # load saved plot ####
  observeEvent(input$activePlot, {
    req(input$activePlot)
    for (i in names(activePlotValues)) {
      activePlotValues[[i]] <- savedData()[[input$activePlot]]$plotValues[[i]]
    }

    for (i in names(activePlotStyle)) {
      activePlotStyle[[i]] <- savedData()[[input$activePlot]]$plotStyle[[i]]
    }

    morePoints(activePlotStyle$morePoints)

    updateSelectInput(session, "activePoint", choices = names(morePoints()),
                      selected = names(morePoints())[length(morePoints())])
  })

  # render plot ####
  values <- reactiveValues(plot = NULL)
  output$styledPlot <- renderPlot({
    req(input$activePlot)
    req(activePlotValues$predictedData$evenlyOnX)
    makeSinglePlot(reactiveValuesToList(activePlotValues), reactiveValuesToList(activePlotStyle))
    values$plot <- recordPlot()
  })

  # down/upload saved plot ####
  uploadedNotes <- reactiveVal(NULL)
  callModule(downloadModel, id = "modelDownload",
             savedData = savedData, uploadedNotes = uploadedNotes)
  callModule(uploadModel, id = "modelUpload", loadedFiles = loadedFiles,
             savedData = savedData, uploadedNotes = uploadedNotes)

  dataFun <- reactive({
    req(input$activePlot)
    req(activePlotValues$modelData)
    function(xVar, quantile) {
      savedData <- predictPipe(plotRModel = activePlotValues$modelData$modelOutput,
                               xCol = activePlotValues$prepData$X,
                               xVar = xVar,
                               yName = activePlotValues$ySelection$colNames$colName1,
                               quantile = quantile)
      return(savedData)
    }
  })

  #callModule(dataExport, "exportData", dat = dataFun, filename = "modelData")
  callModule(plotExport, "export", reactive(values$plot))
}
