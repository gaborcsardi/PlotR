#' @export
#' @rdname shinyModule
runModelUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      sidebarPanel(
        width = 3,
        fluidRow(
          column(8,
                 selectInput(ns("activeFile"),
                             label = "Select a file",
                             choices = c("Import a file ..." = ""))
          ),
          column(4,
                 align = "left",
                 style = "margin-top: 16px;",
                 actionButton(ns("loadFile"), "Load", width = "90%")
          )),
        tags$br(),
        dataSettingsUI(ns("settings"), "Data Settings"),
        tags$br(),
        modelSettingsUI(ns("modSettings"), "Model Settings"),
        tags$br(),
        actionButton(ns("calculateModel"), "Run Model")
      ),
      mainPanel(
        width = 8,
        fluidRow(column(9, h4("View the Prediction")),
                 column(3, align = "right",
                        dataExportButton(ns("exportData")),
                        plotExportButton(ns("export")))),
        tags$br(),
        fluidRow(column(4, savePlotUI(ns("savingPlot"), label = "Save plot")),
                 column(4,
                        fluidRow(
                          column(8,
                                 style = "margin-top: 10px;",
                                 selectInput(ns("activePlot"),
                                             label = "Load plot",
                                             choices = c("Save or upload a plot ..." = ""))
                          ),
                          column(4,
                                 align = "left",
                                 style = "margin-top: 26px;",
                                 actionButton(ns("loadSavedPlot"), "Apply", width = "80%")
                          ))
                        ),
                 column(4,
                        deletePlotUI(ns("deletingPlot"), "Delete plot(s)"))
                 ),
        # conditionalPanel(
        #   condition = "input.activeFile == NULL",
        #   ns = ns,
        # h5("Nothing to plot. Please upload data first and run the model.")),
        plotOutput(ns("plot")),
        tags$hr(),
        fluidRow(
          column(4,
                 selectInput(inputId = ns("centerType"),
                             label = "Select estimation",
                             choices = c("Mean estimation" = "Estimation",
                                         "Median estimation" = "Est_Median"),
                             selected = 1)
          ),
          column(4,
                 selectInput(inputId = ns("errorType"),
                             label = "Select type of uncertainty",
                             choices = c("SEM" = "SEM",
                                         "SD" = "SD",
                                         "SEM Total" = "SEMTotal"),
                             selected = 1),
                 conditionalPanel(
                   condition =
                     "input.errorType == 'SEM' | input.errorType == 'SEMTotal'",
                   ns = ns,
                   sliderInput(ns("SEQuantile"), "Quantile Coverage",
                               value = 0.95, min = 0.5, max = 0.999,
                               step = 0.001)
                 ),
                 conditionalPanel(
                   condition = "input.errorType == 'SD'",
                   ns = ns,
                   sliderInput(ns("SDFactor"), "SD Factor", value = 2,
                               min = 0.5, max = 3, step = 0.1)
                 )
          )
        )
      )
    )
  )
}

#' @export
#' @rdname shinyModule
#' @param loadedFiles (reactive) list of uploaded files
runModel <- function(input, output, session, loadedFiles) {

  savedData <- reactiveVal(list())
  activeFileData <- reactiveVal(NULL)
  currentNamesOfNumCols <- reactiveVal(emptyColumnChoices())
  currentDataSelection <- list(xColumns = reactiveVal(defaultColSelection()),
                               yColumns = reactiveVal(defaultColSelection()),
                               dataOutlier = reactiveVal(defaultDataOutlier()))
  currentModelParams <- reactiveVal(defaultModelSettings())
  selectedPlot <- reactiveVal(NULL)
  plotValues <- getPlotValuesDefaults()
  plotStyle <- getPlotStyleDefaults()
  values <- reactiveValues(plot = NULL)

  observeEvent(loadedFiles(), {
    req(names(loadedFiles()))
    updateSelectInput(session, "activeFile", choices = names(loadedFiles()),
                      selected = names(loadedFiles())[length(loadedFiles())])
  })

  observeEvent(input$loadFile, {
    req(names(loadedFiles()), input$activeFile)

    activeFileData(loadedFiles()[[input$activeFile]])
    currentNamesOfNumCols(colnames(toNumericCols(activeFileData())))

    # reset to default values
    currentDataSelection$xColumns(defaultColSelection(currentNamesOfNumCols()))
    currentDataSelection$yColumns(defaultColSelection(currentNamesOfNumCols()))
    currentDataSelection$dataOutlier(defaultDataOutlier())

    currentModelParams(defaultModelSettings())

    for (name in names(getPlotValuesDefaults())) {
      plotValues[[name]] <- getPlotValuesDefaults()[[name]]
    }
    for (name in names(getPlotStyleDefaults())) {
      plotStyle[[name]] <- getPlotStyleDefaults()[[name]]
    }

    # update with the defaults
    updateSelectInput(session, "centerType",
                      selected = plotValues$plottedTypeOfPrediction$centerType)
    updateSelectInput(session, "errorType",
                      selected = plotValues$plottedTypeOfPrediction$errorType)
    updateSelectInput(session, "SEQuantile",
                      selected = plotValues$plottedTypeOfPrediction$SEQuantile)
    updateSelectInput(session, "SDFactor",
                  selected = plotValues$plottedTypeOfPrediction$SDFactor)

    values$plot <- NULL
  })

  dataSelection <- callModule(
    dataSettings,
    "settings",
    colNames = currentNamesOfNumCols,
    inputSettings = currentDataSelection
  )

  observeEvent(list(dataSelection$xColumns(),
                    dataSelection$yColumns(),
                    dataSelection$dataOutlier()), {
                      currentDataSelection$xColumns(dataSelection$xColumns())
                      currentDataSelection$yColumns(dataSelection$yColumns())
                      currentDataSelection$dataOutlier(dataSelection$dataOutlier())
  })

  modelParameters <- callModule(
    modelSettings,
    "modSettings",
    modelParameters = currentModelParams
  )

  observeEvent(modelParameters(), {
    currentModelParams(modelParameters())
  })

  # calculate model ####
  observeEvent(input$calculateModel, {
    req(activeFileData())

    plotValues <- getPlotValues(
      plotValues = plotValues,
      activeFile = input$activeFile,
      activeFileData = activeFileData(),
      dataSelection = currentDataSelection,
      modelParameters = currentModelParams()
      )

    plotStyle$xRange <- plotValues$defaultXRange
    plotStyle$xAxisLabel$text <- cleanLabel(plotValues$dataSettings$xColumns)
    plotStyle$yAxisLabel$text <- cleanLabel(plotValues$dataSettings$yColumns)
  })

  # add quantile ####
  observe({
    req(plotValues$predictedData$evenlyOnX)

    plotValues$plottedTypeOfPrediction <- list(centerType = input$centerType,
                                               errorType = input$errorType,
                                               SEQuantile = input$SEQuantile,
                                               SDFactor = input$SDFactor)

    plotValues$predictedData$evenlyOnX <-
      plotValues$predictedData$evenlyOnX %>%
      addQuantiles(quantile = input$SEQuantile)

    plotValues$predictedData$observations <-
      plotValues$predictedData$observations %>%
      addQuantiles(quantile = input$SEQuantile)

    plotStyle$yRange <- getRange(
      data = plotValues$selectedData[, unlist(getSelection(
        plotValues$dataSettings$yColumns
        )$colNames), drop = FALSE],
      type = getSelection(plotValues$dataSettings$yColumns)$type,
      credPercent = getSelection(plotValues$dataSettings$yColumns)$credPercent,
      estimation = unlist(getUncertaintyLimit(
        pred = plotValues$predictedData$evenlyOnX,
        type = input$errorType,
        factor = input$SDFactor
        ))
      )
  })

  # render plot ####
  output$plot <- renderPlot({
    if (is.null(plotValues$predictedData$evenlyOnX)) return(NULL)

    makeSinglePlot(reactiveValuesToList(plotValues),
                   reactiveValuesToList(plotStyle))
    values$plot <- recordPlot()
  })

  # save plot ####
  callModule(savePlot, "savingPlot",
             savedData = savedData,
             currentPlot = reactiveValues(plotValues = plotValues,
                                          plotStyle = plotStyle)
  )

  # load saved plot ####
  observe({
    updateSelectInput(session, "activePlot", choices = names(savedData()),
                      selected = names(savedData())[length(savedData())])
  })

  observeEvent(input$loadSavedPlot, {
    for (i in names(plotValues)) {
      plotValues[[i]] <- savedData()[[input$activePlot]]$plotValues[[i]]
    }

    for (i in names(plotStyle)) {
      plotStyle[[i]] <- savedData()[[input$activePlot]]$plotStyle[[i]]
    }

    currentNamesOfNumCols(colnames(toNumericCols(plotValues$activeFileData)))
    currentDataSelection$xColumns(plotValues$dataSettings$xColumns)
    currentDataSelection$yColumns(plotValues$dataSettings$yColumns)
    currentDataSelection$dataOutlier(plotValues$dataSettings$dataOutlier)
    currentModelParams(plotValues$modelParameters)

    updateSelectInput(session, "activeFile",
                      selected = plotValues$activeFile)
    updateSelectInput(session, "centerType",
                      selected = plotValues$plottedTypeOfPrediction$centerType)
    updateSelectInput(session, "errorType",
                      selected = plotValues$plottedTypeOfPrediction$errorType)
    updateSelectInput(session, "SEQuantile",
                      selected = plotValues$plottedTypeOfPrediction$SEQuantile)
    updateSelectInput(session, "SDFactor",
                      selected = plotValues$plottedTypeOfPrediction$SDFactor)
  })

  # delete plot ####
  callModule(deletePlot, "deletingPlot", savedData = savedData)

  # export data ####
  dataFun <- reactive({
    req(plotValues$modelData)
    function(xVar, quantile) {
      prepData <- getPrepData(
        data = plotValues$selectedData,
        xSelection = getSelection(plotValues$dataSettings$xColumns),
        ySelection = getSelection(plotValues$dataSettings$yColumns))

      data <- predictPipe(
        plotRModel = plotValues$modelData$modelOutput,
        xCol = prepData$X,
        xVar = xVar,
        yName = getSelection(plotValues$dataSettings$yColumns)$colNames$colName1,
        quantile = quantile)
      return(data)
    }
  })

  callModule(dataExport, "exportData", dat = dataFun, filename = "modelData")
  callModule(plotExport, "export", reactive(values$plot))

  return(savedData)
}

getSelection <- function(selectedColumns) {
  colName1 <- switch(selectedColumns$type,
                     "point" = selectedColumns$Point,
                     "interval" = selectedColumns$Min,
                     "credInterval" = selectedColumns$CredMin,
                     "meanSD" = selectedColumns$Mean,
                     "meanSEMSD" = selectedColumns$Mean2)

  colName2 <- switch(selectedColumns$type,
                     "point" = NULL,
                     "interval" = selectedColumns$Max,
                     "credInterval" = selectedColumns$CredMax,
                     "meanSD" = selectedColumns$SD,
                     "meanSEMSD" = selectedColumns$SEMSD)
  credPercent <- switch(selectedColumns$type,
                        "credInterval" = selectedColumns$CredPercent,
                        NULL)

  list(type = selectedColumns$type,
       colNames = list(colName1 = colName1, colName2 = colName2),
       credPercent = credPercent)
}

getPlotValuesDefaults <- function(){
  reactiveValues(
    activeFile = NULL,
    activeFileData = NULL,
    dataSettings = NULL,
    selectedData = NULL,
    modelParameters = NULL,
    modelData = NULL,
    predictedData = NULL,
    defaultXRange = NULL,
    plottedTypeOfPrediction = list(centerType = "Estimation",
                                   errorType = "SEM",
                                   SEQuantile = 0.95,
                                   SDFactor = 2),
    ppValues = list()
  )
}

getPlotStyleDefaults <- function(){
  reactiveValues(
    # default values to create the plot
    # basic plot
    sideXAxis = 1,
    sideYAxis = 2,
    colorBg = '#FFFFFF',
    xRange = c(0,1),
    yRange = c(0,1),
    # titles,
    plotTitle = list(text = "",
                     textColor = "#000000",
                     textSize = 1.2,
                     fontType = 1),
    xAxisLabel = list(text = "",
                      textColor = "#000000",
                      textSize = 1,
                      fontType = 1),
    yAxisLabel = list(text = "",
                      textColor = "#000000",
                      textSize = 1,
                      fontType = 1),
    # data points
    dataPoints = list(color = '#002350',
                      symbol = 19,
                      lineWidth = 2,
                      size = 1,
                      colorBg = '#002350',
                      hide = FALSE),
    dataIntervals = list(color = '#002350',
                         lineType = 1,
                         lineWidth = 2,
                         hide = FALSE),
    # data outlier
    dataOutliers = list(color = '#FF0000',
                        symbol = 19,
                        lineWidth = 2,
                        size = 1,
                        colorBg = '#FF0000',
                        hide = FALSE),
    dataOutlierIntervals = list(color = '#FF0000',
                                lineType = 1,
                                lineWidth = 2,
                                hide = FALSE),
    # model outlier
    modelOutliers = list(color = '#A020F0',
                         symbol = 19,
                         lineWidth = 2,
                         size = 1,
                         colorBg = '#A020F0',
                         hide = FALSE),
    modelOutlierIntervals = list(color = '#A020F0',
                                 lineType = 1,
                                 lineWidth = 2,
                                 hide = FALSE),
    # model prediction
    predictionLine = list(color = '#1D60BD',
                          lineType = 1,
                          lineWidth = 2,
                          hide = FALSE),
    # model uncertainty
    modelUncertainty = list(color = '#1D60BD',
                            lineType = 4,
                            lineWidth = 2,
                            hide = FALSE),
    # more points
    morePoints = list()
  )
}
