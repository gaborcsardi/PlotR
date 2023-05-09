#' @export
#' @rdname shinyModule
stylePlotUI <- function(id, title) {
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
               # Save plot ####
               savePlotUI(ns("savingPlot"), label = "Save plot"),
               deletePlotUI(ns("deletingPlot"), "Delete plot(s)")
             ),
             mainPanel(
               width = 8,
               fluidRow(column(9, h4(
                 "Adjust the Plot Style"
               )),
               column(3,
                      align = "right",
                      plotExportButton(ns(
                        "export"
                      )))),
               plotOutput(ns("styledPlot")),
               tags$hr(),
               fluidRow(
                 column(
                   3,
                   h4("Basic Plot"),
                   colourInput(
                     inputId = ns("colorBg"),
                     label = "Background color",
                     value = '#FFFFFF'
                   ),
                   sliderAndNumericRangeUI(
                     ns("xRange"),
                     label = "x axis range",
                     min = 0,
                     max = 1,
                     value = c(0, 1),
                     dragRange = FALSE
                   ),
                   sliderAndNumericRangeUI(
                     ns("yRange"),
                     label = "y axis range",
                     min = 0,
                     max = 1,
                     value = c(0, 1),
                     dragRange = FALSE
                   ),
                   selectInput(
                     inputId = ns("sideXAxis"),
                     label = "Side of x axis",
                     choices = c("bottom" = "1", "top" = "3"),
                     selected = 1
                   ),
                   selectInput(
                     inputId = ns("sideYAxis"),
                     label = "Side of y axis",
                     choices = c("left" = "2", "right" = "4"),
                     selected = 2
                   )
                 ),
                 column(
                   3,
                   h4("Titles"),
                   selectInput(
                     inputId = ns("labelName"),
                     label = "Select label",
                     choices = c(
                       "plot title" = "plotTitle",
                       "x axis" = "xAxisLabel",
                       "y axis" = "yAxisLabel"
                     ),
                     selected = NA
                   ),
                   textInput(ns("text"), label = "Text", value = NULL),
                   selectInput(
                     ns("fontType"),
                     label = "Font type",
                     choices = fontChoices(),
                     selected = NULL
                   ),
                   colourInput(ns("textColor"), label = "Text color",
                               value = NULL),
                   sliderInput(
                     ns("textSize"),
                     label = "Text size",
                     value = 1.2,
                     min = 0.1,
                     max = 5,
                     step = 0.1
                   ),
                   p(
                     strong("Note:"),
                     "Changing the default sizes of titles and axes might",
                     "lead to not optimal sizes in an ",
                     em("all-in-one-plot"),
                     "in the \"Multiple Plots\" tab."
                   ),
                 ),
                 column(
                   3,
                   h4("Data Points"),
                   selectInput(
                     inputId = ns("pointName"),
                     label = "Select points",
                     choices = c(
                       "data points" = "dataPoints",
                       "data outliers" = "dataOutliers",
                       "model outliers" = "modelOutliers"
                     ),
                     selected = NA
                   ),
                   checkboxInput(
                     inputId = ns("pointHide"),
                     label = "Hide points",
                     value = FALSE,
                     width = "100%"
                   ),
                   selectInput(
                     inputId = ns("pointSymbol"),
                     label = "Symbol",
                     choices = pchChoices(),
                     selected = 19
                   ),
                   colourInput(
                     inputId = ns("pointColor"),
                     label = "Color",
                     value = '#002350'
                   ),
                   sliderInput(
                     inputId = ns("pointSize"),
                     label = "Size",
                     min = 0,
                     max = 20,
                     value = 1
                   ),
                   conditionalPanel(
                     condition = "input.pointSymbol == 21 | input.pointSymbol == 22 |input.pointSymbol == 23 |input.pointSymbol == 24 |input.pointSymbol == 25",
                     ns = ns,
                     colourInput(
                       inputId = ns("pointColorBg"),
                       label = "Background color",
                       value = '#002350'
                     ),
                     sliderInput(
                       inputId = ns("pointLineWidth"),
                       label = "Thickness",
                       min = 0,
                       max = 20,
                       value = 2
                     )
                   )
                 ),
                 column(
                   3,
                   h4("Lines"),
                   selectInput(
                     inputId = ns("lineName"),
                     label = "Select lines",
                     choices = c(
                       "prediction" = "predictionLine",
                       "model uncertainty" = "modelUncertainty",
                       "data intervals" = "dataIntervals",
                       "data outlier intervals" = "dataOutlierIntervals",
                       "model outlier intervals" = "modelOutlierIntervals"
                     ),
                     selected = NA
                   ),
                   checkboxInput(
                     inputId = ns("lineHide"),
                     label = "Hide line",
                     value = FALSE,
                     width = "100%"
                   ),
                   selectInput(
                     inputId = ns("lineType"),
                     label = "Linetype",
                     choices = c(
                       "solid" = "1",
                       "dashed" = "2",
                       "dotted" = "3",
                       "dotdash" = "4",
                       "longdash" = "5",
                       "twodash" = "6"
                     ),
                     selected = 1
                   ),
                   colourInput(
                     inputId = ns("lineColor"),
                     label = "Color",
                     value = '#1D60BD'
                   ),
                   sliderInput(
                     inputId = ns("lineWidth"),
                     label = "Thickness",
                     min = 0,
                     max = 20,
                     value = 2
                   )
                 )
               )
             )
           ))
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
stylePlot <- function(input, output, session, savedData) {
  activePlotValues <- getPlotValuesDefaults()
  activePlotStyle <- getPlotStyleDefaults()

  lastSelected <- reactiveValues(label = NULL,
                                 point = NULL,
                                 line = NULL)

  # basic plot - range
  userInputXRange <- sliderAndNumericRangeServer(
    "xRange",
    value = reactive(activePlotStyle$xRange),
    min = reactive(minRange(activePlotStyle$xRange)),
    max = reactive(maxRange(activePlotStyle$xRange))
  )
  userInputYRange <- sliderAndNumericRangeServer(
    "yRange",
    value = reactive(activePlotStyle$yRange),
    min = reactive(minRange(activePlotStyle$yRange)),
    max = reactive(maxRange(activePlotStyle$yRange))
  )

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
    ## selected plot >> activePlotValues, activePlotStyle ----

    for (i in names(activePlotValues)) {
      activePlotValues[[i]] <-
        savedData()[[input$activePlot]]$plotValues[[i]]
    }

    for (i in names(activePlotStyle)) {
      activePlotStyle[[i]] <-
        savedData()[[input$activePlot]]$plotStyle[[i]]
    }

    ## activePlotValues >> inputs ----
    # plot style
    # basic plot
    updateColourInput(session, "colorBg", value = activePlotStyle$colorBg)
    updateSelectInput(session, "sideXAxis", selected = activePlotStyle$sideXAxis)
    updateSelectInput(session, "sideYAxis", selected = activePlotStyle$sideYAxis)

    # titles
    updateSelectInput(session,
                      "labelName",
                      selected = getLastSelected(lastSelected$label, "plotTitle"))
    updateTextInput(session, "text", value = activePlotStyle[[getLastSelected(lastSelected$label, "plotTitle")]]$text)
    updateColourInput(session, "textColor", value = activePlotStyle[[getLastSelected(lastSelected$label, "plotTitle")]]$textColor)
    updateSliderInput(session, "textSize", value = activePlotStyle[[getLastSelected(lastSelected$label, "plotTitle")]]$textSize)
    updateSelectInput(session, "fontType", selected = activePlotStyle[[getLastSelected(lastSelected$label, "plotTitle")]]$fontType)

    # data points
    updateSelectInput(session,
                      "pointName",
                      selected = getLastSelected(lastSelected$point, "dataPoints"))
    updateColourInput(session, "pointColor", value = activePlotStyle[[getLastSelected(lastSelected$point, "dataPoints")]]$color)
    updateSelectInput(session, "pointSymbol", selected = activePlotStyle[[getLastSelected(lastSelected$point, "dataPoints")]]$symbol)
    updateSliderInput(session, "pointSize", value = activePlotStyle[[getLastSelected(lastSelected$point, "dataPoints")]]$size)
    updateSliderInput(session, "pointLineWidth", value = activePlotStyle[[getLastSelected(lastSelected$point, "dataPoints")]]$lineWidth)
    updateColourInput(session, "pointColorBg", value = activePlotStyle[[getLastSelected(lastSelected$point, "dataPoints")]]$colorBg)
    updateCheckboxInput(session, "pointHide", value = activePlotStyle[[getLastSelected(lastSelected$point, "dataPoints")]]$hide)

    # lines
    updateSelectInput(session,
                      "lineName",
                      selected = getLastSelected(lastSelected$line, "predictionLine"))
    updateColourInput(session, "lineColor", value = activePlotStyle[[getLastSelected(lastSelected$line, "predictionLine")]]$color)
    updateSelectInput(session, "lineType", selected = activePlotStyle[[getLastSelected(lastSelected$line, "predictionLine")]]$lineType)
    updateSliderInput(session, "lineWidth", value = activePlotStyle[[getLastSelected(lastSelected$line, "predictionLine")]]$lineWidth)
    updateCheckboxInput(session, "hideHide", value = activePlotStyle[[getLastSelected(lastSelected$line, "predictionLine")]]$hideHide)
  })

  # activePlotStyle(changed label, point, line) >> inputs ####
  observeEvent(input$labelName, {
    req(input$activePlot, input$labelName)
    updateTextInput(session, "text", value = activePlotStyle[[input$labelName]]$text)
    updateColourInput(session, "textColor", value = activePlotStyle[[input$labelName]]$textColor)
    updateSliderInput(session, "textSize", value = activePlotStyle[[input$labelName]]$textSize)
    updateSelectInput(session, "fontType", selected = activePlotStyle[[input$labelName]]$fontType)
  })

  observeEvent(input$pointName, {
    req(input$activePlot, input$pointName)
    updateColourInput(session, "pointColor", value = activePlotStyle[[input$pointName]]$color)
    updateSelectInput(session, "pointSymbol", selected = activePlotStyle[[input$pointName]]$symbol)
    updateSliderInput(session, "pointSize", value = activePlotStyle[[input$pointName]]$size)
    updateSliderInput(session, "pointLineWidth", value = activePlotStyle[[input$pointName]]$lineWidth)
    updateColourInput(session, "pointColorBg", value = activePlotStyle[[input$pointName]]$colorBg)
    updateCheckboxInput(session, "pointHide", value = activePlotStyle[[input$pointName]]$hide)
  })

  observeEvent(input$lineName, {
    req(input$activePlot, input$lineName)
    updateColourInput(session, "lineColor", value = activePlotStyle[[input$lineName]]$color)
    updateSelectInput(session, "lineType", selected = activePlotStyle[[input$lineName]]$lineType)
    updateSliderInput(session, "lineWidth", value = activePlotStyle[[input$lineName]]$lineWidth)
    updateCheckboxInput(session, "hideHide", value = activePlotStyle[[input$lineName]]$hideHide)
  })

  # inputs >> activePlotStyle ####
  observe({
    # basic plot
    activePlotStyle$xRange <- userInputXRange()
    activePlotStyle$yRange <- userInputYRange()
    activePlotStyle$colorBg <- input$colorBg
    activePlotStyle$sideXAxis <- input$sideXAxis
    activePlotStyle$sideYAxis <- input$sideYAxis
  })

  observe({
    req(input$labelName)
    # titles
    activePlotStyle[[input$labelName]] <- list(
      text = input$text,
      textColor = input$textColor,
      textSize = input$textSize,
      fontType = as.numeric(input$fontType)
    )
  })

  observe({
    req(input$pointName)
    # points
    activePlotStyle[[input$pointName]] <-
      list(
        color = input$pointColor,
        symbol = as.numeric(input$pointSymbol),
        lineWidth = input$pointLineWidth,
        size = input$pointSize,
        colorBg = input$pointColorBg,
        hide = input$pointHide
      )
  })

  observe({
    req(input$lineName)
    # lines
    activePlotStyle[[input$lineName]] <-
      list(
        color = input$lineColor,
        lineType = as.numeric(input$lineType),
        lineWidth = input$lineWidth,
        hide = input$lineHide
      )
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

  # save plot ####
  callModule(
    savePlot,
    "savingPlot",
    savedData = savedData,
    currentPlot = reactiveValues(plotValues = activePlotValues,
                                 plotStyle = activePlotStyle)
  )

  # delete plot ####
  callModule(deletePlot, "deletingPlot", savedData = savedData)

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

getLastSelected <- function(lastSelected, default) {
  ifelse(is.null(lastSelected),
          default,
          lastSelected)
}

pchChoices <- function() {
  c(
    "square" = 0,
    "circle" = 1,
    "triangle point up" = 2,
    "plus" = 3,
    "cross" = 4,
    "diamond" = 5,
    "triangle point down" = 6,
    "square cross" = 7,
    "star" = 8,
    "diamond plus" = 9,
    "circle plus" = 10,
    "triangles up and down" = 11,
    "square plus" = 12,
    "circle cross" = 13,
    "square and triangle down" = 14,
    "filled square" = 15,
    "filled circle" = 16,
    "filled triangle point-up" = 17,
    "filled diamond" = 18,
    "solid circle" = 19,
    "bullet (smaller circle)" = 20,
    "filled circle bg color" = 21,
    "filled square bg color" = 22,
    "filled diamond bg color" = 23,
    "filled triangle point-up bg color" = 24,
    "filled triangle point-down bg color" = 25
  )
}

fontChoices <- function() {
  c(
    "plain text" = 1,
    "bold face" = 2,
    "italic" = 3,
    "bold italic" = 4
  )
}
