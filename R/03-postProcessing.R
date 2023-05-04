#' @export
#' @rdname shinyModule
postProcessingUI <- function(id, title) {
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
                 choices = c("Save or upload a plot ..." = ""),
                 multiple = F
               ),
               tags$hr(),
               selectInput(
                 ns("operation"),
                 label = "Choose operation",
                 choices = c(
                   "Derivative" = "dev",
                   "Greater or equal" = "geq",
                   "Less or qual" = "leq",
                   "Equal" = "eq"
                 ),
                 selected = "geq"
               ),
               conditionalPanel(
                 ns = ns,
                 condition = "input.operation != 'dev'",
                 tags$hr(),
                 radioButtons(
                   ns("compare"),
                   label = "Constrain prediction to",
                   choices = c(
                     "Y-value" = "Value",
                     "Interval of y predictions" = "Value interval",
                     "Plot" = "Plot"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.compare == 'Value interval'",
                   ns = ns,
                   selectInput(
                     ns("xValueTypeC"),
                     label = "Choose x value limits of comparison",
                     choices = c(
                       "For x greater than" = "gx",
                       "For x lower than" = "lx",
                       "For x in interval" = "intx"
                     ),
                     selected = "intx"
                   ),
                   conditionalPanel(
                     condition = "input.xValueTypeC == 'gx' || input.xValueTypeC == 'lx' || input.xValueTypeC == 'intx' ",
                     ns = ns,
                     numericInput(
                       ns("compX1C"),
                       label = NULL,
                       value = NULL,
                       width = "70%"
                     )
                   ),
                   conditionalPanel(
                     condition = "input.xValueTypeC == 'intx' ",
                     ns = ns,
                     numericInput(
                       ns("compX2C"),
                       label = "Upper x value",
                       value = NULL,
                       width = "70%"
                     )
                   ),
                 ),
                 conditionalPanel(
                   condition = "input.compare == 'Value'",
                   ns = ns,
                   numericInput(ns("compY"), label = "Y-value",
                                value = NULL)
                 ),

                 conditionalPanel(
                   condition = "input.compare == 'Plot'",
                   ns = ns,
                   selectInput(
                     ns("comparePlots"),
                     label = "Select plot to compare",
                     choices = NULL,
                     multiple = F#,
                     #selectize = TRUE
                   ),
                 ),
                 selectInput(
                   ns("xValueType"),
                   label = "Choose x value limits to constrain",
                   choices = c(
                     "For all x" = "allx",
                     "For x greater than" = "gx",
                     "For x lower than" = "lx",
                     "For x in interval" = "intx"
                   ),
                   selected = "allx"
                 ),
                 conditionalPanel(
                   condition = "input.xValueType == 'gx' || input.xValueType == 'lx' || input.xValueType == 'intx' ",
                   ns = ns,
                   numericInput(
                     ns("compX1"),
                     label = NULL,
                     value = NULL,
                     width = "70%"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.xValueType == 'intx' ",
                   ns = ns,
                   numericInput(
                     ns("compX2"),
                     label = "Upper x value",
                     value = NULL,
                     width = "70%"
                   )
                 ),
                 tags$hr(),
                 numericInput(
                   ns("unc"),
                   label = "Uncertainty",
                   min = 0,
                   value = 0,
                   width = "70%"
                 )
               ),
               tags$hr(),
               textInput(
                 ns("plotName"),
                 NULL,
                 value = "",
                 placeholder = "name your post-processed plot"
               ),
               actionButton(ns("postPlot"), "Apply post processing"),
             ),
             mainPanel(
               width = 8,
               h4("View the Selected Plot"),
               plotOutput(ns("viewSelectedPlot")),
               tags$hr(),
               fluidRow(column(9, h4(
                 "View the Post Processing"
               )),
               column(3,
                      align = "right",
                      plotExportButton(ns(
                        "export"
                      )))),
               plotOutput(ns("viewPostPlot"))
             )
           ))
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
postProcessing <- function(input, output, session, savedData) {
  activePostPlot <- reactiveVal(NULL)
  values <- reactiveValues(plot = NULL)

  observe({
    req(names(savedData()))
    updateSelectInput(session, "activePlot", choices = names(savedData()))
    updateSelectInput(session, "comparePlots", choices = names(savedData()))
  })

  observe({
    if (input$xValueType == "gx" | input$xValueType == "intx") {
      updateNumericInput(session, "compX1", label = "Lower x value")
    }
    if (input$xValueType == "lx") {
      updateNumericInput(session, "compX1", label = "Upper x value")
    }
    if (input$xValueTypeC == "gx" | input$xValueTypeC == "intx") {
      updateNumericInput(session, "compX1C", label = "Lower x value")
    }
    if (input$xValueTypeC == "lx") {
      updateNumericInput(session, "compX1C", label = "Upper x value")
    }

  })
  observeEvent(input$postPlot, {
    req(input$activePlot)

    postPlotValues <- savedData()[[input$activePlot]]$plotValues

    ppValues <- postPlotValues$ppValues
    ppValuesNew <- list(
      compare = input$compare,
      compY = input$compY,
      xValueTypeC = input$xValueTypeC,
      compX1C = input$compX1C,
      compX2C = input$compX2C,
      xValueType = input$xValueType,
      compX1 = input$compX1,
      compX2 = input$compX2,
      operation = input$operation,
      unc = input$unc
    )
    if (input$compare == "Plot") {
      ppValuesNew$Plot <-
        savedData()[[input$comparePlots]]$plotValues$modelData$modelOutput
      ppValues <-
        append(ppValues, savedData()[[input$comparePlots]]$plotValues$ppValues)
    }
    ppValues <- append(ppValues, list(ppValuesNew))

    postPlotValues$ppValues <- ppValues

    prepData <- getPrepData(
      data = postPlotValues$selectedData,
      xSelection = getSelection(postPlotValues$dataSettings$xColumns),
      ySelection = getSelection(postPlotValues$dataSettings$yColumns)
    )

    predictedData <- predictData(
      modelData = postPlotValues$modelData$modelOutput,
      prepData = prepData,
      smoothConst = postPlotValues$modelParameters$smoothConst,
      postProcessing = TRUE,
      ppValues = ppValues
    ) %>%
      tryCatchWithWarningsAndErrors(errorTitle = "Prediction failed", alertStyle = "shinyalert")

    req(!is.null(predictedData))
    postPlotValues$predictedData <- predictedData

    postPlotStyle <- savedData()[[input$activePlot]]$plotStyle

    postPlotStyle$xRange <- getRange(
      data = postPlotValues$selectedData[, unlist(getSelection(postPlotValues$dataSettings$xColumns)$colNames), drop = FALSE],
      type = getSelection(postPlotValues$dataSettings$xColumns)$type,
      credPercent = getSelection(postPlotValues$dataSettings$xColumns)$credPercent,
      estimation = postPlotValues$predictedData$evenlyOnX$xVar
    )

    postPlotStyle$yRange <- getRange(
      data = postPlotValues$selectedData[, unlist(getSelection(postPlotValues$dataSettings$yColumns)$colNames), drop = FALSE],
      type = getSelection(postPlotValues$dataSettings$yColumns)$type,
      credPercent = getSelection(postPlotValues$dataSettings$yColumns)$credPercent,
      estimation = unlist(
        getUncertaintyLimit(
          postPlotValues$predictedData$evenlyOnX,
          type = "SEM",
          factor = 2
        )
      )
    )

    if (input$plotName == "") {
      shinyalert("Oops!",
                 paste("Please enter a name to save this plot."),
                 type = "error")
    } else if (input$plotName %in% names(savedData())) {
      shinyalert("Oops!",
                 paste("This name already exists. Please provide a unique name."),
                 type = "error")
    } else {
      savedData(c(savedData(),
                  setNames(list(
                    list(
                      plotName = input$plotName,
                      plotValues = postPlotValues,
                      plotStyle = postPlotStyle
                    )
                  ),
                  input$plotName)))
    }

    activePostPlot(savedData()[[input$plotName]])
  })

  output$viewSelectedPlot <- renderPlot({
    validate(
      need(input$activePlot, "Select a plot ...")
    )
    req(input$activePlot)
    validate(
      need(!is.null(savedData()[[input$activePlot]]$plotValues$defaultXRange), "Data not valid ...")
    )

    makeSinglePlot(savedData()[[input$activePlot]]$plotValues,
                   savedData()[[input$activePlot]]$plotStyle)
  })

  output$viewPostPlot <- renderPlot({
    validate(
      need(activePostPlot(), "Apply post processing ...")
    )

    makeSinglePlot(activePostPlot()$plotValues, activePostPlot()$plotStyle)
    values$plot <- recordPlot()
  })

  callModule(plotExport, "export", reactive(values$plot))
}
