#' @export
#' @rdname shinyModule
addMorePointsUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(title,
           id = id,
           value = id,
           fluidRow(
             sidebarPanel(
               style = "position:fixed; width:20%; max-width:350px; overflow-y:auto; height:88%",
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
               fluidRow(column(9, h4("View the Plot")),
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
                   h4("Add points"),
                   textInput(ns("pointName"), label = "Name the point", value = NULL),
                   addPointsUI(ns("morePoints")),
                   actionButton(ns("addPointsButton"), "Add data point")
                 ),
                 column(
                   3,
                   h4("Remove points"),
                   selectInput(
                     ns("pointsToRemove"),
                     label = "Select points to remove",
                     choices = c("Add a point ..." = ""),
                     multiple = TRUE
                   ),
                   actionButton(ns("removePointsButton"), "Remove data point")
                 ),
                 column(
                   3,
                   h4("Style points"),
                   selectInput(
                     ns("activePoint"),
                     label = "Style the point",
                     choices = c("Add a point ..." = ""),
                     selected = NA
                   ),
                   checkboxInput(ns("hidePointLabel"), label = "Hide point label",
                                 value = FALSE),
                   colourInput(
                     ns("pointColor"),
                     label = "Point color",
                     value = '#002350'
                   ),
                   sliderInput(
                     ns("pointSize"),
                     label = "Point size",
                     value = 1,
                     min = 0.1,
                     max = 5,
                     step = 0.1
                   ),
                   selectInput(
                     ns("pointSymbol"),
                     label = "Point symbol",
                     choices = pchChoices(),
                     selected = 19
                   ),
                   colourInput(
                     ns("textColor"),
                     label = "Text color",
                     value = '#002350'
                   ),
                   sliderInput(
                     ns("textSize"),
                     label = "Text size",
                     value = 1,
                     min = 0.1,
                     max = 5,
                     step = 0.1
                   ),
                   selectInput(
                     ns("fontType"),
                     label = "Font type",
                     choices = fontChoices(),
                     selected = 1
                   )
                 )
               )
             )
           ))
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
addMorePoints <- function(input, output, session, savedData) {
  activePlotValues <- getPlotValuesDefaults()
  activePlotStyle <- getPlotStyleDefaults()
  morePoints <- reactiveVal(list())

  oneMorePoint <- callModule(addPoints, id = "morePoints")

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

    morePoints(activePlotStyle$morePoints)

    updateSelectInput(
      session,
      "activePoint",
      choices = names(morePoints()),
      selected = names(morePoints())[length(morePoints())]
    )
  })

  # add more points ####
  observeEvent(input$addPointsButton, {
    req(input$activePlot)
    # save last changes
    morePoints(activePlotStyle$morePoints)
    # add new point
    morePoints(addPoint(morePoints(), input$pointName, oneMorePoint()))
    # save to plot object
    activePlotStyle$morePoints <- morePoints()
  })

  # remove points ####
  observeEvent(input$removePointsButton, {
    req(length(input$pointsToRemove))
    # save last changes
    morePoints(activePlotStyle$morePoints)
    # remove points
    morePoints(removePoints(morePoints(), input$pointsToRemove))
    # save to plot object
    activePlotStyle$morePoints <- morePoints()
  })

  observe({
    req(morePoints())
    updateTextInput(session, "pointName", value = paste("Point", (length(morePoints(
    )) + 1)))
    updateSelectInput(
      session,
      "pointsToRemove",
      choices = names(morePoints()),
      selected = NA
    )
    updateSelectInput(
      session,
      "activePoint",
      choices = names(morePoints()),
      selected = names(morePoints())[length(morePoints())]
    )
  })

  # style more points ####
  observeEvent(input$activePoint, {
    req(input$activePoint)
    for (element in c(
      "pointColor",
      "pointSize",
      "pointSymbol",
      "hidePointLabel",
      "textColor",
      "textSize",
      "fontType"
    )) {
      updateColourInput(session, element,
                        value = activePlotStyle$morePoints[[input$activePoint]][[element]])
    }
  })

  observe({
    req(input$activePoint)
    # more points
    activePlotStyle$morePoints[[input$activePoint]]$pointColor <-
      input$pointColor
    activePlotStyle$morePoints[[input$activePoint]]$pointSize <-
      input$pointSize
    activePlotStyle$morePoints[[input$activePoint]]$pointSymbol <-
      input$pointSymbol
    activePlotStyle$morePoints[[input$activePoint]]$hidePointLabel <-
      input$hidePointLabel
    activePlotStyle$morePoints[[input$activePoint]]$textColor <-
      input$textColor
    activePlotStyle$morePoints[[input$activePoint]]$textSize <-
      input$textSize
    activePlotStyle$morePoints[[input$activePoint]]$fontType <-
      as.numeric(input$fontType)
  })

  # render plot ####
  values <- reactiveValues(plot = NULL)
  output$styledPlot <- renderPlot({
    req(input$activePlot)
    req(activePlotValues$predictedData$evenlyOnX)
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

  dataFun <- reactive({
    req(input$activePlot)
    req(activePlotValues$modelData)
    function(xVar, quantile) {
      savedData <-
        predictPipe(
          plotRModel = activePlotValues$modelData$modelOutput,
          xCol = activePlotValues$prepData$X,
          xVar = xVar,
          yName = activePlotValues$ySelection$colNames$colName1,
          quantile = quantile
        )
      return(savedData)
    }
  })

  #callModule(dataExport, "exportData", dat = dataFun, filename = "modelData")
  callModule(plotExport, "export", reactive(values$plot))
}

missingPointValues <- function(point) {
  missingValuesAt <- function(label) {
    (is.na(point[[paste0(label, "min")]]) ||
       is.na(point[[paste0(label, "max")]])) && is.na(point[[label]])
  }

  missingValuesAt("x") | missingValuesAt("y")
}

addPoint <- function(pointsList, newPointName, newPoint) {
  if (newPointName == "") {
    shinyalert("No name!",
               paste("Please enter a name to add this point."),
               type = "error")
  } else if (newPointName %in% names(pointsList)) {
    shinyalert(
      "Name exists!",
      paste("This name already exists. Please provide a unique name."),
      type = "error"
    )
  } else if (missingPointValues(newPoint)) {
    shinyalert("Missing values!",
               paste("Please provide values for x and y to add a point."),
               type = "error")
  } else {
    pointsList <- c(pointsList,
                    setNames(list(newPoint),
                             newPointName))
  }

  return(pointsList)
}

removePoints <- function(pointsList, pointsToRemove) {
  for (i in pointsToRemove) {
    pointsList <- pointsList[!grepl(i, names(pointsList))]
  }

  return(pointsList)
}
