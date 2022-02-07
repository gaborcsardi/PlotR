#' @export
#' @rdname shinyModule
#' @param label (character) label used in UI
addMoreDataUI <- function(id, label) {

  ns <- NS(id)
  div(
    selectInput(ns("type"),
                label = paste("Type of", label),
                choices = c("Single point" = "point",
                            "Interval" = "interval",
                            "Mean + 1 SD uncertainty" = "meanSD",
                            "Mean + 1 SEM SD uncertainty" = "meanSEMSD",
                            "Credible Interval" = "credInterval"),
                selected = "point"),
    conditionalPanel(
      condition = "input.type == 'point'",
      ns = ns,
      numericInput(ns("Point"),
                   label = paste("Value for", label),
                   value = NULL),
    ),
    conditionalPanel(
      condition = "input.type == 'interval'",
      ns = ns,
      numericInput(ns("Min"),
                  label = paste("Lower bound for", label),
                  value = NULL),
      numericInput(ns("Max"),
                  label = paste("Upper bound for", label),
                  value = NULL)
    ),
    conditionalPanel(
      condition = "input.type == 'credInterval'",
      ns = ns,
      numericInput(ns("CredMin"),
                  label = paste("Lower bound for", label),
                  value = NULL),
      numericInput(ns("CredMax"),
                  label = paste("Upper bound for", label),
                  value = NULL),
      sliderInput(ns("CredPercent"),
                  label = "Credibility level",
                  min = 0, max = 100, post  = " %", value = 95)
    ),
    conditionalPanel(
      condition = "input.type == 'meanSD'",
      ns = ns,
      numericInput(ns("Mean"),
                  label = paste("Mean of", label),
                  value = NULL),
      numericInput(ns("SD"),
                  label = paste("SD of", label),
                  value = NULL)
    ),
    conditionalPanel(
      condition = "input.type == 'meanSEMSD'",
      ns = ns,
      numericInput(ns("Mean2"),
                  label = paste("Mean of", label),
                  value = NULL),
      numericInput(ns("SEMSD"),
                  label = paste("SEM SD of", label),
                  value = NULL)
    ),
    conditionalPanel(
      condition = "input.type != 'point'",
      ns = ns,
      numericInput(ns("nSamples"),
                   label = paste("Choose number of samples for", label),
                   value = 100, min = 1)
    )
  )
}

#' @export
#' @rdname shinyModule
addMoreData <- function(input, output, session) {

  values <- reactiveValues(
    moreDat = NULL,
    moreMean = NULL,
    moreSD = NULL
  )

  observe({
    values$moreDat <- getSelection(list(type = input$type,
                                        Point = input$Point,
                                        Min = input$Min,
                                        Max = input$Max,
                                        CredMin = input$CredMin,
                                        CredMax = input$CredMax,
                                        CredPercent = input$CredPercent,
                                        Mean = input$Mean,
                                        SD = input$SD,
                                        Mean2 = input$Mean2,
                                        SEMSD = input$SEMSD))

  })

  observeEvent(values$moreDat, {

    values$moreMean <- NULL
    values$moreSD <- NULL

    req(values$moreDat$colNames$colName1)
    if (input$type != "point") req(values$moreDat$colNames$colName2)

    moreDf <- data.frame(X1 = values$moreDat$colNames$colName1)
    moreDf$X2 <- values$moreDat$colNames$colName2

    values$moreMean <- getMean(moreDf, dataType = input$type)
    values$moreSD <- getUncertainty(moreDf, dataType = input$type)

  })

  reactive(list(mean = values$moreMean,
                sd = values$moreSD,
                nSamples = input$nSamples
  ))
}
