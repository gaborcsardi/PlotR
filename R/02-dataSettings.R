#' Data settings module
#'
#' UI function to define settings for imported data
#'
#' @param id id of module
#' @param label label of settings
#'
#' @rdname dataSettings
dataSettingsUI <- function(id, label = "Data Settings") {

  ns <- NS(id)

  list(
    tags$h4(label),
    selectColumnsUI(id = ns("x"), label = "x"),
    selectColumnsUI(id = ns("y"), label = "y"),
    checkboxInput(inputId = ns("outlierD"),
                  label = "Remove data outliers in y",
                  value = FALSE, width = "100%"),
    conditionalPanel(
      condition = "input.outlierD == true",
      ns = ns,
      sliderInput(inputId = ns("outlierValueD"),
                  label = "Data outlier threshold in standard deviations",
                  min = 2, max = 8, value = 4, step = 0.1))
  )
}


#' Server function for data settings
#'
#' Backend for data settings module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param data (reactive) dataframe of the selected loaded file
#' @param plotData (reactive) list of selected saved data
dataSettings <- function(input, output, session, data, plotData) {

  dataSettingsX <- reactiveVal(NULL)
  dataSettingsY <- reactiveVal(NULL)

  observe({
    req(plotData())
    dataSettingsX(plotData()$plotValues$dataSettings$xColumns)
    dataSettingsY(plotData()$plotValues$dataSettings$yColumns)
  })

  xColumns <- callModule(
    selectColumns, id = "x", data = data, datSettings = dataSettingsX
    )
  yColumns <- callModule(
    selectColumns, id = "y", data = data, datSettings = dataSettingsY
    )

  observe({
    req(plotData())
    updateCheckboxInput(
      session, "outlierD",
      value = plotData()$plotValues$dataSettings$dataOutlier$outlierD
      )
    updateSliderInput(
      session, "outlierValueD",
      value = plotData()$plotValues$dataSettings$dataOutlier$outlierValueD
      )
  })

  list(
    xColumns = xColumns,
    yColumns = yColumns,
    dataOutlier = reactive(list(outlierD = input$outlierD,
                                outlierValueD = input$outlierValueD
    ))
  )
}


selectColumnsUI <- function(id, label) {

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
      selectInput(ns("Point"),
                  label = paste("Select", label),
                  choices = NULL),
    ),
    conditionalPanel(
      condition = "input.type == 'interval'",
      ns = ns,
      selectInput(ns("Min"),
                  label = paste("Lower bound for", label),
                  choices = NULL),
      selectInput(ns("Max"),
                  label = paste("Upper bound for", label),
                  choices = NULL)
    ),
    conditionalPanel(
      condition = "input.type == 'credInterval'",
      ns = ns,
      selectInput(ns("CredMin"),
                  label = paste("Lower bound for", label),
                  choices = NULL),
      selectInput(ns("CredMax"),
                  label = paste("Upper bound for", label),
                  choices = NULL),
      sliderInput(ns("CredPercent"),
                  label = "Credibility level",
                  min = 0, max = 100, post  = " %", value = 95)
    ),
    conditionalPanel(
      condition = "input.type == 'meanSD'",
      ns = ns,
      selectInput(ns("Mean"),
                  label = paste("Mean of", label),
                  choices = NULL),
      selectInput(ns("SD"),
                  label = paste("SD of", label),
                  choices = NULL)
    ),
    conditionalPanel(
      condition = "input.type == 'meanSEMSD'",
      ns = ns,
      selectInput(ns("Mean2"),
                  label = paste("Mean of", label),
                  choices = NULL),
      selectInput(ns("SEMSD"),
                  label = paste("SEM SD of", label),
                  choices = NULL)
    )
  )
}



selectColumns <- function(input, output, session, data, datSettings) {

  observe({
    req(data())
    updateSelectInput(session, "Point", choices = colnames(data()),
                      selected = colnames(data())[1])
    updateSelectInput(session, "Min", choices = colnames(data()),
                      selected = colnames(data())[1])
    updateSelectInput(session, "Max", choices = colnames(data()),
                      selected = colnames(data())[2])
    updateSelectInput(session, "CredMin", choices = colnames(data()),
                      selected = colnames(data())[1])
    updateSelectInput(session, "CredMax", choices = colnames(data()),
                      selected = colnames(data())[2])
    updateSelectInput(session, "Mean", choices = colnames(data()),
                      selected = colnames(data())[1])
    updateSelectInput(session, "SD", choices = colnames(data()),
                      selected = colnames(data())[2])
    updateSelectInput(session, "Mean2", choices = colnames(data()),
                      selected = colnames(data())[1])
    updateSelectInput(session, "SEMSD", choices = colnames(data()),
                      selected = colnames(data())[2])
  })

  observeEvent(datSettings(), {
    req(data(), datSettings())
    updateSelectInput(session, "type", selected = datSettings()$type)
    updateSelectInput(session, "Point", choices = colnames(data()),
                      selected = datSettings()$Point)
    updateSelectInput(session, "Min", choices = colnames(data()),
                      selected = datSettings()$Min)
    updateSelectInput(session, "Max", choices = colnames(data()),
                      selected = datSettings()$Max)
    updateSelectInput(session, "CredMin", choices = colnames(data()),
                      selected = datSettings()$CredMin)
    updateSelectInput(session, "CredMax", choices = colnames(data()),
                      selected = datSettings()$CredMax)
    updateSelectInput(session, "Mean", choices = colnames(data()),
                      selected = datSettings()$Mean)
    updateSelectInput(session, "SD", choices = colnames(data()),
                      selected = datSettings()$SD)
    updateSelectInput(session, "Mean2", choices = colnames(data()),
                      selected = datSettings()$Mean2)
    updateSelectInput(session, "SEMSD", choices = colnames(data()),
                      selected = datSettings()$SEMSD)
  })

  reactive(list(type = input$type,
                Point = input$Point,
                Min = input$Min,
                Max = input$Max,
                CredMin = input$CredMin,
                CredMax = input$CredMax,
                CredPercent = input$CredPercent,
                Mean = input$Mean,
                SD = input$SD,
                Mean2 = input$Mean2,
                SEMSD = input$SEMSD
  ))
}

getDefault <- function(xType, n) {
  ifelse(xType == "point", n + 1, n + 2)
}

#' Model settings module
#'
#' UI function to define settings for the model
#'
#' @param id id of module
#' @param label label of settings
#'
#' @rdname dataSettings
modelSettingsUI <- function(id, label = "Model Settings") {

  ns <- NS(id)

  list(
    tags$h4(label),
    checkboxInput(inputId = ns("outlier"),
                  label = "Remove model outliers",
                  value = FALSE, width = "100%"),
    conditionalPanel(
      condition = "input.outlier == true",
      sliderInput(inputId = ns("outlierValue"),
                  label = "Model outlier threshold in standard deviations",
                  min = 2, max = 8, value = 4, step = 0.1),
      ns = ns
    ),
    radioButtons(inputId = ns("const"), label = "Extrapolation",
                 choices = c("linear" = 2, "constant" = 1)),
    checkboxInput(inputId = ns("sdVar"), label = "Variable standard deviation",
                  value = FALSE),
    sliderInput(inputId = ns("smoothConst"), label = "Amount of smoothing",
                value = 1, min = 0.2, max = 5),
    checkboxInput(inputId = ns("advancedSettings"),
                  label = "Use advanced settings",
                  value = FALSE, width = "100%"),
    conditionalPanel(
      condition = "input.advancedSettings == true",
      ns = ns,
      sliderInput(inputId = ns("K"), label = "Number of basis functions",
                  value = 25, min = 4, max = 150),
      sliderInput(inputId = ns("burnin"), label = "Number of burnin iterations",
                  value = 1000, min = 100, max = 10000, step = 100),
      sliderInput(inputId = ns("iter"), label = "Number of total iterations",
                  value = 5000, min = 1000, max = 250000, step = 1000),
      sliderInput(inputId = ns("nChains"), label = "Number of MCMC chains",
                  value = 4, min = 1, max = 16, step = 1)
    )
  )
}

#' Server function for model settings
#'
#' Backend for model settings module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param data (reactive) dataframe of the selected loaded file
#' @param plotData (reactive) list of selected saved data
modelSettings <- function(input, output, session, data, plotData) {

  modelParameters <- reactiveVal(NULL)

  observe({
    req(plotData())
    modelParameters(plotData()$plotValues$modelParameters)
  })

  observe({
    req(modelParameters())
    updateCheckboxInput(session, "outlier", value = modelParameters()$outlier)
    updateSliderInput(session, "outlierValue", value = modelParameters()$outlierValue)
    updateRadioButtons(session, "const", selected = modelParameters()$const)
    updateCheckboxInput(session, "sdVar", value = modelParameters()$sdVar)
    updateSliderInput(session, "smoothConst", value = modelParameters()$smoothConst)
    updateSliderInput(session, "K", value = modelParameters()$K)
    updateSliderInput(session, "burnin", value = modelParameters()$burnin)
    updateSliderInput(session, "iter", value = modelParameters()$iter)
    updateSliderInput(session, "nChains", value = modelParameters()$nChains)
  })

  reactive(list(outlier = input$outlier,
                outlierValue = input$outlierValue,
                burnin = input$burnin,
                iter = input$iter,
                nChains = input$nChains,
                smoothConst = input$smoothConst,
                K = input$K,
                sdVar = input$sdVar,
                const = as.numeric(input$const)
  ))
}
