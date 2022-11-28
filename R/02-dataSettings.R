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
                  value = defaultDataOutlier()$outlierD, width = "100%"),
    conditionalPanel(
      condition = "input.outlierD == true",
      ns = ns,
      sliderInput(inputId = ns("outlierValueD"),
                  label = "Data outlier threshold in standard deviations",
                  min = 2, max = 8, value = defaultDataOutlier()$outlierValueD, step = 0.1))
  )
}

#' Server function for data settings
#'
#' Backend for data settings module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param colNames (reactive) colnames of dataframe of the selected loaded file
#' @param inputSettings (list) list of reactive settings for input fields
dataSettings <- function(input, output, session, colNames, inputSettings) {

  xColumns <- callModule(
    selectColumns,
    id = "x",
    colNames = colNames,
    datSettings = inputSettings$xColumns
    )
  yColumns <- callModule(
    selectColumns,
    id = "y",
    colNames = colNames,
    datSettings = inputSettings$yColumns
    )

  observe({
    updateCheckboxInput(
      session, "outlierD",
      value = inputSettings$dataOutlier()$outlierD
      )
    updateSliderInput(
      session, "outlierValueD",
      value = inputSettings$dataOutlier()$outlierValueD
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


defaultDataOutlier <- function() {
  list(
    outlierD = FALSE,
    outlierValueD = 4
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


selectColumns <- function(input, output, session, colNames, datSettings) {
  observeEvent(datSettings(), {
    updateSelectInput(session, "type", selected = datSettings()$type)
    updateSelectInput(session, "Point", choices = colNames(),
                      selected = datSettings()$Point)
    updateSelectInput(session, "Min", choices = colNames(),
                      selected = datSettings()$Min)
    updateSelectInput(session, "Max", choices = colNames(),
                      selected = datSettings()$Max)
    updateSelectInput(session, "CredMin", choices = colNames(),
                      selected = datSettings()$CredMin)
    updateSelectInput(session, "CredMax", choices = colNames(),
                      selected = datSettings()$CredMax)
    updateSelectInput(session, "Mean", choices = colNames(),
                      selected = datSettings()$Mean)
    updateSelectInput(session, "SD", choices = colNames(),
                      selected = datSettings()$SD)
    updateSelectInput(session, "Mean2", choices = colNames(),
                      selected = datSettings()$Mean2)
    updateSelectInput(session, "SEMSD", choices = colNames(),
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


defaultColSelection <- function(columnNames = NULL) {
  if (is.null(columnNames)) {
    colChoices <- character(0)
  } else {
    colChoices <- columnNames
  }

  list(type = "point",
       Point = colChoices[1],
       Min = colChoices[1],
       Max = colChoices[2],
       CredMin = colChoices[1],
       CredMax = colChoices[2],
       CredPercent = 95,
       Mean = colChoices[1],
       SD = colChoices[2],
       Mean2 = colChoices[1],
       SEMSD = colChoices[2]
  )
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
    checkboxInput(
      inputId = ns("outlier"),
      label = "Remove model outliers",
      value = defaultModelSettings()$outlier,
      width = "100%"
    ),
    conditionalPanel(
      condition = "input.outlier == true",
      sliderInput(
        inputId = ns("outlierValue"),
        label = "Model outlier threshold in standard deviations",
        min = 2,
        max = 8,
        value = defaultModelSettings()$outlierValue,
        step = 0.1
      ),
      ns = ns
    ),
    radioButtons(
      inputId = ns("const"),
      label = "Extrapolation",
      selected = defaultModelSettings()$const,
      choices = c("linear" = 2, "constant" = 1)
    ),
    checkboxInput(
      inputId = ns("sdVar"),
      label = "Variable standard deviation",
      value = defaultModelSettings()$sdVar
    ),
    sliderInput(
      inputId = ns("smoothConst"),
      label = "Amount of smoothing",
      value = defaultModelSettings()$smoothConst,
      min = 0.2,
      max = 5
    ),
    checkboxInput(
      inputId = ns("advancedSettings"),
      label = "Use advanced settings",
      value = defaultModelSettings()$advancedSettings,
      width = "100%"
    ),
    conditionalPanel(
      condition = "input.advancedSettings == true",
      ns = ns,
      sliderInput(
        inputId = ns("K"),
        label = "Number of basis functions",
        value = defaultModelSettings()$K,
        min = 4,
        max = 150
      ),
      sliderInput(
        inputId = ns("burnin"),
        label = "Number of burnin iterations",
        value = defaultModelSettings()$burnin,
        min = 100,
        max = 10000,
        step = 100
      ),
      sliderInput(
        inputId = ns("iter"),
        label = "Number of total iterations",
        value = defaultModelSettings()$iter,
        min = 1000,
        max = 250000,
        step = 1000
      ),
      sliderInput(
        inputId = ns("nChains"),
        label = "Number of MCMC chains",
        value = defaultModelSettings()$nChains,
        min = 1,
        max = 16,
        step = 1
      )
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
#' @param modelParameters (reactive) list of settings for input fields
modelSettings <-
  function(input,
           output,
           session,
           modelParameters) {
    observe({
      req(modelParameters())
      updateCheckboxInput(session, "outlier", value = modelParameters()$outlier)
      updateSliderInput(session, "outlierValue", value = modelParameters()$outlierValue)
      updateRadioButtons(session, "const", selected = modelParameters()$const)
      updateCheckboxInput(session, "sdVar", value = modelParameters()$sdVar)
      updateSliderInput(session, "smoothConst", value = modelParameters()$smoothConst)
      updateCheckboxInput(session, "advancedSettings", value = modelParameters()$advancedSettings)
      updateSliderInput(session, "K", value = modelParameters()$K)
      updateSliderInput(session, "burnin", value = modelParameters()$burnin)
      updateSliderInput(session, "iter", value = modelParameters()$iter)
      updateSliderInput(session, "nChains", value = modelParameters()$nChains)
    })

    reactive(
      list(
        outlier = input$outlier,
        outlierValue = input$outlierValue,
        smoothConst = input$smoothConst,
        sdVar = input$sdVar,
        const = as.numeric(input$const),
        advancedSettings = input$advancedSettings,
        K = input$K,
        burnin = input$burnin,
        iter = input$iter,
        nChains = input$nChains
      )
    )
  }

defaultModelSettings <- function() {
  list(
    outlier = FALSE,
    outlierValue = 4,
    smoothConst = 1,
    sdVar = FALSE,
    const = 2,
    advancedSettings = FALSE,
    K = 25,
    burnin = 1000,
    iter = 5000,
    nChains = 4
  )
}
