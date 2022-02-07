#' @export
#' @rdname shinyModule
multiplePlotsUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    #useShinyjs(),
    useShinyalert(),
    fluidRow(
      sidebarPanel(width = 2,
                   selectInput(ns("activePlots"),
                               label = "Select saved plots",
                               choices = NULL,
                               multiple = TRUE#,
                               #selectize = TRUE
                   ),
                   tags$hr(),
                   sliderInput(ns("previewWidth"),
                               label = "Width of preview",
                               min = 400, max = 2000, value = 1400, step = 10),
                   sliderInput(ns("previewHeight"),
                                label = "Height of preview",
                                min = 400, max = 2000, value = 800, step = 10),
                   tags$br(),
                   h4("Combine plots"),
                   selectInput(ns("combiType"),
                               label = NULL,
                               choices = c("all-in-one" = "joinedPlot",
                                           "grid" = "fullGrid"),
                               selected = "fullGrid"),
                   conditionalPanel(
                     condition = "input.combiType == 'fullGrid'",
                     ns = ns,
                     numericInput(inputId = ns("nGridCols"),
                                 label = "Number of columns",
                                 min = 1, max = 8, value = 1, step = 1)),
                   tags$hr(),
                   conditionalPanel(
                     condition = "input.combiType == 'joinedPlot'",
                     ns = ns,
                     h4("Adjust margins"),
                     selectMarginUI(ns("margins"))
                   ),
                   tags$hr(),
                   h4("Hide axes"),
                   selectInput(ns("xAxisToHide"),
                               label = "Hide x axis of selected plot",
                               choices = NULL,
                               multiple = TRUE),
                   selectInput(ns("yAxisToHide"),
                               label = "Hide y axis of selected plot",
                               choices = NULL,
                               multiple = TRUE)
      ),
      mainPanel(width = 8,
                h4("View Multiple Plots"),
                p(strong("Notes:"), "Adjust the format of a single plot in the tab Style Plot.",
                  "Increase the width/height if figure margins become to large when selecting many plots."),
                conditionalPanel(
                  condition = "input.combiType == 'joinedPlot'",
                  ns = ns,
                  p("The background color of an", em("all-in-one"),
                    "plot is determined by the first selected single plot. ",
                    "The axes of the first selected plot will be furthest inside.")),
                plotOutput(ns("multiPlot"), height = "800px", width = "100%", inline = TRUE)
      ),
      sidebarPanel(width = 2,
                   div(plotExportButton(ns("export")))
      )
    )
  )
}

selectMarginUI <- function(id) {

  ns <- NS(id)
  div(
    selectInput(ns("side"),
                label = "Select plot side",
                choices = c("bottom" = "1", "left" = "2", "top" = "3", "right" = "4"),
                selected = "1"),
    conditionalPanel(
      condition = "input.side == '1'",
      ns = ns,
      numericInput(ns("nMarginLineBottom"),
                  label = "Number of margin lines",
                  min = 0, max = 1, value = 1, step = 1),
    ),
    conditionalPanel(
      condition = "input.side == '2'",
      ns = ns,
      numericInput(ns("nMarginLineLeft"),
                   label = "Number of margin lines",
                   min = 0, max = 1, value = 1, step = 1),
    ),
    conditionalPanel(
      condition = "input.side == '3'",
      ns = ns,
      numericInput(ns("nMarginLineTop"),
                   label = "Number of margin lines",
                   min = 0, max = 1, value = 1, step = 1),
    ),
    conditionalPanel(
      condition = "input.side == '4'",
      ns = ns,
      numericInput(ns("nMarginLineRight"),
                   label = "Number of margin lines",
                   min = 0, max = 1, value = 1, step = 1),
    )
  )
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
multiplePlots <- function(input, output, session, savedData) {

  activePlotsNames <- reactiveVal(NULL)
  activePlotsData <- reactiveVal(list())
  values <- reactiveValues(plot = NULL)
  nActivePlots <- reactiveVal(NULL)

  nMarginLines <- callModule(selectMargin, id = "margins", nData = nActivePlots)

  observe({
    req(names(savedData()))
    updateSelectInput(session, "activePlots", choices = names(savedData()),
                      selected = activePlotsNames())
    #shinyjs::disable("exportCombined")
  })

  observeEvent(input$activePlots, {
    req(input$activePlots)

    updateSelectInput(session, "xAxisToHide", choices = input$activePlots#,
                      # only use if nMarginLines AND position of an axis will be reactive
                      #selected = getNamesToDrop(input$activePlots, input$combiType)
    )
    updateSelectInput(session, "yAxisToHide", choices = input$activePlots#,
                      #selected = getNamesToDrop(input$activePlots, input$combiType)
    )

    activePlotsNames(input$activePlots)
    nActivePlots(length(input$activePlots))
    activePlotsData(savedData()[input$activePlots])
  })

  output$multiPlot <- renderPlot({
    req(names(activePlotsData()))

    tryCatch(
      withCallingHandlers(
        makeMultiPlot(activePlotsData(),
                      nMarginLines = nMarginLines(),
                      combiType = input$combiType,
                      nGridCols = input$nGridCols,
                      xAxisToHide = input$xAxisToHide,
                      yAxisToHide = input$yAxisToHide
        ),
        message = function(m) showNotification(m$message, type = "message"),
        warning = function(w) showNotification(w$message, type = "warning")
      ),
      error = function(e) shinyalert("Error!", e$message, type = "error")
    )

    values$plot <- recordPlot()
  },
  width = reactive(input$previewWidth),
  height = reactive(input$previewHeight)
  )

  callModule(plotExport, "export", reactive(values$plot),
             plotWidth = reactive(input$previewWidth), plotHeight = reactive(input$previewHeight))
}

selectMargin <- function(input, output, session, nData) {
  observe({
    req(nData())
    updateNumericInput(session, "nMarginLineBottom", max = nData(), value = nData())
    updateNumericInput(session, "nMarginLineLeft", max = nData(), value = nData())
    updateNumericInput(session, "nMarginLineTop", max = nData(), value = nData())
    updateNumericInput(session, "nMarginLineRight", max = nData(), value = nData())
  })

  reactive(list(bottom = input$nMarginLineBottom,
                left = input$nMarginLineLeft,
                top = input$nMarginLineTop,
                right = input$nMarginLineRight
  ))
}

getNamesToDrop <- function(names, combiType, from = 4){
  if (length(names) < from | combiType == "fullGrid") return(NULL)
  names[from:length(names)]
}
