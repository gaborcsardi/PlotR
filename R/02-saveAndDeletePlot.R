# save plot ####

#' @rdname shinyModule
savePlotUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    fluidRow(column(8,
                    textInput(ns("plotName"), label, value = "", placeholder = "name your plot")
                    ),
             column(4,
                    align = "left",
                    style = "margin-top: 26px;",
                    actionButton(ns("saveActivePlot"), "Save", width = "80%")
                    ))
  )
}

#' @rdname shinyModule
#' @param currentPlot plot object to be saved
savePlot <- function(input, output, session, savedData, currentPlot){

  observe({
    req(length(savedData()))
    updateSelectInput(session, "activePlot", choices = names(savedData()),
                      selected = names(savedData())[length(savedData())])
  })

  observeEvent(input$saveActivePlot, {
    req(currentPlot$plotValues$defaultXRange)

    if (input$plotName == "") {
      shinyalert("Oops!", paste("Please enter a name to save this plot."),
                 type = "error")
    } else if (input$plotName %in% names(savedData())) {
      shinyalert("Oops!", paste("This name already exists. Please provide a unique name."),
                 type = "error")
      # add instead pop up option to overwrite existing plot, smth. like: Name already exist.
      # File will be overwritten. Accept/Cancel
    } else {
      savedData(c(savedData(),
                  setNames(list(list(plotName = input$plotName,
                                     plotValues = reactiveValuesToList(currentPlot$plotValues),
                                     plotStyle = reactiveValuesToList(currentPlot$plotStyle))),
                           input$plotName)))

      updateTextInput(session, "plotName", value = "")
    }
  })
}


# load plot ####

# complicated since all plotValues and plotStyles must be updated ....


# delete plot ####

#' @rdname shinyModule
deletePlotUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    fluidRow(column(8,
                    style = "margin-top: 10px;",
                    selectInput(ns("deletePlotSelection"),
                                label = label,
                                choices = c("Save or upload a plot ..." = ""),
                                multiple = T)
                    ),
             column(4,
                    align = "left",
                    style = "margin-top: 26px;",
                    actionButton(ns("deletePlot"), "Delete", width = "80%")
                    ))
  )
}

#' @rdname shinyModule
deletePlot <- function(input, output, session, savedData){
  # disable the button on page load
  shinyjs::disable("deletePlot")

  observe({
    req(length(savedData()))
    updateSelectInput(session, "deletePlotSelection", choices = names(savedData()), selected = NULL)

    if (length(savedData())) {
      # enable the button
      shinyjs::enable("deletePlot")
      return(NULL)
    }

  })

  observeEvent(input$deletePlot, {
    req(input$deletePlotSelection)

    lapply(input$deletePlotSelection, function(plot){
      savedData(savedData()[!grepl(plot, names(savedData()))])
    })

    savedData()
  })
}
