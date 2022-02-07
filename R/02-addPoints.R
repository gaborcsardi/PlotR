#' Add points module
#'
#' UI function to define point values for a point to be added to the plot
#'
#' @param id id of module
#'
#' @rdname addPoints
addPointsUI <- function(id) {
  ns <- NS(id)

  div(
    numericInput(
      ns("x"),
      paste("x"),
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns("xmin"),
      paste("xmin"),
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns("xmax"),
      paste("xmax"),
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns("y"),
      paste("y"),
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns("ymin"),
      paste("ymin"),
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    ),
    numericInput(
      ns("ymax"),
      paste("ymax"),
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
  )
}

#' Server function for add points
#'
#' Backend for add points module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
addPoints <- function(input, output, session) {
  reactive(c(
    list(y = input$y,
         ymin = input$ymin,
         ymax = input$ymax,
         x = input$x,
         xmin = input$xmin,
         xmax = input$xmax),
    defaultPointStyle())
  )
}

defaultPointStyle <- function() {
  list(hidePointLabel = FALSE,
       pointColor = '#002350',
       pointSize = 1,
       pointSymbol = 19,
       textColor = '#002350',
       textSize = 1,
       fontType = 1)
}
