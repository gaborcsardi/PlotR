#' Get Text for Help Panel in Shiny App
#'
#' @param id id of selected tab
#'
#' @export
getHelp <- function(id) {
  tagList(
    tags$b("Welcome to the PlotR App!"),
    tags$p("This is the help")
  )
}
