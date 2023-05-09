#' @import shiny
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom DataTools checkErrorNoNumericColumns checkWarningEmptyValues importDataUI
#'  importDataServer remoteModelsUI remoteModelsServer tryCatchWithWarningsAndErrors
#' @importFrom dplyr bind_cols bind_rows filter mutate
#' @importFrom DT DTOutput renderDT
#' @importFrom graphics arrows axis box lines mtext par plot points rect text legend
#' @importFrom grDevices dev.off pdf png recordPlot replayPlot rgb svg tiff
#' @importFrom jsonlite toJSON
#' @importFrom magrittr %>%
#' @importFrom mgcv Predict.matrix smoothCon s
#' @importFrom Rfast rmvnorm spdinv
#' @importFrom rlang .data
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs useShinyjs
#' @importFrom stats cor density dunif na.omit pnorm rnorm rgamma runif sd setNames var median qnorm
#' @importFrom tibble tribble
#' @importFrom utils head packageVersion write.table
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom zip zipr
NULL

#' Server and UI Functions for Shiny Module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param id namespace id
#' @param title title of tab in tabset panel
#'
#' @name shinyModule
NULL
