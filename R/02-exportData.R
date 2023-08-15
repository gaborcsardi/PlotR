dataExportButton <- function(id, title = "Export Data") {
  ns <- NS(id)
  actionButton(ns("export"), title)
}

dataExport <- function(input, output, session, dat, filename = "data", useXOptions = TRUE) {
  observeEvent(input$export, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      c(list(
        selectInput(
          session$ns("exportType"),
          "File type",
          choices = c("csv", "xlsx", "json"),
          selected = "xlsx"
        ),
        conditionalPanel(
          condition = "input['exportType'] == 'csv'",
          ns = session$ns,
          div(style = "display: inline-block;horizontal-align:top; width: 80px;",
              textInput(session$ns("colseparator"), "column separator:", value = ",")),
          div(style = "display: inline-block;horizontal-align:top; width: 80px;",
              textInput(session$ns("decseparator"), "decimal separator:", value = "."))
        )
      ),
      if(useXOptions) list(
        numericInput(session$ns("xminE"), label = "Min x-value", value = 0),
        numericInput(session$ns("xmaxE"), label = "Max x-value", value = 1000),
        numericInput(session$ns("stepE"), label = "Increments x-value", value = 10),
        sliderInput(session$ns("quantileE"), "Credible Interval Coverage", value = 0.95,
                    min = 0.5, max = 0.999, step = 0.001)
        ) else list(),
      list(downloadButton(session$ns("exportExecute"), "Export"))
      )
    ))
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      exportFilename(filename, input$exportType)
    },
    content = function(file){
      if(useXOptions) {
        datDF <- dat()(xVar = seq(input$xminE, input$xmaxE, by = input$stepE),
                       quantile = input$quantileE)
      } else {
        datDF <- dat()()
      }

      switch(
        input$exportType,
        csv = exportCSV(file, datDF, input$colseparator,
                        input$decseparator),
        xlsx = exportXLSX(file, datDF),
        json = exportJSON(file, datDF)
      )
    }
  )
}

#' Filename of Export
#'
#' @param fileending character csv or xlsx
#' @param filename name of file
#'
#' @export
exportFilename <- function(filename = "isotopeData", fileending){
  paste(filename, fileending, sep = ".")
}

#' Export to csv
#'
#' @param file filename
#' @param dat data.frame
#' @param colseparator column seperator
#' @param decseparator decimal seperator
#' @export
exportCSV <- function(file, dat, colseparator, decseparator){
  write.table(x = dat, file = file, sep = colseparator,
              dec = decseparator, row.names = FALSE)
}

#' Export to xlsx
#'
#' @param file filename
#' @param dat data.frame
#' @export
exportXLSX <- function(file, dat){
  write.xlsx(dat, file)
}

#' Export to json
#'
#' @param file filename
#' @param dat data.frame
exportJSON <- function(file, dat){
  json <- toJSON(dat)
  write(json, file)
}

calcExportData <- function(xVar, quantile, data, xSelection, ySelection, modelOutput,
                           renameQColumns = TRUE) {
  prepData <- getPrepData(
    data = data,
    xSelection = xSelection,
    ySelection = ySelection
  )

  data <- predictPipe(
    plotRModel = modelOutput,
    xCol = prepData$X,
    xVar = xVar,
    yName = ySelection$colNames$colName1,
    quantile = quantile
  ) %>%
    tryCatchWithWarningsAndErrors(errorTitle = "Prediction failed", alertStyle = "shinyalert")

  if (renameQColumns) {
    # rename quantile columns
    qPerc <- round(quantile * 100)

    oldNames <- names(data)
    newNames <- oldNames %>%
      gsub(pattern = "Est_MEAN_Q_",
           replacement = sprintf("Est_MEAN_%s_", qPerc)) %>%
      gsub(pattern = "Est_Total_Q_",
           replacement = sprintf("Est_Total_%s_", qPerc))
    names(data) <- newNames
  }

  return(data)
}
