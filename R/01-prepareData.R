#' Select Data Wrapper
#'
#' @param plotValues (list) list with all values required for the plot
#' @param activeFile (character) name of the selected file
#' @param activeFileData (data.frame) content of the selected file
#' @param dataSelection (list) list of reactive selected columns of the current file
selectDataWrapper <- function(plotValues, activeFile, activeFileData, dataSelection){
  plotValues$defaultXRange <- NULL
  plotValues$modelData <- NULL
  plotValues$predictedData <- NULL

  plotValues$activeFile <- activeFile
  plotValues$activeFileData <- activeFileData
  plotValues$dataSettings <- list(xColumns = dataSelection$xColumns(),
                                  yColumns = dataSelection$yColumns(),
                                  dataOutlier = dataSelection$dataOutlier())

  xSelection <- getSelection(plotValues$dataSettings$xColumns)
  ySelection <- getSelection(plotValues$dataSettings$yColumns)

  plotValues$selectedData <-
    activeFileData[, unlist(c(xSelection$colNames, ySelection$colNames))] %>%
    asNumericWithoutNA()

  if (is.null(plotValues$selectedData)) return(plotValues)

  plotValues$selectedData <- addColumnDataOutlier(
    selectedData = plotValues$selectedData,
    ySelection = ySelection,
    dataOutlier = plotValues$dataSettings$dataOutlier
  )

  plotValues$defaultXRange <- getRange(
    data = plotValues$selectedData[, unlist(xSelection$colNames),
                                   drop = FALSE],
    type = xSelection$type,
    credPercent = xSelection$credPercent
  )

  plotValues
}

asNumericWithoutNA <- function(selectedData) {
  if (any(!sapply(selectedData, is.numeric))) {
    # transform to numeric
    selectedData <- toNumericCols(selectedData)
  }

  if (!is.null(selectedData) && any(is.na(selectedData))) {
    # removes NA values only from selected data
    selectedData <- na.omit(selectedData)
  }

  selectedData
}

getPrepData <- function(data, xSelection, ySelection){
  data <- removeDataOutliers(data)

  x <- tryCatchWithMessage(getPrepDataPart(data, xSelection, part = "X"))
  y <- tryCatchWithMessage(getPrepDataPart(data, ySelection, part = "Y"))

  bind_cols(x, y)
}

getPrepDataPart <- function(data, selection, part) {
  if (part != "X" && part != "Y") stop("Wrong column specification.")

  datSel <- getSelectedData(data, selection)

  res <- list()
  res[[part]] <- getMean(datSel$columns, datSel$type)
  res[[paste0(part, "Uncertainty")]] <- getUncertainty(datSel$columns, datSel$type,
                                                       credPercent = datSel$credPercent,
                                                       zeroIfPoint = TRUE) %>%
    replaceNA()

  if (part == "X") {
    res$XUncertainty2 <- getUncertainty2(res$X, res$XUncertainty, datSel$type)
    res$standardizedX <- (res$X - mean(res$X)) / (sd(res$X))
  }

  as.data.frame(res)
}

getSelectedData <- function(data, selection) {
  list(columns = data[, unlist(selection$colNames), drop = FALSE],
       type = selection$type,
       credPercent = selection$credPercent)
}

getMean <- function(df, dataType) {
  if (dataType %in% c("point", "meanSD", "meanSEMSD")) return(df[, 1])
  if (dataType %in% c("interval", "credInterval")) return((df[, 1] + df[, 2]) / 2)

  stop("Unknown dataType, please add a rule.")
}

getUncertainty <- function(df, dataType, credPercent = NA, zeroIfPoint = FALSE) {
  if (dataType == "point") {
    if (zeroIfPoint) return(rep(0, length(df[, 1]))) else return(NULL)
  }
  if (dataType == "meanSD") return(df[, 2])
  if (dataType == "meanSEMSD") return(df[, 2] * sqrt(length(df[, 2])))
  if (dataType == "interval") return(abs(df[, 1] - df[, 2]) / 4)
  if (dataType == "credInterval") {
    if (is.na(credPercent)) warning("type == credInterval, but credPercent value == NA")
    p <- credPercent / 100
    z <- qnorm((p + 1) / 2)
    if (is.infinite(z)) z <- 8.2
    sigma <- abs(df[, 2] - df[, 1]) / (2 * z)
    return(sigma)
  }
}

getUncertainty2 <- function(dataMean, dataUncertainty, dataType) {
  if (dataType == "point") return(NULL)
  if (dataType %in% c("interval", "meanSD", "meanSEMSD"))
    return(pmax(0, dataUncertainty / sd(dataMean)))
}

replaceNA <- function(df) {
  if (is.null(df)) return(NULL)

  df[is.na(df)] <- 0
  return(df)
}

