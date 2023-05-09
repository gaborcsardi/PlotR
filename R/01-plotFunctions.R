makeMultiPlot <- function(valuesList, nMarginLines, combiType = "rowGrid", nGridCols = 2,
                          xAxisToHide = NULL, yAxisToHide = NULL, showSig = FALSE,
                          referencePlot = NULL, sigLevel = 0.95, legendPosition
                          ){

  if (is.null(names(valuesList))) return(NULL)

  if(showSig){
    if(!is.null(referencePlot) & length(names(valuesList)) > 1){
      referenceCurve <- valuesList[[referencePlot]]$plotValues$predictedData$evenlyOnX

      for (q in names(valuesList)){
        valuesList[[q]]$plotValues$predictedData$evenlyOnX$sig <- FALSE

        relevantOverlap <- getRelevantOverlap(valuesOfQ = valuesList[[q]],
                                              referenceCurve = referenceCurve,
                                              sigLevel = sigLevel)

        if(NROW(relevantOverlap)>0){
          sigIndex <-
            valuesList[[q]]$plotValues$predictedData$evenlyOnX$xVar %in% relevantOverlap$xVar
          valuesList[[q]]$plotValues$predictedData$evenlyOnX$sig[sigIndex] <- TRUE
        }
      }
    }
  }

  if(combiType == "joinedPlot") {
    par(mar = c(3 * (nMarginLines$bottom),
                3 * (nMarginLines$left),
                3 * (nMarginLines$top),
                3 * (nMarginLines$right)
                ) + 0.1)
    for (p in names(valuesList)) {
      i <- which(p == names(valuesList))
      if(i > 1) { par(new = T) }
      makePlot(valuesList[[p]]$plotValues, valuesList[[p]]$plotStyle,
               hideXAxis = (p %in% xAxisToHide),
               hideYAxis = (p %in% yAxisToHide),
               marginLine = 3 * (i - 1))
    }

    # add legend to multiplot
      addLegendServer(
              id = "joinedPlot",
              position = legendPosition,
              title = "Color for data points",
              legendEntries = names(valuesList),
              fillColor = unlist(lapply(valuesList, function(x) x$plotStyle$dataPoints$color)))
  }

  if(combiType == "fullGrid") {
    par(mfrow = c(ceiling(length(valuesList)/nGridCols), nGridCols))
    for (p in names(valuesList)) {
      makePlot(valuesList[[p]]$plotValues, valuesList[[p]]$plotStyle,
               hideXAxis = (p %in% xAxisToHide),
               hideYAxis = (p %in% yAxisToHide))
    }
  }
}


#' Get Relevant Overlap
#'
#' @param valuesOfQ predictions of the curve q
#' @param referenceCurve predictions of the reference curve
#' @param sigLevel significance level
getRelevantOverlap <- function(valuesOfQ, referenceCurve, sigLevel) {
  #get relevant overlap
  xVarsOverlapIndex <-
    valuesOfQ$plotValues$predictedData$evenlyOnX$xVar >= min(referenceCurve$xVar) &
    valuesOfQ$plotValues$predictedData$evenlyOnX$xVar <= max(referenceCurve$xVar)

  relevantOverlap <- valuesOfQ$plotValues$predictedData$evenlyOnX[xVarsOverlapIndex, ]

  #match closest points
  for (i in 1:nrow(relevantOverlap)){
    bestReferenceMatch <- which.min(abs(relevantOverlap$xVar[i] - referenceCurve$xVar))
    pv <- pnorm(referenceCurve[bestReferenceMatch,]$Estimation - relevantOverlap$Estimation[i],0,
                sqrt((relevantOverlap$SE[i])^2 + referenceCurve[bestReferenceMatch,]$SE^2))
    if(min(pv*2, (1 - pv) * 2) < (1 - sigLevel)){
      relevantOverlap$sig[i] <- TRUE
    }
  }
  relevantOverlap <- relevantOverlap[relevantOverlap$sig,]

  return(relevantOverlap)
}

makeSinglePlot <- function(plotValues, plotStyle){
  par(mar = c(4.1, 4.1, 4.1, 4.1))
  makePlot(plotValues = plotValues, plotStyle = plotStyle)
}

makePlot <- function(plotValues, plotStyle, hideXAxis = FALSE, hideYAxis = FALSE, marginLine = 0){
  xSel <- getSelection(plotValues$dataSettings$xColumns)
  ySel <- getSelection(plotValues$dataSettings$yColumns)

  plotAxes(plotStyle = plotStyle,
           marginLine = marginLine,
           hideXAxis = hideXAxis,
           hideYAxis = hideYAxis
  )

  # points are drawn in the following order
  # 1. plot normal points ----
  if (!is.null(plotValues$modelData$data)) {
    # plot data without data and model outliers
    plotDataPoints(removeModelOutliers(plotValues$modelData$data),
                   xNames = xSel$colNames, xType = xSel$type, xCredPercent = xSel$credPercent,
                   yNames = ySel$colNames, yType = ySel$type, yCredPercent = ySel$credPercent,
                   stylePoints = plotStyle$dataPoints,
                   styleIntervals = plotStyle$dataIntervals)
  } else {
    # plot data without data outliers
    plotDataPoints(removeDataOutliers(plotValues$selectedData),
                   xNames = xSel$colNames, xType = xSel$type, xCredPercent = xSel$credPercent,
                   yNames = ySel$colNames, yType = ySel$type, yCredPercent = ySel$credPercent,
                   stylePoints = plotStyle$dataPoints,
                   styleIntervals = plotStyle$dataIntervals)
  }

  # 2. plot outliers ----
  # plot data outliers
  plotDataPoints(selectDataOutliers(plotValues$selectedData),
                 xNames = xSel$colNames, xType = xSel$type, xCredPercent = xSel$credPercent,
                 yNames = ySel$colNames, yType = ySel$type, yCredPercent = ySel$credPercent,
                 stylePoints = plotStyle$dataOutliers,
                 styleIntervals = plotStyle$dataOutlierIntervals)

  if (!is.null(plotValues$modelData$data)) {
    # plot model outliers
    plotDataPoints(selectModelOutliers(plotValues$modelData$data),
                   xNames = xSel$colNames, xType = xSel$type, xCredPercent = xSel$credPercent,
                   yNames = ySel$colNames, yType = ySel$type, yCredPercent = ySel$credPercent,
                   stylePoints = plotStyle$modelOutliers,
                   styleIntervals = plotStyle$modelOutlierIntervals)
  }



  # 3. plot prediction ----
  if (!is.null(plotValues$predictedData)) {
    plotPredictions(predData = plotValues$predictedData$evenlyOnX,
                    centerType = plotValues$plottedTypeOfPrediction$centerType,
                    errorType = plotValues$plottedTypeOfPrediction$errorType,
                    uncertaintyFactor = plotValues$plottedTypeOfPrediction$SDFactor,
                    stylePrediction = plotStyle$predictionLine,
                    styleUncertainty = plotStyle$modelUncertainty)
  }

  # 4. plot manually added data ----
  plotDataPointsAdd(pointDat = plotStyle$morePoints)
}

plotAxes <- function(plotStyle,
                     marginLine = 0,
                     hideXAxis = FALSE,
                     hideYAxis = FALSE
) {
  plot(mean(plotStyle$xRange),
       mean(plotStyle$yRange),
       pch = NA,
       xlim = plotStyle$xRange,
       ylim = plotStyle$yRange,
       axes = F,
       xlab = "",
       ylab = "",
       main = plotStyle$plotTitle$text,
       col.main = plotStyle$plotTitle$textColor,
       cex.main = plotStyle$plotTitle$textSize,
       font.main = plotStyle$plotTitle$fontType)

  if (marginLine == 0) {
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col = plotStyle$colorBg)
    box(lwd = 1)
  }

  if (!hideXAxis)  getAxis(plotStyle$xAxisLabel, plotStyle$sideXAxis, marginLine)
  if (!hideYAxis) getAxis(plotStyle$yAxisLabel, plotStyle$sideYAxis, marginLine)
}

getAxis <- function(axisLabel, axisSide, marginLine){
  axis(axisSide, line = marginLine, col = "black", lwd = 1)
  mtext(side = axisSide, line = marginLine + 1.9,
        text = axisLabel$text, col = axisLabel$textColor,
        cex = axisLabel$textSize, font = axisLabel$fontType)
}

plotDataPoints <- function(data,
                           xNames, yNames,
                           xCredPercent, yCredPercent,
                           xType = "point", yType = "point",
                           stylePoints,
                           styleIntervals,
                           labels = NULL, hideLabels = TRUE,
                           labelCex = 1, labelVFont = NULL, labelCol = NULL
                           ) {
  if (is.null(data)) return(NULL)

  pointColor <- stylePoints$color
  pointSymbol <- stylePoints$symbol
  pointSize <- stylePoints$size
  pointLineWidth <- stylePoints$lineWidth
  pointBgCol <- stylePoints$colorBg
  hideData <- stylePoints$hide
  errorColor <- styleIntervals$color
  intervalLineType <- styleIntervals$lineType
  intervalWidth <- styleIntervals$lineWidth
  hideErrors <- styleIntervals$hide

  x <- getMean(data[, unlist(xNames), drop = FALSE], dataType = xType)
  xDev <- getUncertainty(data[, unlist(xNames), drop = FALSE], dataType = xType,
                         credPercent = xCredPercent, zeroIfPoint = TRUE)

  y <- getMean(data[, unlist(yNames), drop = FALSE], dataType = yType)
  yDev <- getUncertainty(data[, unlist(yNames), drop = FALSE], dataType = yType,
                         credPercent = yCredPercent, zeroIfPoint = TRUE)

  if (length(x) == 0 || length(y) == 0) return(NULL)

  xMin <- x - xDev
  xMax <- x + xDev
  yMin <- y - yDev
  yMax <- y + yDev

  if (all(xDev == 0)) xType <- "point" # draw point if no deviation
  if (all(yDev == 0)) yType <- "point" # draw point if no deviation

  plotXErrors(hide = (xType == "point" || hideErrors),
              xMin = xMin, xMax = xMax, y = y,
              col = errorColor, lwd = intervalWidth, lty = intervalLineType)

  plotYErrors(hide = (yType == "point" || hideErrors),
              x = x, yMin = yMin, yMax = yMax,
              col = errorColor, lwd = intervalWidth, lty = intervalLineType)

  plotPoints(hide = hideData,
             x = x, y = y,
             col = pointColor, cex = pointSize, pch = pointSymbol,
             lwd = pointLineWidth, bg = pointBgCol)

  plotLabels(hide = (hideLabels || is.null(labels)),
             x = x, y = y, labels = labels,
             col = labelCol, cex = labelCex, vfont = labelVFont)
}

plotPoints <- function(hide, x, y, ...) {
  if (hide) return(NULL)
  points(x, y, ...)
}

plotXErrors <- function(hide, xMin, xMax, y, ...) {
  if (hide) return(NULL)
  arrows(xMin, y, xMax, y, length = 0.05, angle = 90, code = 3, ...)
}

plotYErrors <- function(hide, x, yMin, yMax, ...) {
  if (hide) return(NULL)
  arrows(x, yMin, x, yMax, length = 0.05, angle = 90, code = 3, ...)
}

plotLabels <- function(hide, x, y, labels, ...) {
  if (hide) return(NULL)
  text(x, y, labels = labels, pos = 4, ...)
}

plotDataPointsAdd <- function(pointDat){
  if(is.null(pointDat) | length(pointDat) == 0) return(NULL)

  for(i in names(pointDat)){
    plotPoints(hide = (is.na(pointDat[[i]]$x) || is.na(pointDat[[i]]$y)),
               x = pointDat[[i]]$x,
               y = pointDat[[i]]$y,
               col = pointDat[[i]]$pointColor, cex = pointDat[[i]]$pointSize,
               pch = as.numeric(pointDat[[i]]$pointSymbol))

    x <- ifelse(!is.na(pointDat[[i]]$x), pointDat[[i]]$x, mean(c(pointDat[[i]]$xmin, pointDat[[i]]$xmax)))

    plotYErrors(hide = (is.na(x) || is.na(pointDat[[i]]$ymin) || is.na(pointDat[[i]]$ymax)),
                x = x,
                yMin = pointDat[[i]]$ymin, yMax = pointDat[[i]]$ymax,
                col = pointDat[[i]]$pointColor, lwd = pointDat[[i]]$pointSize)

    y <- ifelse(!is.na(pointDat[[i]]$y), pointDat[[i]]$y, mean(c(pointDat[[i]]$ymin, pointDat[[i]]$ymax)))

    plotXErrors(hide = (is.na(y) || is.na(pointDat[[i]]$xmin) || is.na(pointDat[[i]]$xmax)),
                xMin = pointDat[[i]]$xmin, xMax = pointDat[[i]]$xmax,
                y = y,
                col = pointDat[[i]]$pointColor, lwd = pointDat[[i]]$pointSize)

    plotLabels(hide = (pointDat[[i]]$hidePointLabel || is.na(x) || is.na(y)),
               x = x, y = y, labels = i,
               col = pointDat[[i]]$textColor, cex = pointDat[[i]]$textSize,
               font = as.numeric(pointDat[[i]]$fontType))
  }
}

minRange <- function(range) {
  min(range) - 0.5*diff(range) %>% signif(digits = 3)
}

maxRange <- function(range) {
  max(range) + 0.5*diff(range) %>% signif(digits = 3)
}

getRange <- function(data, type, credPercent, estimation = NULL) {

  dataPoints <- getMean(data, dataType = type)
  dataDeviation <- getUncertainty(data, dataType = type, credPercent = credPercent, zeroIfPoint = TRUE)

  return(range(c(dataPoints - dataDeviation, dataPoints + dataDeviation, estimation)))
}

plotPredictions <- function(predData,
                            stylePrediction,
                            styleUncertainty,
                            centerType = "Estimation",
                            errorType = "SEM",
                            uncertaintyFactor = 2
                            ){
  if (is.null(predData)) return(NULL)

  predColor <- stylePrediction$color
  predLineType <- stylePrediction$lineType
  predWidth <- stylePrediction$lineWidth
  predHide <- stylePrediction$hide
  uncertaintyColor <- styleUncertainty$color
  uncertaintyLineType <- styleUncertainty$lineType
  uncertaintyWidth <- styleUncertainty$lineWidth
  uncertaintyHide <- styleUncertainty$hide

  plotRPred <- predData

  plotLines(hide = predHide,
            plotRPred[[centerType]] ~ plotRPred$xVar,
            lwd = predWidth,
            lty = predLineType,
            col = predColor)


  if("sig" %in% colnames(predData)){
    segment <- 1
    plotRPred$segment <- segment
    for (j in 2:NROW(plotRPred)){
      if(plotRPred$sig[j] == TRUE & plotRPred$sig[j-1] == FALSE){
        segment <- segment + 1
      }
      plotRPred$segment[j] <- segment
    }
    segments <- max(plotRPred$segment)
    for (i in 1:segments){
      plotLines(hide = FALSE,
                plotRPred[plotRPred$sig == TRUE & plotRPred$segment == i, centerType] ~
                  plotRPred$xVar[plotRPred$sig == TRUE & plotRPred$segment == i],
                lwd = 3,
                lty = 1,
                col = "#FF0000")

    }
  }

  plotLines(hide = uncertaintyHide,
            getUncertaintyLimit(plotRPred,
                                type = errorType,
                                factor = uncertaintyFactor)$upper ~ plotRPred$xVar,
            lwd = uncertaintyWidth,
            lty = uncertaintyLineType,
            col = uncertaintyColor)
  plotLines(hide = uncertaintyHide,
            getUncertaintyLimit(plotRPred,
                                type = errorType,
                                factor = uncertaintyFactor)$lower ~ plotRPred$xVar,
            lwd = uncertaintyWidth,
            lty = uncertaintyLineType,
            col = uncertaintyColor)
  # lines((plotRPred$Estimation + 1.96 * plotRPred$SETOTAL) ~ plotRPred$xVar, lwd = 1, lty = 3)
  # lines((plotRPred$Estimation - 1.96 * plotRPred$SETOTAL) ~ plotRPred$xVar, lwd = 1, lty = 3)
}

plotLines <- function(hide, x, ...) {
  if (hide) return(NULL)
  lines(x, ...)
}

extractLabel <- function(names){
  if (length(names) == 1) return(names)

  if (length(names) == 2) {
    core1 <- gsub("min", "", names[1], ignore.case = TRUE)
    core2 <- gsub("max", "", names[2], ignore.case = TRUE)

    return(ifelse(core1 == core2, core1, paste(names[1], names[2], sep = " / ")))
  }

  if (length(names) > 2) return(NULL)
}

cleanLabel <- function(dataColumns) {
  label <- getSelection(dataColumns)$colNames$colName1

  result <- gsub("\\.", " ", label)
  result <- gsub("_", " ", result)
  result <- gsub("min", "", result, ignore.case = TRUE)
  result <- gsub("max", "", result, ignore.case = TRUE)

  return(result)
}

# getPaletteColor <- function(name){
#   if(name != "default") {
#     name <- brewer.pal(n = 9, name = name)
#     name <- colorRampPalette(name)
#     name
#   } else return(name)
# }

emptyPlot <- function(label = "No data available") {
  plot(0:1, 0:1, pch = NA, xlab = "", ylab = "")
  text(0.5, 0.5, label = label, col = "red")
}



#' selectInput to specify legend position
#'
#' @param id module id
#' @param label label of button
#' @param choices choices e.g. "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
addLegendUI <- function(id, label = "Legend Position", choices = c("none", "topleft", "topright", "bottomright", "bottomleft")) {
  ns <- NS(id)
    selectizeInput(inputId = ns("legend"),
                   label = label,
                   choices = choices)
}

#' Add a legend to an existing plot
#'
#' @param id module id
#' @param position legend position, e.g. "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
#' @param title legend title
#' @param legendEntries names of legend entries
#' @param fillColor color for the legend symbols
addLegendServer <- function(id, position, title, legendEntries, fillColor) {
  moduleServer(
    id,
    function(input, output, session) {
      if(position != "none"){
      legend(x = position,
             title = title,
             legend = legendEntries,
             fill = fillColor)
      }
    }
  )
}
