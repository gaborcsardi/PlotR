#' Predict Estimations for More X Input
#'
#' @param moreMean numeric mean input value
#' @param moreSD numeric sd for input value
#' @param plotValues list of plotValues from a savedPlots element
#' @param moreNSample (numeric) number of samples for the input
predictAdditionalData <- function(moreMean, moreSD,
                                  plotValues,
                                  moreNSample = NULL) {

  if (is.null(plotValues$modelData$modelOutput)) stop("Cannot predict data! No model output found.")
  modelData <- plotValues$modelData$modelOutput
  prepData <- getPrepData(data = plotValues$selectedData,
              xSelection = getSelection(plotValues$dataSettings$xColumns),
              ySelection = getSelection(plotValues$dataSettings$yColumns))
  smoothConst <- plotValues$modelParameters$smoothConst
  ppValues <- plotValues$ppValues

  if (is.null(moreNSample) || is.null(moreSD) || sum(abs(moreSD)) == 0) xVar <- moreMean else {
      xVar <- get("rnorm")(moreNSample, moreMean, moreSD)
  }

  predictPipe(
    plotRModel = modelData,
    xCol = prepData$X,
    xVar = xVar,
    quantile = 0.95, smoothConst = smoothConst,
    postProcessing = length(ppValues) > 0, ppValues = ppValues
  ) %>%
    as.list() %>%
    as.data.frame()
}


#' Derive Explanatory for More Y Input
#'
#' @param moreMean numeric mean input value
#' @param moreSD numeric sd for input value
#' @param plotValues list of plotValues from savedPlots element
#' @param graphName (character) name of a savedPlots element
#' @param moreNSample (numeric) number of samples for the input
#' @param isTest TRUE if used within tests
deriveExplanatory <- function(moreMean, moreSD,
                              plotValues,
                              graphName,
                              moreNSample = NULL,
                              isTest = FALSE) {
  if (is.null(plotValues$modelData$modelOutput)) stop("Cannot derive explanatory! No model output found.")
  modelData <- plotValues$modelData$modelOutput
  prepData <- getPrepData(data = plotValues$selectedData,
                          xSelection = getSelection(plotValues$dataSettings$xColumns),
                          ySelection = getSelection(plotValues$dataSettings$yColumns))
  ppValues <- plotValues$ppValues

  if (is.null(moreNSample) || is.null(moreSD) || moreSD == 0) {
    yMean <- moreMean
  } else {
    yMean <- get("rnorm")(moreNSample, moreMean, moreSD)
  }

  xCol <- prepData$X
  possibleXVar <- getXVarEvenly(m = prepData$X, si = prepData$XUncertainty, length = 1000)

  predictedSample <- predictSample(plotRModel = modelData,
                                   xCol = xCol,
                                   xVar = possibleXVar,
                                   postProcessing = length(ppValues) > 0,
                                   ppValues = ppValues)$Predictions

  predDensities <- lapply(1:nrow(predictedSample), function(x) density(predictedSample[x,]))

  if (isTest) {
    lapply(yMean, function(y) {
      deriveSingleExplanatory(yMean = y, xVar = possibleXVar, predDensities = predDensities)
    }) %>%
      bind_rows()
  } else {
    withProgress(
      message = paste("Deriving Data for", graphName),
      detail = paste0("This may take some seconds ..."),
      value = 0, {
        lapply(yMean, function(y) {
          incProgress(1 / length(yMean))
          deriveSingleExplanatory(yMean = y, xVar = possibleXVar, predDensities = predDensities)
        }) %>%
          bind_rows()
      })
  }
}


#' Derive Explanatory from Densities of the Estimation Sample
#'
#' @param yMean numeric value for which to derive the explanatory
#' @param xVar numeric vector of possible values for the explanatory
#' @param predDensities list of outputs from density()
#' @param quantile (numeric) value between (0,1)
deriveSingleExplanatory <- function(yMean, xVar, predDensities, quantile = 0.95) {

  xVarDensity <- lapply(1:length(predDensities), function(i) {
    posi <- which.min(abs(predDensities[[i]]$x - yMean))
    data.frame(xVar = xVar[i],
               likelihood = predDensities[[i]]$y[posi],
               bandwidth = predDensities[[i]]$bw)
  }) %>% bind_rows()

  xVarEstimates <- rdensity(n = 100,
                            x = xVarDensity$xVar,
                            probs = xVarDensity$likelihoods,
                            bw = mean(xVarDensity$bandwidth))

  xVarMean <- mean(xVarEstimates)
  xVarSD <- sd(xVarEstimates)
  xVarSE <- xVarSD / sqrt(length(xVarEstimates))

  data.frame(y = yMean,
             xVar_MLE = xVar[which.max(xVarDensity$likelihood)],
             xVar_Sample_Mean = xVarMean,
             xVar_Sample_Median = median(xVarEstimates),
             xVar_Sample_SD = xVarSD,
             xVar_Sample_Mean_Q_lower = xVarMean + qnorm((1 - quantile)/2) * xVarSE,
             xVar_Sample_Mean_Q_upper = xVarMean + qnorm(1 - (1 - quantile)/2) * xVarSE)
}


#' Generate n random numbers from density() object
#'
#' @param n The total random numbers to generate
#' @param x numeric vector
#' @param probs probabilities for x
#' @param bw bandwidth used in density()
rdensity <- function(n, x, probs, bw) {
  sample(x = x, size = n, prob = probs, replace = TRUE) + rnorm(n, 0, bw)
}


#' Filter or restrict the common xRange
#'
#' @param commonPredictions (data.frame) data.frame with columns xVar, and Estimations for each selected plot
#' @param xRange (numeric) range for which correlation will be calculated
filterCommonXRange <- function(commonPredictions, xRange) {
  res <- commonPredictions %>% filter(.data$xVar >= xRange[1], .data$xVar <= xRange[2])
  res$xVar <- NULL
  res
}


#' Predictions for Selected Models for a Common X Range
#'
#' @param activeDataList (reactive) list of selected saved data
#' @param xRange (numeric) range for which correlation will be calculated
#' @param isTest TRUE if used within tests
getCommonPredictions <- function(activeDataList, xRange, isTest = FALSE) {
  xVar <- seq(from = xRange[1], to = xRange[2], length.out = 1000)

  if (isTest) {
    c(setNames(list(xVar), "xVar"),
      lapply(activeDataList, function(x) {
        predictAdditionalData(moreMean = xVar,
                              moreSD = 0,
                              plotValues = x$plotValues)[, "Estimation"]}
      )) %>%
      bind_cols()
  } else {
    withProgress(
      message = "Predicting data",
      detail = paste0("This may take some seconds ..."),
      value = 0, {
        c(setNames(list(xVar), "xVar"),
          lapply(activeDataList, function(x) {
            incProgress(1 / length(activeDataList))
            prediction <- predictAdditionalData(moreMean = xVar,
                                                moreSD = 0,
                                                plotValues = x$plotValues) %>%
              tryCatchWithWarningsAndErrors(errorTitle = paste("Prediction failed for", x$plotName),
                                            alertStyle = "shinyalert")
            if (is.null(prediction)) return(NULL)
            prediction[, "Estimation"]}
          )) %>%
          bind_cols() %>%
          tryCatchWithWarningsAndErrors(errorTitle = "Common Prediction failed", alertStyle = "shinyalert")
      })
  }
}


#' Predictions for Selected Models for a Common X Range
#'
#' @param activeDataList (reactive) list of selected saved data
#' @param what (character) name of the function to apply to xRange
getCommonX <- function(activeDataList, what) {
  lapply(activeDataList, function(x) x$plotStyle$xRange) %>% unlist() %>% get(what)()
}
