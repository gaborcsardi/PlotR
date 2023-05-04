predictData <- function(modelData, prepData, smoothConst, postProcessing = FALSE, ppValues = list()){
  if (is.null(modelData)) stop("Cannot predict data! No model output found.")
    evenlyOnX <- predictSample(plotRModel = modelData,
                               xCol = prepData$X,
                               xVar = getXVarEvenly(m = prepData$X,
                                                    si = prepData$XUncertainty),
                               postProcessing = postProcessing,
                               ppValues = ppValues) %>%
      getEstimations(smoothConst = smoothConst) %>%
      addQuantiles(quantile = 0.95)

    observations <- predictSample(plotRModel = modelData,
                                  xCol = prepData$X,
                                  xVar = prepData$X,
                                  postProcessing = postProcessing,
                                  ppValues = ppValues) %>%
      getEstimations(smoothConst = smoothConst) %>%
      addQuantiles(quantile = 0.95)

  list(
    evenlyOnX = evenlyOnX,
    observations = observations
  )
}

getXVarEvenly <- function(m, si, length = 100) {
  seq(min(m - abs(3 * si)), max(m + abs(3 * si)), length.out = length)
}

predictPipe <- function(plotRModel, xCol, yName = NULL, xVar = seq(0, 1, by = 0.1),
                         smoothConst = 1, quantile = 0.95, postProcessing = FALSE, ppValues = list()){
  if (is.null(plotRModel)) stop("Cannot predict data! No model output found.")
  predictSample(plotRModel = plotRModel,
                xCol = xCol, xVar = xVar,
                postProcessing = postProcessing, ppValues = ppValues) %>%
    getEstimations(smoothConst = smoothConst) %>%
    addQuantiles(quantile = quantile) %>%
    pasteNameOfEstimation(yName)
}

getEstimations <- function(predictedSample, smoothConst){
  if (is.null(predictedSample)) return(NULL)

  Predictions <- predictedSample$Predictions
  PredictionsSigma <- predictedSample$PredictionsSigma

  data.frame(xVar = predictedSample$xVar,
             Estimation = rowMeans(Predictions),
             Est_Median = apply(Predictions, 1, median),
             SE = apply(Predictions, 1, sd) * smoothConst,
             SD = PredictionsSigma,
             SETOTAL = sqrt(PredictionsSigma^2 + (apply(Predictions, 1, sd)* smoothConst)^2))
}

addQuantiles <- function(estimations, quantile){
  if (is.null(estimations)) return(NULL)

  estimations %>% mutate(
    Est_MEAN_Q_lower = .data$Estimation + qnorm((1-quantile)/2) * .data$SE,
    Est_MEAN_Q_upper = .data$Estimation + qnorm(1 - (1-quantile)/2) * .data$SE,
    Est_Total_Q_lower = .data$Estimation + qnorm((1-quantile)/2) * .data$SETOTAL,
    Est_Total_Q_upper = .data$Estimation + qnorm(1 - (1-quantile)/2) * .data$SETOTAL
  )
}

getUncertaintyLimit <- function(pred, type = "SEM", factor = 2) {
  lower <- switch(type,
                  "SD" = pred$Estimation - factor * pred$SD,
                  "SEM" = pred$Est_MEAN_Q_lower,
                  "SEMTotal" = pred$Est_Total_Q_lower)
  upper <- switch(type,
                  "SD" = pred$Estimation + factor * pred$SD,
                  "SEM" = pred$Est_MEAN_Q_upper,
                  "SEMTotal" = pred$Est_Total_Q_upper)

  return(list(lower = lower,
              upper = upper))
}

pasteNameOfEstimation <- function(estimations, yName){
  if (is.null(estimations)) return(NULL)

  if(!is.null(yName)){
    names(estimations) <- paste0(yName, "_", names(estimations))
  }

  estimations
}

predictSample <- function(plotRModel, xCol, xVar, postProcessing = FALSE, ppValues = list()) {
  if (is.null(plotRModel)) return(NULL)

  PredMatr <- Predict.matrix(plotRModel$sc,
                             data = data.frame(
                               standardizedX = (xVar - mean(xCol)) /
                                 sd(xCol)))
  betas <- plotRModel$beta
  betaSigma <- plotRModel$betaSigma

  Predictions <-
    sapply(1:nrow(betas), function(x)
      PredMatr %*% betas[x, ] * plotRModel$sRe + plotRModel$mRe)

  if(is.null(nrow(Predictions))) {
    Predictions <- matrix(Predictions, nrow = 1)
  }

  if(postProcessing){
    for(i in 1:length(ppValues)){
      if(length(ppValues[[i]]) > 0){
        ppValuesTemp <- ppValues[[i]]
        unc <- ppValuesTemp$unc
        if(ppValuesTemp$operation != "dev"){
          if(ppValuesTemp$compare == "Plot"){
            betas2 <- ppValuesTemp$Plot$beta
            if(nrow(betas2) < nrow(betas)){
              betas2 <- betas2[rep(1:nrow(betas2), ceiling(nrow(betas) / nrow(betas2))),]
            }

            Predictions2 <-
              sapply(1:nrow(betas), function(x)
                PredMatr %*% betas2[x, ] * ppValuesTemp$Plot$sRe +
                  ppValuesTemp$Plot$mRe)


            if(ppValuesTemp$xValueType == "allx"){
              if(ppValuesTemp$operation == "geq"){
                Predictions[Predictions < Predictions2] <-  Predictions2[Predictions < Predictions2] +
                  rnorm(sum(Predictions < Predictions2), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[Predictions > Predictions2] <- Predictions2[Predictions < Predictions2] +
                  rnorm(sum(Predictions > Predictions2), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[Predictions != Predictions2] <- Predictions2[Predictions != Predictions2] +
                  rnorm(sum(Predictions != Predictions2), 0, unc)
              }
            }
            if(ppValuesTemp$xValueType == "gx"){
              selected <- xVar > ppValuesTemp$compX1
              if(ppValuesTemp$operation == "geq"){
                Predictions[selected, ][Predictions[selected, ] < Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] < Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] < Predictions2[selected, ]), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[selected, ][Predictions[selected, ] > Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] > Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] > Predictions2[selected, ]), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[selected, ][Predictions[selected, ] != Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] != Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] != Predictions2[selected, ]), 0, unc)
              }
            }
            if(ppValuesTemp$xValueType == "lx"){
              selected <- xVar < ppValuesTemp$compX1
              if(ppValuesTemp$operation == "geq"){
                Predictions[selected, ][Predictions[selected, ] < Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] < Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] < Predictions2[selected, ]), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[selected, ][Predictions[selected, ] > Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] > Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] > Predictions2[selected, ]), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[selected, ][Predictions[selected, ] != Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] != Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] != Predictions2[selected, ]), 0, unc)
              }
            }
            if(ppValuesTemp$xValueType == "intx"){
              selected <- xVar < ppValuesTemp$compX2 & xVar > ppValuesTemp$compX1
              if(ppValuesTemp$operation == "geq"){
                Predictions[selected, ][Predictions[selected, ] < Predictions2[selected, ]] <-Predictions2[selected, ][Predictions[selected, ] < Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] < Predictions2[selected, ]), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[selected, ][Predictions[selected, ] > Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] > Predictions2[selected, ]]+
                  rnorm(sum(Predictions[selected, ] > Predictions2[selected, ]), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[selected, ][Predictions[selected, ] != Predictions2[selected, ]] <- Predictions2[selected, ][Predictions[selected, ] != Predictions2[selected, ]] +
                  rnorm(sum(Predictions[selected, ] != Predictions2[selected, ]), 0, unc)
              }
            }

          } else {
            if(ppValuesTemp$compare == "Value interval"){
              if(ppValuesTemp$xValueTypeC == "intx"){
                yValues <-  Predictions[xVar < ppValuesTemp$compX2C & xVar > ppValuesTemp$compX1C, ]
              }
              if(ppValuesTemp$xValueType == "lx"){
                yValues <-  Predictions[xVar < ppValuesTemp$compX1C, ]
              }
              if(ppValuesTemp$xValueType == "gx"){
                yValues <-  Predictions[xVar > ppValuesTemp$compX1C, ]
              }
              toCompare <- mean(yValues)
              unc <- sqrt(unc^2 + sd(yValues)^2)
            } else {
              toCompare <- ppValuesTemp$compY
            }

            if(ppValuesTemp$xValueType == "allx"){
              if(ppValuesTemp$operation == "geq"){
                Predictions[Predictions < toCompare] <- toCompare +
                  rnorm(sum(Predictions < toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[Predictions > toCompare] <- toCompare +
                  rnorm(sum(Predictions > toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[Predictions != toCompare] <- toCompare +
                  rnorm(sum(Predictions != toCompare), 0, unc)
              }
            }
            if(ppValuesTemp$xValueType == "gx"){
              selected <- xVar > ppValuesTemp$compX1
              if(ppValuesTemp$operation == "geq"){
                Predictions[selected, ][Predictions[selected, ] < toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] < toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[selected, ][Predictions[selected, ] > toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] > toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[selected, ][Predictions[selected, ] != toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] != toCompare), 0, unc)
              }
            }
            if(ppValuesTemp$xValueType == "lx"){
              selected <- xVar < ppValuesTemp$compX1
              if(ppValuesTemp$operation == "geq"){
                Predictions[selected, ][Predictions[selected, ] < toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] < toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[selected, ][Predictions[selected, ] > toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] > toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[selected, ][Predictions[selected, ] != toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] != toCompare), 0, unc)
              }
            }
            if(ppValuesTemp$xValueType == "intx"){
              selected <- xVar < ppValuesTemp$compX2 & xVar > ppValuesTemp$compX1
              if(ppValuesTemp$operation == "geq"){
                Predictions[selected, ][Predictions[selected, ] < toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] < toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "leq"){
                Predictions[selected, ][Predictions[selected, ] > toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] > toCompare), 0, unc)
              }
              if(ppValuesTemp$operation == "eq"){
                Predictions[selected, ][Predictions[selected, ] != toCompare] <- toCompare +
                  rnorm(sum(Predictions[selected, ] != toCompare), 0, unc)
              }
            }
          }
        } else {
          # error if only one row, e.g. predicting only one datapoint
          Predictions <- apply(Predictions, 2, diff)
          Predictions <- rbind(Predictions[1,], Predictions)
        }
      }
    }
  }

  if (!is.null(plotRModel$betaSigma)){
    PredictionsSigma <-
      rowMeans(sqrt(sapply(1:nrow(plotRModel$betaSigma), function(x)
        exp((PredMatr %*% plotRModel$betaSigma[x, ])) / plotRModel$sigma[x]) * plotRModel$sRe^2))
  } else {
    PredictionsSigma <- sqrt(mean(plotRModel$sigma) * plotRModel$sRe^2)
  }

  list(Predictions = Predictions,
       PredictionsSigma = PredictionsSigma,
       xVar = xVar)
}
