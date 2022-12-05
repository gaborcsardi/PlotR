# Prevent Errors in R CMD check in line
# s <- smoothCon(s(standardizedX, m = penalty, k = K), data = data, knots = NULL)[[1]]
# s2 <- smoothCon(s(Date3, m = penalty, k = K), data = data, knots = NULL)[[1]]
utils::globalVariables(c("standardizedX", "Date3"))

#' Get plotValues
#'
#' @param plotValues (list) list with all values required for the plot
#' @param activeFile (character) name of the selected file
#' @param activeFileData (data.frame) content of the selected file
#' @param dataSelection (list) list of reactive selected columns of the
#'  current file
#' @param modelParameters (list) list of settings for the model
getPlotValues <- function(plotValues, activeFile, activeFileData, dataSelection,
                          modelParameters){
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
    activeFileData[, unlist(c(xSelection$colNames, ySelection$colNames))]

  if (any(!sapply(plotValues$selectedData, is.numeric))) {
    # transform to numeric
    plotValues$selectedData <- toNumericCols(plotValues$selectedData)
  }

  if (!is.null(plotValues$selectedData) && any(is.na(plotValues$selectedData))) {
    plotValues$selectedData <- na.omit(plotValues$selectedData)
  }

  if (is.null(plotValues$selectedData)) return(plotValues)

  plotValues$selectedData <- addColumnDataOutlier(
    selectedData = plotValues$selectedData,
    ySelection = ySelection,
    dataOutlier = plotValues$dataSettings$dataOutlier
  )

  prepData <- getPrepData(data = plotValues$selectedData,
                          xSelection = xSelection,
                          ySelection = ySelection)

  plotValues$modelParameters <- modelParameters
  plotValues$modelData <- getModelFit(
    data = removeDataOutliers(plotValues$selectedData),
    prepData = prepData,
    xSelection,
    ySelection,
    plotValues$modelParameters
  )

  if (is.null(plotValues$modelData$modelOutput)) return(plotValues)

  plotValues$predictedData <- predictData(modelData = plotValues$modelData$modelOutput,
                               prepData = prepData,
                               smoothConst = plotValues$modelParameters$smoothConst)

  plotValues$defaultXRange <- getRange(
    data = plotValues$selectedData[, unlist(xSelection$colNames),
                                   drop = FALSE],
    type = xSelection$type,
    credPercent = xSelection$credPercent,
    estimation = getXVarEvenly(m = prepData$X,
                               si = prepData$XUncertainty)
  )

  plotValues
}

getModelFit <- function(data,
                     prepData,
                     xSelection, ySelection,
                     modelParameters,
                     isCheck = FALSE
){
  modelOutput <- fitModel(
    prepData = prepData,
    modelParameters = modelParameters,
    progressMessage = "Calculating Model",
    isCheck = isCheck
  )

  if (is.null(modelOutput)) {
    data$isModelOutlier <- FALSE
    return(list(data = data,
                modelOutput = NULL))
  }

  if (modelParameters$outlier == TRUE) {

    predictedData <- predictData(modelData = modelOutput,
                                 prepData = prepData,
                                 smoothConst = modelParameters$smoothConst)

    data <- findModelOutlier(data = data,
                             predictedData = predictedData$observations,
                             yNames = ySelection$colNames,
                             yType = ySelection$type,
                             outlierValue = modelParameters$outlierValue)

    modelOutput <- fitModel(
      prepData = getPrepData(data = data,
                             xSelection = xSelection, ySelection = ySelection),
      modelParameters = modelParameters,
      progressMessage = "Removing model outliers",
      isCheck = isCheck)

    list(data = data,
         modelOutput = modelOutput
    )
  } else {
    data$isModelOutlier <- FALSE
    list(data = data,
         modelOutput = modelOutput)
  }
}

fitModel <- function(prepData,
                     modelParameters,
                     progressMessage = "Calculating Model",
                     isCheck = FALSE){

  K <- modelParameters$K
  smoothConst <- modelParameters$smoothConst
  burnin <- modelParameters$burnin
  iter <- modelParameters$iter
  nChains <- modelParameters$nChains
  sdVar <- modelParameters$sdVar
  const <- modelParameters$const

  tryCatchWithMessage(
    fitPlotRModelMC(
      prepData,
      K = K,
      burnin = burnin,
      iter = iter,
      penalty = const,
      smoothConst = smoothConst,
      nChains = nChains,
      sdVar = sdVar,
      progressMessage = progressMessage,
      isCheck = isCheck
    )
  )
}

fitPlotRModelMC <- function(data,
                            K = 24, burnin = 1000,
                            iter = 24000, penalty = 2,
                            smoothConst = 1,
                            nChains = 4,
                            sdVar = FALSE,
                            progressMessage = "Calculating Model",
                            isCheck = FALSE){

  n <- nrow(data)
  if (n < K) {
    shinyalert("Not enough rows for running the model.",
               "Please use more data rows, increase the sd for outlier removal or
                     decrease the number of basis functions.",
               type = "error")
    return(NULL)
  }

  ret <- lapply(1:nChains, function(x){
    fitPlotRModel(data = data, K = K, burnin = burnin,
                  iter = iter,
                  penalty = penalty,
                  smoothConst = smoothConst,
                  nChains = x,
                  sdVar = sdVar,
                  progressMessage = progressMessage,
                  isCheck = isCheck)
  })

  res <- ret[[1]]
  res$beta <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$beta))
  res$betaSigma <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$betaSigma))
  res$sigma <- do.call("c", lapply(1:length(ret), function(x) ret[[x]]$sigma))
  return(res)
}


fitPlotRModel <- function(data,
                          K = 12, burnin = 1000,
                          iter = 24000, penalty = 2,
                          smoothConst = 1,
                          nChains = 4,
                          sdVar = FALSE,
                          progressMessage = "Calculating Model",
                          isCheck = FALSE){

  data$Date4 <- data$standardizedX

  n <- nrow(data)

  nknots <- K
  burnInProp <- pmax(pmin(0.8, burnin / iter), 0.01)
  thinning <- max(1, floor(iter * (1 - burnInProp) / 1000))

  s <- smoothCon(s(standardizedX, m = penalty, k = K), data = data, knots = NULL)[[1]]

  data$Date3 <- data$standardizedX

  s2 <- smoothCon(s(Date3, m = penalty, k = K), data = data, knots = NULL)[[1]]

  #Strafmatrizen
  P <- s$S[[1]]
  #Rang der Strafmatrix P
  nknots <- dim(P)[1]
  M <- qr(P)$rank

  #Zur jeweiligen Erstellung der Praediktionsmatrix

  #Designmatrix
  XX <- Predict.matrix(s, data)

  #####################################
  ###Starting Values
  #####################################
  #Chain 1
  if (!sdVar){
    sigma <- 1
  } else {
    sigma <- rep(1, nrow(data))
  }

  lam <- 1E-5
  beta <- rep(0, ncol(XX))
  XX2 <- XX

  if (sdVar){
    sigmaSigma <- rep(1, nrow(data))
    betaSigma <- rep(0, ncol(XX))
    lamSigma <- 1E-5
    lamSigma2 <- 1E-5
  }

  ######################################
  ###Tuningparameter der a-priori Verteilungen:
  ######################################
  a.eps <- 1E-5
  b.eps <- 1E-5
  a.mu <- 1E-5
  b.mu <- 1E-5
  lam.mu <- 1E-5
  lam.sigma <- 1E-5

  #######################################
  ###Parametermatrizen zur Speicherung der MCMC Iterationen
  #######################################
  betamc <- matrix(ncol = dim(XX)[2], nrow = iter)
  betamcSigma <- matrix(ncol = dim(XX)[2], nrow = iter)
  taumc <- matrix(ncol = 1, nrow = iter)
  smc <- matrix(ncol = 1, nrow = iter)

  ########################################
  #MCMC-Algorithmus
  ########################################
  changeX <- which(data$XUncertainty2 > 0)

  #rescale
  mRe <- mean(data$Y)
  sRe <- sd(data$Y)
  data$Y <- (data$Y - mRe) / sRe

  if (!is.null(data$YUncertainty)){
    data$YUncertainty <- data$YUncertainty / sRe
  }

  YMean <- data$Y
  MCMCplotR <- function(start, iter){
    for (i in start:iter) {
      if (!is.null(data$YUncertainty)){
        sdmY <- 1 / (1 / sigma + 1 / (data$YUncertainty ^ 2 + 1E-6))
        mY <- ((XX2 %*% beta) / sigma +
                 YMean / (data$YUncertainty ^ 2 + 1E-6)) * sdmY
        data$Y <- rnorm(length(data$YUncertainty), mY, sd = sqrt(sdmY))
      }
      #Betas
      inverse <- spdinv(crossprod(XX2 / sigma, XX2) + lam * P)
      beta <<- as.vector(rmvnorm(1, mu = inverse %*% crossprod(XX2 / sigma, data$Y),
                                 sigma = inverse))
      # #MH-step for time

      if (length(changeX) > 0){
        random <- rnorm(n, sd = data$XUncertainty2)
        data$Date3 <- data$standardizedX + random
        XX <- Predict.matrix(s2, data)
        alphas <- rep(0, n)
        if (sdVar){
          sigmaChange <- sigma[changeX]
        } else {
          sigmaChange <- sigma
        }
        sigmaChange <- sigmaChange * smoothConst
        alphas[changeX] <- AcceptanceTime(data[changeX, ],
                                          XX[changeX, ],
                                          XX2[changeX, ],
                                          sigmaChange,
                                          beta)
        alphas[is.na(alphas)] <- 0
        randomAlpha <- runif(n)
        updated <- which(randomAlpha < alphas)
        data$standardizedX[updated] <- data$Date3[updated]
        if (length(updated) > 0){
          XX2[updated, ] <- (Predict.matrix(s, data[updated, ]))
        }
      }

      #Sigma
      #Smoothing Parameter lambda
      lam <<- rgamma(
        1,
        shape = lam.mu + M / 2,
        scale = (lam.sigma + 0.5 * crossprod(beta, P) %*% beta) ^ - 1
      ) * smoothConst

      # nolint start
      #conditional posterioris:
      if (!sdVar){
        scale <- (b.eps + 0.5 * sum((((data$Y - XX2 %*% beta)) ^ 2))) ^ - 1
        sigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale)

      } else {
        scale0 <- (b.eps + 0.5 * sum((((data$Y - XX2 %*% beta)) ^ 2))) ^ - 1
        sigma0 <- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale0)

        scaleSigma <- (b.eps + 0.5 * sum((log((data$Y - XX %*% beta)^2) - (XX %*% betaSigma)) ^ 2)) ^ - 1
        sigmaSigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scaleSigma)
        inverseSigma <- spdinv(crossprod(XX2 / sigmaSigma, XX2) + lamSigma * P)
        betaSigma <<- as.vector(rmvnorm(1, mu = inverseSigma %*%
                                          crossprod((XX2 / sigmaSigma),
                                                    log((data$Y - XX2 %*% beta) ^ 2)),
                                        sigma = inverseSigma))
        sigmaTmp <-  as.numeric(exp(XX2 %*% (betaSigma)))
        sigma0 <- mean(sigmaTmp) / sigma0
        sigma <<- sigmaTmp / sigma0 + 1E-4
      }

      if (sdVar){
        lamSigma <<- rgamma(
          1,
          shape = lam.mu + M / 2,
          scale = (lam.sigma + 0.5 * crossprod(betaSigma, P) %*% betaSigma) ^ - 1
        )
      }

      # nolint end

      #Werte in Parametermatrizen einsetzen
      betamc[i, ] <<- beta
      if (sdVar){
        betamcSigma[i, ] <<- betaSigma
      }
      if (sdVar){
        smc[i, ]  <<-  sigma0
      } else {
        smc[i, ]  <<-  mean(sigma)
      }
      # gammamc[i, ] <- gamma
      # lambdamc[i, ] <- lam
    }
    return(betamc)
  }

  if (isCheck) {
    for ( k in 1:10) {
      j <- seq(1, iter, iter / 10)[k]
      MCMCplotR(start = j, iter = j + iter / 10 - 1)
    }
  } else {
    withProgress(message = progressMessage,
                 detail = paste0("Chain ", nChains, " This may take some seconds ..."), value = 0, {
                   for ( k in 1:10) {
                     incProgress(1 / 10)
                     j <- seq(1, iter, iter / 10)[k]
                     # showMessage(
                     #   MCMCplotR,
                     #   msg = "Calculating Spatio-Temporal Average Model",
                     #   value = k / 10)(
                     #     start = j, iter = j + iter / 10 - 1
                     #   )
                     MCMCplotR(start = j, iter = j + iter / 10 - 1)
                   }
                 })
  }

  burnin <- round(burnInProp * iter)
  every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden

  #Vektor der tatsaechlich benutzten Beobachtungen
  usedsamples <- seq(from = burnin, to = iter, by = every)
  if (sdVar){
    seTotal <- range(sqrt(apply(sapply(1:length(usedsamples), function(x)
      (XX2 %*% betamc[usedsamples[x], ]) * sRe + mRe), 1, var) +
        rowMeans(sapply(1:length(usedsamples), function(x)
          exp((XX2 %*% betamcSigma[usedsamples[x], ])) / smc[usedsamples[x]] * sRe^2))))
    betamcSigma <- betamcSigma[usedsamples, ]

  } else {
    betamcSigma <- NULL
    seTotal <- range(sqrt(apply(sapply(1:length(usedsamples), function(x)
      (XX %*% betamc[usedsamples[x], ]) * sRe + mRe), 1, var) + mean(smc)))
  }

  list(beta = betamc[usedsamples, ], betaSigma = betamcSigma,
       sc = s, sigma = smc[usedsamples, ],
       mRe = mRe, sRe = sRe,
       range = list(mean = range(rowMeans(sapply(1:length(usedsamples), function(x)
         (XX2 %*% betamc[usedsamples[x], ]) * sRe + mRe))),
         se = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
           (XX2 %*% betamc[usedsamples[x], ]) * sRe + mRe), 1, var))),
         seTotal = seTotal)
  )
}

AcceptanceTime <- function(data, XX, XX2, sigma, beta){
  pmin(1, exp(
    cpostX(
      XX = XX,
      xtru = data$Date3,
      xobs = data$Date4,
      sigma.obs = data$XUncertainty2 ^ 2,
      sigma.eps = sigma,
      beta = beta,
      y = data$Y) -
      cpostX(
        XX = XX2,
        xtru = data$standardizedX,
        xobs = data$Date4,
        sigma.obs = data$XUncertainty2 ^ 2,
        sigma.eps = sigma,
        beta = beta,
        y = data$Y
      )
  ))
}

cpostX <- function(XX,
                   xtru,
                   xobs,
                   sigma.obs,
                   sigma.eps,
                   beta,
                   y) {
  (y - XX %*% beta) ^ 2 / (-2 * sigma.eps) +
    + (xtru - xobs)^2 / (- sigma.obs / 2)
  #   log(dunif (
  #   xtru,
  #   min = xobs - sqrt(sigma.obs),
  #   max = xobs + sqrt(sigma.obs)
  # ))
}
