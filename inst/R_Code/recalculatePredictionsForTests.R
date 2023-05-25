# calculates a list of predictions for all combinations of inputs ("point"/"interval"/"meanSD")

source("tests/testthat/setup-testHelpers.R")

testData <- utils::read.csv("tests/testthat/plotR2.csv")

combiInputs <- findTestCombis(xCols = c(min = "min.BC.AD", max = "max.BC.AD",
                                        mean = "min.BC.AD", sd = "BC_AD_SD"),
                              yCols = c(min =  "Cereal.Sum", max = "Cereal.Sum.Max",
                                        mean = "Cereal.Sum", sd = "CerSumSD"))

predTestData <- lapply(combiInputs, function(i) {

  testData <- addColumnDataOutlier(testData,
                                   list(colNames = i$yNames, type = i$yType),
                                   list(outlierD = FALSE, outlierValueD = 2))

  modelParameters = list(burnin = 1000,
                         iter = 24000,
                         nChains = 4,
                         smoothConst = 1,
                         K = 24,
                         sdVar = FALSE,
                         const = as.numeric(2)
  )

  modelData <- getModelFit(data = removeDataOutliers(testData),
                        prepData = getPrepData(data = testData,
                                               xSelection = list(colNames = i$xNames, type = i$xType),
                                               ySelection = list(colNames = i$yNames, type = i$yType)),
                        list(colNames = i$xNames, type = i$xType),
                        list(colNames = i$yNames, type = i$yType),
                        modelOutlier = list(outlier = TRUE, outlierValue = 2),
                        modelParameters = modelParameters)

  predictData(modelData$modelOutput,
              prepData = prepData,
              smoothConst = 1)
})

save(predTestData, file = "tests/testthat/predTestData.RData")
