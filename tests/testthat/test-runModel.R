context("run model")

testthat::test_that("function addColumnDataOutlier", {

  selectedData <- structure(list(
    min.95. = c(-55, -33, -11, 12, 39, 67, 98, 129,
                162, 194, 203, 265, 304, 346, 410, 473, 514, 559, 596, 630, 655,
                675, 691, 703, 713, 716, 721),
    Cereal.Sum = c(543.9296188, 1327.66702,
                   1175.908707, 678.5853659, 129.6458527, 1453.605016, 254.6636156,
                   1266.407563, 453.7181996, 2132.582547, 3549.959401, 1485.340344,
                   2046.829856, 1936.738956, 1677.708543, 224.2065934, 288.20635,
                   668.3963964, 1790.182921, 888.3141762, 631.2422008, 717.0618557,
                   1233.244681, 0, 410.17249, 602.2077922, 88.4726683)),
    class = "data.frame", row.names = c(NA, -27L))

  selectedData <- addColumnDataOutlier(selectedData,
                                       list(type = "point",
                                            colNames = list(colName1 = "Cereal.Sum",
                                                            colName2 = NULL),
                                            credPercent = NULL),
                                       list(outlierD = TRUE, outlierValueD = 2))

  testthat::expect_equal(removeDataOutliers(selectedData)$Cereal.Sum[1:10],
                         c(543.9296188, 1327.66702, 1175.908707, 678.5853659, 129.6458527,
                           1453.605016, 254.6636156, 1266.407563, 453.7181996, 2132.582547))

  testthat::expect_equal(selectDataOutliers(selectedData)$min.95.,
                         203)
})

testthat::test_that("function getPrepData", {
  data <- data.frame(
    min.95. = c(-55,-33,-11,12,39,67,98,129,
                162,194,203,265,304,346,410,473,514,559,596,630,
                655,675,691,703,713,716,721),
    Cereal.Sum = c(543.9296188,1327.66702,1175.908707,
                   678.5853659,129.6458527,1453.605016,254.6636156,
                   1266.407563,453.7181996,2132.582547,3549.959401,1485.340344,
                   2046.829856,1936.738956,1677.708543,224.2065934,288.20635,
                   668.3963964,1790.182921,888.3141762,631.2422008,
                   717.0618557,1233.244681,0,410.17249,602.2077922,88.4726683),
    isDataOutlier = c(FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE),
    id = c(1L,2L,3L,4L,5L,6L,7L,8L,9L,
           10L,NA,11L,12L,13L,14L,15L,16L,17L,18L,19L,20L,
           21L,22L,23L,24L,25L,26L)
  )

  prepData <- getPrepData(data = data,
                          xSelection = list(type = "point",
                                            colNames = list(colName1 = "min.95.",
                                                            colName2 = NULL),
                                            credPercent = NULL),
                          ySelection = list(type = "point",
                                            colNames = list(colName1 = "Cereal.Sum",
                                                            colName2 = NULL),
                                            credPercent = NULL))

  testthat::expect_equal(prepData$X[1:10],
                         c(-55, -33, -11, 12, 39, 67, 98, 129, 162, 194))

  testthat::expect_equal(prepData$YUncertainty[1:10],
                         c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

  testthat::expect_equal(prepData$standardizedX[1:10],
                         c(-1.50527189968611, -1.42701910257437, -1.34876630546264,
                           -1.26695656302764, -1.17091903929959, -1.07132457024829,
                           -0.961059265227206, -0.85079396020612, -0.733414764538512,
                           -0.619592514194166))
})

testthat::test_that("function getModelFit, predictData", {
  data <- data.frame(
    min.95. = c(-55,-33,-11,12,39,67,98,129,
                162,194,203,265,304,346,410,473,514,559,596,630,
                655,675,691,703,713,716,721),
    Cereal.Sum = c(543.9296188,1327.66702,1175.908707,
                   678.5853659,129.6458527,1453.605016,254.6636156,
                   1266.407563,453.7181996,2132.582547,3549.959401,1485.340344,
                   2046.829856,1936.738956,1677.708543,224.2065934,288.20635,
                   668.3963964,1790.182921,888.3141762,631.2422008,
                   717.0618557,1233.244681,0,410.17249,602.2077922,88.4726683),
    isDataOutlier = c(FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE),
    id = c(1L,2L,3L,4L,5L,6L,7L,8L,9L,
           10L,NA,11L,12L,13L,14L,15L,16L,17L,18L,19L,20L,
           21L,22L,23L,24L,25L,26L)
  )

  prepData <- structure(list(
    X = c(-55, -33, -11, 12, 39, 67, 98, 129, 162,
          194, 265, 304, 346, 410, 473, 514, 559, 596, 630, 655, 675, 691,
          703, 713, 716, 721),
    XUncertainty = c(0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    standardizedX = c(-1.50527189968611,
                      -1.42701910257437, -1.34876630546264, -1.26695656302764, -1.17091903929959,
                      -1.07132457024829, -0.961059265227206, -0.85079396020612, -0.733414764538512,
                      -0.619592514194166, -0.367049396242647, -0.228328528635474, -0.0789368250585192,
                      0.148707675630174, 0.372795230995606, 0.518629989249301, 0.678692528796038,
                      0.810299505756689, 0.931235646747557, 1.02015927982908, 1.09129818629429,
                      1.14820931146647, 1.1908926553456, 1.22646210857821, 1.23713294454799,
                      1.25491767116429),
    Y = c(543.9296188, 1327.66702, 1175.908707,
          678.5853659, 129.6458527, 1453.605016, 254.6636156, 1266.407563,
          453.7181996, 2132.582547, 1485.340344, 2046.829856, 1936.738956,
          1677.708543, 224.2065934, 288.20635, 668.3963964, 1790.182921,
          888.3141762, 631.2422008, 717.0618557, 1233.244681, 0, 410.17249,
          602.2077922, 88.4726683),
    YUncertainty = c(0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    class = "data.frame", row.names = c(NA, -26L))

  modelData <- getModelFit(data = data,
                        prepData = prepData,
                        xSelection = list(type = "point",
                                          colNames = list(colName1 = "min.95.",
                                                          colName2 = NULL),
                                          credPercent = NULL),
                        ySelection = list(type = "point",
                                          colNames = list(colName1 = "Cereal.Sum",
                                                          colName2 = NULL),
                                          credPercent = NULL),
                        modelParameters = list(outlier = TRUE, outlierValue = 2,
                                               burnin = 1000L, iter = 5000L, nChains = 4L,
                                               smoothConst = 1L, K = 25L, sdVar = FALSE, const = 2))



  testthat::expect_equal(modelData$data$xVar[1:10],
                         c(-55, -33, -11, 12, 39, 67, 98, 129, 162, 194))
  testthat::expect_type(modelData$modelOutput$range$mean, "double")
  testthat::expect_true(length(modelData$modelOutput$mRe) == 1)


  predictedData <- predictData(modelData = modelData$modelOutput,
                               prepData = prepData,
                               smoothConst = 1, postProcessing = FALSE, ppValues = list())

  testthat::expect_equal(predictedData$evenlyOnX$xVar[1:3],
                         c(-55, -47.1616161616162, -39.3232323232323))
  testthat::expect_type(predictedData$observations$Estimation, "double")
  testthat::expect_type(predictedData$observations$SD, "double")
})

