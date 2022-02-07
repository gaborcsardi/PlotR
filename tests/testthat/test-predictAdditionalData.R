context("predict additional data")

testthat::test_that("function addColumnDataOutlier", {
  testData <- readRDS("test-predictAdditionalData_testDataList.rds")
  testPredictions <- predictAdditionalData(moreMean = 400,
                                           moreSD = NULL,
                                           moreNSample = 100,
                                           plotValues = testData[[1]]$plotValues)

  testthat::expect_equal(
    testPredictions,
    data.frame(
      xVar = c(400),
      Estimation = c(1396.62830138083),
      Est_Median = c(1396.51633891337),
      SE = c(6.04153793035514),
      SD = c(6.70078303318344),
      SETOTAL = c(9.0222321972846),
      Est_MEAN_Q_lower = c(1384.7871046261),
      Est_MEAN_Q_upper = c(1408.46949813556),
      Est_Total_Q_lower = c(1378.94505121399),
      Est_Total_Q_upper = c(1414.31155154766)
    )
  )
})


testthat::test_that("function deriveExplanatory", {
  testData <- readRDS("test-predictAdditionalData_testDataList.rds")

  testDerivedX <- deriveExplanatory(moreMean = 500,
                                    moreSD = 0,
                                    plotValues = testData[[2]]$plotValues,
                                    graphName = "p1",
                                    moreNSample = 1,
                                    isTest = TRUE)

  testthat::expect_true(all(sapply(testDerivedX, is.numeric)))
  testthat::expect_false(any(sapply(testDerivedX, is.na)))
})

testthat::test_that("function deriveSingleExplanatory", {
  testDensities <- readRDS("test-predictAdditionalData_testPreds.rds")
  testDerivedX <- deriveSingleExplanatory(
    yMean = 500.2195,
    xVar = c(-55, -54.2232232232232, -53.4464464464464, -52.6696696696697,
             -51.8928928928929, -51.1161161161161, -50.3393393393393, -49.5625625625626,
             -48.7857857857858, -48.009009009009),
    predDensities = testDensities,
    quantile = 0.95)

  testthat::expect_true(all(sapply(testDerivedX, is.numeric)))
  testthat::expect_false(any(sapply(testDerivedX, is.na)))
})


testthat::test_that("function rdensity", {
  testSample <- rdensity(3, x = 1:4, probs = rep(0.25, 4), bw = 0.5)

  testthat::expect_true(all(sapply(testSample, is.numeric)))
  testthat::expect_false(any(sapply(testSample, is.na)))
})


testthat::test_that("function getCommonPredictions, getCommonX and filterCommonXRange", {
  testData <- readRDS("test-predictAdditionalData_testDataList.rds")

  testCommonPreds <- getCommonPredictions(testData ,
                                          xRange = getCommonX(testData, what = "range"),
                                          isTest = TRUE)

  testthat::expect_equal(getCommonX(testData, what = "range"), c(-55, 721))
  testthat::expect_equal(getCommonX(testData, what = "min"), -55)
  testthat::expect_equal(getCommonX(testData, what = "max"), 721)

  testthat::expect_equal(testCommonPreds[1:10,], tribble(
    ~xVar,              ~p2,              ~p1,
    -55, 1999.07417837079, 826.628640412835,
    -54.2232232232232, 1996.42793237573, 828.182108423744,
    -53.4464464464464, 1993.78178256493, 829.735199381638,
    -52.6696696696697, 1991.13582512299,  831.28753623332,
    -51.8928928928929, 1988.49015623398, 832.838741925773,
    -51.1161161161161, 1985.84487208263, 834.388439405909,
    -50.3393393393393, 1983.20006885287, 835.936251620624,
    -49.5625625625626, 1980.55584272958, 837.481801516867,
    -48.7857857857858, 1977.91228989657, 839.024712041548,
    -48.009009009009, 1975.26950653872, 840.564606141563
  ))

  testthat::expect_equal(filterCommonXRange(testCommonPreds, xRange = c(100, 500))[1:3,],
                         tribble(
                           ~p2,              ~p1,
                           1626.53259334953, 1175.20491289582,
                           1625.56986207801,  1178.1186696921,
                           1624.61146168366, 1181.05044142093
                         ))

})
