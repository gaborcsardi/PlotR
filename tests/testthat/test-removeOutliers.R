context("remove outlier functions")

# collect testData ####

combiInputs <- list(
  list(xType = "point", xNames = list(colName1 = "min.BC.AD",  colName2 = NULL),
       yType = "point", yNames = list(colName1 = "Cereal.Sum", colName2 = NULL)),
  list(xType = "interval", xNames = list(colName1 = "min.BC.AD", colName2 = "max.BC.AD"),
       yType = "point", yNames = list(colName1 = "Cereal.Sum", colName2 = NULL)),
  list(xType = "meanSD", xNames = list(colName1 = "min.BC.AD",  colName2 = "BC_AD_SD"),
       yType = "point", yNames = list(colName1 = "Cereal.Sum", colName2 = NULL)),
  list(xType = "point", xNames = list(colName1 = "min.BC.AD", colName2 = NULL),
       yType = "interval", yNames = list(colName1 = "Cereal.Sum", colName2 = "Cereal.Sum.Max")),
  list(xType = "interval", xNames = list(colName1 = "min.BC.AD", colName2 = "max.BC.AD"),
       yType = "interval", yNames = list(colName1 = "Cereal.Sum", colName2 = "Cereal.Sum.Max")),
  list(xType = "meanSD", xNames = list(colName1 = "min.BC.AD", colName2 = "BC_AD_SD"),
       yType = "interval", yNames = list(colName1 = "Cereal.Sum", colName2 = "Cereal.Sum.Max")),
  list(xType = "point", xNames = list(colName1 = "min.BC.AD", colName2 = NULL),
       yType = "meanSD", yNames = list(colName1 = "Cereal.Sum", colName2 = "CerSumSD")),
  list(xType = "interval", xNames = list(colName1 = "min.BC.AD", colName2 = "max.BC.AD"),
       yType = "meanSD", yNames = list(colName1 = "Cereal.Sum", colName2 = "CerSumSD")),
  list(xType = "meanSD", xNames = list(colName1 = "min.BC.AD", colName2 = "BC_AD_SD"),
       yType = "meanSD", yNames = list(colName1 = "Cereal.Sum", colName2 = "CerSumSD"))
  )

testData <- structure(list(
  min.95. = c(-55L, -33L, -11L, 12L, 39L, 67L, 98L,
              129L, 162L, 194L, 203L, 265L, 304L, 346L, 410L, 473L, 514L, 559L,
              596L, 630L, 655L, 675L, 691L, 703L, 713L, 716L, 721L),
  max.95. = c(-50L, 24L, 98L, 166L, 226L, 277L, 320L, 356L, 389L, 416L, 423L, 465L,
              492L, 518L, 560L, 602L, 628L, 660L, 692L, 721L, 747L, 775L, 802L,
              830L, 857L, 886L, 911L),
  Cereal.Sum = c(543.9296188, 1327.66702,
                 1175.908707, 678.5853659, 129.6458527, 1453.605016, 254.6636156,
                 1266.407563, 453.7181996, 2132.582547, 3549.959401, 1485.340344,
                 2046.829856, 1936.738956, 1677.708543, 224.2065934, 288.20635,
                 668.3963964, 1790.182921, 888.3141762, 631.2422008, 717.0618557,
                 1233.244681, 0, 410.17249, 602.2077922, 88.4726683),
  min.BC.AD = c(2005L, 1983L, 1961L, 1938L, 1911L, 1883L, 1852L, 1821L, 1788L, 1756L,
                1747L, 1685L, 1646L, 1604L, 1540L, 1477L, 1436L, 1391L, 1354L,
                1320L, 1295L, 1275L, 1259L, 1247L, 1237L, 1234L, 1229L),
  max.BC.AD = c(2000L, 1926L, 1852L, 1784L, 1724L, 1673L, 1630L, 1594L, 1561L, 1534L,
                1527L, 1485L, 1458L, 1432L, 1390L, 1348L, 1322L, 1290L, 1258L,
                1229L, 1203L, 1175L, 1148L, 1120L, 1093L, 1064L, 1039L),
  Cereal.Sum.Max = c(646.355540024444,
                     1430.09294122444, 1278.33462822444, 781.011287124444, 232.071773924444,
                     1556.03093722444, 357.089536824444, 1368.83348422444, 556.144120824444,
                     2235.00846822444, 3652.38532222444, 1587.76626522444, 2149.25577722444,
                     2039.16487722444, 1780.13446422444, 326.632514624444, 390.632271224444,
                     770.822317624444, 1892.60884222444, 990.740097424444, 733.668122024444,
                     819.487776924444, 1335.67060222444, 102.425921224444, 512.598411224444,
                     704.633713424444, 190.898589524444),
  CerSumSD = c(50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L,
               50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L, 50L),
  BC_AD_SD = c(20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L,
               20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L
  )),
  class = "data.frame", row.names = c(NA, -27L))

# tests ####

testthat::test_that("function findDataOutlier", {

  testthat::expect_type(findDataOutlier(testData[[1]], 1), "logical")
  testthat::expect_equal(length(findDataOutlier(testData[[1]], 1)),
                         length(testData[[1]]))
})

testthat::test_that("function checkForDataOutliers", {

  for (i in 1:length(combiInputs)) {
    testData <- checkForDataOutliers(
      data = testData,
      yNames = combiInputs[[i]]$yNames,
      yType = combiInputs[[i]]$yType,
      outlierValueD = 2)

    testthat::expect_type(testData, "list")
    testthat::expect_equal(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                             FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                             FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                           testData$isDataOutlier)
    testthat::expect_false(any(is.na(testData$isDataOutlier)))
  }

})

load("predTestData.RData")

testthat::test_that("function findModelOutlier", {

  testData$id <- 1:nrow(testData)

  for (i in 1:length(combiInputs)) {
    checkResult <- findModelOutlier(
      data = testData,
      predictedData = predTestData[[i]]$observations,
      yNames = combiInputs[[i]]$yNames, yType = combiInputs[[i]]$yType,
      outlierValue = 2)

    testthat::expect_type(checkResult, "list")
    testthat::expect_true(all(names(testData) %in% names(checkResult)))
    testthat::expect_false(any(is.na(checkResult)))
  }

})
