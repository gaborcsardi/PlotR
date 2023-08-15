testthat::test_that("function calcExportData", {
  testData <- data.frame(
    min.BC.AD = c(2005L,1983L,1961L,1938L,1911L,
                  1883L,1852L,1821L,1788L,1756L,1747L,1685L,1646L,1604L,
                  1540L,1477L,1436L,1391L,1354L,1320L,1295L,1275L,
                  1259L,1247L,1237L,1234L,1229L),
    max.BC.AD = c(2000L,1926L,1852L,1784L,1724L,
                  1673L,1630L,1594L,1561L,1534L,1527L,1485L,1458L,1432L,
                  1390L,1348L,1322L,1290L,1258L,1229L,1203L,1175L,
                  1148L,1120L,1093L,1064L,1039L),
    Cereal.Sum = c(543.9296188,1327.66702,1175.908707,
                   678.5853659,129.6458527,1453.605016,254.6636156,
                   1266.407563,453.7181996,2132.582547,3549.959401,1485.340344,
                   2046.829856,1936.738956,1677.708543,224.2065934,288.20635,
                   668.3963964,1790.182921,888.3141762,631.2422008,
                   717.0618557,1233.244681,0,410.17249,602.2077922,88.4726683),
    isDataOutlier = c(FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                      FALSE,FALSE,FALSE,FALSE,FALSE),
    id = c(1L,2L,3L,4L,5L,6L,7L,8L,9L,
           10L,11L,12L,13L,14L,15L,16L,17L,18L,19L,20L,21L,
           22L,23L,24L,25L,26L,27L)
  )

  xSelection <- list(
    type = "interval",
    colNames = list(colName1 = "min.BC.AD", colName2 = "max.BC.AD"),
    credPercent = NULL
  )
  ySelection <- list(
    type = "point",
    colNames = list(colName1 = "Cereal.Sum", colName2 = NULL),
    credPercent = NULL
  )

  prepData <- getPrepData(data = testData,
                          xSelection = xSelection,
                          ySelection = ySelection)

  testModelData <- getModelFit(
    data = testData,
    prepData = prepData,
    xSelection = xSelection,
    ySelection = ySelection,
    modelParameters = list(
      outlier = TRUE,
      outlierValue = 2,
      burnin = 1000L,
      iter = 5000L,
      nChains = 4L,
      smoothConst = 1L,
      K = 25L,
      sdVar = FALSE,
      const = 2
    )
  )

  exportData <- calcExportData(
    xVar = seq(0, 1000, by = 10),
    quantile = 0.776,
    data = testData,
    xSelection = xSelection,
    ySelection = ySelection,
    modelOutput = testModelData$modelOutput,
    renameQColumns = FALSE
  )

  testthat::expect_equal(
    names(exportData),
    c(
      "Cereal.Sum_xVar",
      "Cereal.Sum_Estimation",
      "Cereal.Sum_Est_Median",
      "Cereal.Sum_SE",
      "Cereal.Sum_SD",
      "Cereal.Sum_SETOTAL",
      "Cereal.Sum_Est_MEAN_Q_lower",
      "Cereal.Sum_Est_MEAN_Q_upper",
      "Cereal.Sum_Est_Total_Q_lower",
      "Cereal.Sum_Est_Total_Q_upper"
    )
  )

  exportData <- calcExportData(
    xVar = seq(0, 1000, by = 10),
    quantile = 0.776,
    data = testData,
    xSelection = xSelection,
    ySelection = ySelection,
    modelOutput = testModelData$modelOutput
  )

  testthat::expect_equal(
    names(exportData),
    c(
      "Cereal.Sum_xVar",
      "Cereal.Sum_Estimation",
      "Cereal.Sum_Est_Median",
      "Cereal.Sum_SE",
      "Cereal.Sum_SD",
      "Cereal.Sum_SETOTAL",
      "Cereal.Sum_Est_MEAN_78_lower",
      "Cereal.Sum_Est_MEAN_78_upper",
      "Cereal.Sum_Est_Total_78_lower",
      "Cereal.Sum_Est_Total_78_upper"
    )
  )
})
