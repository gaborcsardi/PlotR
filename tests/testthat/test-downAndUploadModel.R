testthat::test_that("function incIndexOfName", {
  expect_equal(incIndexOfName("plotR4(3)"), "plotR4(4)")
  expect_equal(incIndexOfName("plotR4(3357)"), "plotR4(3358)")
  expect_equal(incIndexOfName("plotNew"), "plotNew(1)")
  expect_equal(incIndexOfName("pl(7)otR4(3)"), "pl(7)otR4(4)")
})

testthat::test_that("function removeModelOutputs", {
  testData <- readRDS(testthat::test_path("test-predictAdditionalData_testDataList.rds"))
  testRes <- removeModelOutputs(testData)

  for (i in seq_along(testData)) {
    expect_equal(names(testRes[[i]]$plotValues),
                 c("plottedTypeOfPrediction", "modelParameters", "activeFile",
                   "xSelection", "ySelection", "activeFileData", "selectedData",
                   "ppValues", "dataSettings"))
    expect_true(!any(c("modelData", "predictedData") %in% names(testRes[[i]]$plotValues)))
  }
})

testthat::test_that("function extractModelOutputs", {
  testData <- readRDS(testthat::test_path("test-predictAdditionalData_testDataList.rds"))
  testRes <- extractModelOutputs(testData)

  for (i in seq_along(testData)) {
    expect_equal(names(testRes[[i]]$plotValues), c("modelData", "predictedData"))
    expect_true(!any(c("plottedTypeOfPrediction", "modelParameters", "activeFile",
                       "xSelection", "ySelection", "activeFileData", "selectedData",
                       "ppValues", "dataSettings") %in% names(testRes[[i]]$plotValues)))
    expect_equal(names(testRes[[i]]), c("plotName", "plotValues"))
  }
})

testthat::test_that("function combineDataAndModelOutputs", {
  testData <- readRDS(testthat::test_path("test-predictAdditionalData_testDataList.rds"))

  testModelData <- removeModelOutputs(testData)
  testModelOutput <- extractModelOutputs(testData)
  testRes <- combineDataAndModelOutputs(testModelData, testModelOutput)

  for (i in seq_along(testData)) {
    expect_setequal(names(testRes[[i]]$plotValues), names(testData[[i]]$plotValues))

    for (name in names(testData[[i]]$plotValues)) {
      expect_equal(testData[[i]][["plotValues"]][[name]],
                   testRes[[i]][["plotValues"]][[name]])
    }
  }
})
