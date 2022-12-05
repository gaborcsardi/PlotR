testthat::test_that("function incIndexOfFile", {
  expect_equal(incIndexOfFile("plotR4(3).csv"), "plotR4(4).csv")
  expect_equal(incIndexOfFile("plotR4(3357).xls"), "plotR4(3358).xls")
  expect_equal(incIndexOfFile("plotNew.xls"), "plotNew(1).xls")
  expect_equal(incIndexOfFile("pl(7)otR4(3).csv"), "pl(7)otR4(4).csv")
})
