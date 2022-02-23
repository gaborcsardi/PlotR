testthat::test_that("addPackageVersionNo", {
  testthat::expect_equal(
    addPackageVersionNo("abc"),
    "abc\n\nPlotR version 22.2.1 ."
  )
})
