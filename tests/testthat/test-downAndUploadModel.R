testthat::test_that("addPackageVersionNo", {
  testthat::expect_equal(
    substr(addPackageVersionNo("abc"), 1, 22),
    "abc\n\nPlotR version 23."
  )
})
