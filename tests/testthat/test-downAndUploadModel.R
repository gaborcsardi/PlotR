testthat::test_that("addPackageVersionNo",  {
  testthat::expect_equal(addPackageVersionNo("abc"),
                         "abc\n\nPlotR version 1.0.1 .")
})
