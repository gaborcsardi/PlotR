testthat::test_that("function createGoodnessOfFitTable", {
  testTable <- "m1" %>%
    createGoodnessOfFitTable(llog = structure(
      c(
        0.524883897559859,
        -39.7318857160679,
        -1.18109903307794,
        -1.94142944155971,
        -1.60037046874117,
        -3.79445299433086,
        0.684114476275018,
        -0.680511285203661,
        -0.163397818378108
      ),
      dim = c(3L, 3L)
    ),
    edf = 502.32)

  expect_equal(testTable,
               structure(
                 list(
                   plot = "m1",
                   AIC = 1008.91,
                   BIC = 556.13,
                   n_obs = 3L
                 ),
                 class = "data.frame",
                 row.names = c(NA,
                               -1L)
               ))
})
