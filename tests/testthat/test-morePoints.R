context("more points")


testthat::test_that("function addPoint", {

  morePointsList <- list(
    `Point 1` = list(y = 1000L, ymin = NA, ymax = NA, x = 200L,
                     xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                     pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1),
    `Point 2` = list(y = 1000L, ymin = NA, ymax = NA,
                     x = 300L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#C254E3",
                     pointSize = 2, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1),
    `Point 3` = list(y = 1000L, ymin = NA, ymax = NA,
                     x = 400L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#44D91F",
                     pointSize = 3, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1),
    `Point 4` = list(y = 1000L, ymin = NA, ymax = NA,
                     x = 500L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                     pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1)
    )

  oneMorePoint <- list(y = 1000L, ymin = NA, ymax = NA,
                       x = 200L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#ED8968",
                       pointSize = 2, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1)

  testthat::expect_equal(
    addPoint(morePointsList, "Point 5", oneMorePoint),
    list(
      `Point 1` = list(y = 1000L, ymin = NA, ymax = NA, x = 200L,
                       xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 2` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 300L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#C254E3",
                       pointSize = 2, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 3` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 400L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#44D91F",
                       pointSize = 3, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 4` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 500L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 5` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 200L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#ED8968",
                       pointSize = 2, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1))
    )
})


testthat::test_that("function removePoints", {

  morePointsList <- list(
    `Point 1` = list(y = 1000L, ymin = NA, ymax = NA, x = 200L,
                     xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                     pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1),
    `Point 2` = list(y = 1000L, ymin = NA, ymax = NA,
                     x = 300L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                     pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1),
    `Point 3` = list(y = 1000L, ymin = NA, ymax = NA,
                     x = 400L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                     pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1),
    `Point 4` = list(y = 1000L, ymin = NA, ymax = NA,
                     x = 500L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                     pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                     fontType = 1))

  testthat::expect_equal(
    removePoints(morePointsList, c("Point 2", "Point 3")),
    list(
      `Point 1` = list(y = 1000L, ymin = NA, ymax = NA, x = 200L,
                       xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 4` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 500L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1)
    )
  )

  testthat::expect_equal(
    removePoints(morePointsList, "Point 1"),
    list(
      `Point 2` = list(y = 1000L, ymin = NA, ymax = NA, x = 300L,
                       xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 3` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 400L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1),
      `Point 4` = list(y = 1000L, ymin = NA, ymax = NA,
                       x = 500L, xmin = NA, xmax = NA, hidePointLabel = FALSE, pointColor = "#002350",
                       pointSize = 1, pointSymbol = 19, textColor = "#002350", textSize = 1,
                       fontType = 1)
    )
  )

})
