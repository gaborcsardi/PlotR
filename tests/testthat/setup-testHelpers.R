# helper function for tests ####

# defines a list of input combinations
# requires the colnames of min/max/mean/sd values of the testData for x and for y

findTestCombis <- function(xCols, yCols) {

  xInputs <- list(
    point = list(xType = "point",
                 xNames = list(colName1 = xCols[["mean"]], colName2 = NULL)),
    interval = list(xType = "interval",
                    xNames = list(colName1 = xCols[["min"]], colName2 = xCols[["max"]])),
    meanSD = list(xType = "meanSD",
                  xNames = list(colName1 = xCols[["mean"]], colName2 = xCols[["sd"]]))
  )

  yInputs <- list(
    point = list(yType = "point",
                 yNames = list(colName1 = yCols[["mean"]], colName2 = NULL)),
    interval = list(yType = "interval",
                    yNames = list(colName1 = yCols[["min"]], colName2 = yCols[["max"]])),
    meanSD = list(yType = "meanSD",
                  yNames = list(colName1 = yCols[["mean"]], colName2 = yCols[["sd"]]))
  )

  combis <- expand.grid(x = names(xInputs), y = names(yInputs))

  lapply(1:nrow(combis), function(i) c(xInputs[[combis[i, "x"]]],
                                       yInputs[[combis[i, "y"]]]))
}
