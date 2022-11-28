checkWarningEmptyValues <- reactive({
  function(data) {
    vals <- data[, -1, drop = FALSE]

    #mode(vals) <- "numeric"
    # if (attr(data, "includeSd")) {
    #   vals <- vals[, seq(2, ncol(vals), by = 2, )]
    # }

    vals <- as.data.frame(sapply(vals, function(x) suppressWarnings(as.numeric(x))))

    if (any(is.na(vals) | vals == "")) {
      return("Found empty / non-numeric values.")
    }

    TRUE
  }
})

checkErrorNoNumericColumns <- reactive({
  function(data) {
    nNumericCol <- sum(findNumericCol(as.data.frame(data)))

    if (nNumericCol < 2) {
      return("Less than 2 columns with numeric values.")
    }

    TRUE
  }
})

toNumericCols <- function(data) {
  if (is.null(data)) return(NULL)

  data <- data[, findNumericCol(data), drop = FALSE]
  suppressWarnings(as.data.frame(sapply(data, as.numeric)))
}

findNumericCol <- function(df) {
  cols <- lapply(df, function(x) suppressWarnings(as.numeric(x)))
  unlist(lapply(cols, function(x) !all(is.na(x))))
}
