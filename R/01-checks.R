toNumericCols <- function(data) {
  if (is.null(data)) return(NULL)

  data <- data[, findNumericCol(data), drop = FALSE]
  suppressWarnings(as.data.frame(sapply(data, as.numeric)))
}

findNumericCol <- function(df) {
  cols <- lapply(df, function(x) suppressWarnings(as.numeric(x)))
  unlist(lapply(cols, function(x) !all(is.na(x))))
}

numericColumnNames <- function(df) {
  i <- unlist(lapply(df, isNumeric))
  names(df)[i]
}

partialNumericColumnNames <- function(df) {
  i <- unlist(lapply(df, function(x) !all(is.na(suppressWarnings(as.numeric(x))))))
  names(df)[i]
}

characterColumnNames <- function(df) {
  i <- unlist(lapply(df, is.character))
  names(df)[i]
}

isNumeric <- function(x) {
  is.numeric(x) || is.integer(x)
}
