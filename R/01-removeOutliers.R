addColumnDataOutlier <- function(selectedData, ySelection,
                          dataOutlier){

  if (dataOutlier$outlierD == TRUE) {
    selectedData <- checkForDataOutliers(
      data = selectedData,
      yNames = ySelection$colNames, yType = ySelection$type,
      outlierValueD = dataOutlier$outlierValueD)
  } else {
    selectedData$isDataOutlier <- FALSE
  }

  selectedData$id <- NA
  selectedData[!selectedData$isDataOutlier, "id"] <- 1:nrow(removeDataOutliers(selectedData))
  selectedData
}

checkForDataOutliers <- function(data, yNames, yType, outlierValueD) {
  data$isDataOutlier <- findDataOutlier(
    points = getMean(data[, unlist(yNames), drop = FALSE], dataType = yType),
    outlierValueD = outlierValueD
  )

  data
}

findDataOutlier <- function(points, outlierValueD) {
  moD <- mean(points, na.rm = TRUE)
  soD <- sd(points, na.rm = TRUE)

  (points < moD - outlierValueD * soD) | (points > moD + outlierValueD * soD)
}

selectDataOutliers <- function(data){
  data[data$isDataOutlier, ]
}

removeDataOutliers <- function(data){
  data[!data$isDataOutlier, ]
}

findModelOutlier <- function(data, predictedData,
                             yNames, yType,
                             outlierValue) {

  predictedData$id <- rownames(predictedData)
  data <- merge(x = data, y = predictedData, by="id", all.x = TRUE)
  data$residuals <- abs(data$Estimation -
                          getMean(data[, unlist(yNames), drop = FALSE], yType)
                        )
  data$isModelOutlier <- (data$residuals > outlierValue * data$SD)
  data
}

selectModelOutliers <- function(data){
  data[data$isModelOutlier, ]
}

removeModelOutliers <- function(data){
  data[!data$isModelOutlier, ]
}
