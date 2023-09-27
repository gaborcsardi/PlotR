#' Remove Model Outputs
#'
#' @param models list of model objects
removeModelOutputs <- function(models) {
  lapply(models, function(model) {
    model$plotValues$modelData <- NULL
    model$plotValues$predictedData <- NULL
    model
  })
}

#' Extract Model Outputs
#'
#' @param models list of model objects
extractModelOutputs <- function(models) {
  lapply(models, function(model) {
    model$plotValues <- model$plotValues[c("modelData", "predictedData")]
    model$plotStyle <- NULL
    model
  })
}

#' Combine Data and Model Outputs
#'
#' @param modelData list of model data objects
#' @param modelOutput list of model output objects
combineDataAndModelOutputs <- function(modelData, modelOutput) {
  stopifnot("Cannot combine data and model output!" = all(names(modelData) == names(modelOutput)))

  model <- modelData
  for (name in names(model)) {
    model[[name]]$plotValues <- c(model[[name]]$plotValues, modelOutput[[name]]$plotValues)
  }

  model
}

extractSavedModels <- function(upload) {
  # old format < 23.09.0
  if (length(upload[["data"]]) == 0) {
    uploadedData <- upload[["model"]]
    return(uploadedData)
  }

  if (length(upload[["model"]]) > 0) {
    # model output was saved
    uploadedData <- combineDataAndModelOutputs(
      modelData = upload[["data"]],
      modelOutput = upload[["model"]]
    )
    return(uploadedData)
  } else {
    # only data and inputs were saved
    uploadedData <- upload[["data"]]
  }

  return(uploadedData)
}

#' Inc Index Of Name
#'
#' If the name has no index, add a new index: "(1)". If an index already exists, increase it by one.
#'
#' @param name (character) name
incIndexOfName <- function(name) {
  # extract index
  currentIndex <-
    regmatches(name, regexpr("\\([[:digit:]]+\\)$", name))

  # inc index
  if (length(currentIndex) == 0) {
    paste0(name, "(1)")
  } else {
    # get new index
    newIndex <- currentIndex %>%
      gsub(pattern = "\\(|\\)",
           replacement = "") %>%
      as.numeric() + 1

    # replace with new index
    gsub("\\([[:digit:]]+\\)$" ,
         paste0("(", newIndex, ")") ,
         name)
  }
}
