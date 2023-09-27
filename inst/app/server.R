library("PlotR")
library("shiny")
library("yaml")

# load config variables
configFile <- system.file("config.yaml", package = "PlotR")
appConfig <- yaml::read_yaml(configFile)

function (input, output, session) {
  loadedFiles <- callModule(uploadFiles, "files", config = appConfig)
  savedPlots <- callModule(runModel, "model", loadedFiles = loadedFiles)
  callModule(postProcessing, "post", savedData = savedPlots)
  callModule(stylePlot, "style", savedData = savedPlots)
  callModule(addMorePoints, "addPoints", savedData = savedPlots)
  callModule(downUploads, "downUpload", savedData = savedPlots,
             loadedFiles = loadedFiles, config = appConfig)
  callModule(multiplePlots, "multiple", savedData = savedPlots)
  callModule(multiplePredictions, "multiplePreds", savedData = savedPlots,
             loadedFiles = loadedFiles)
  callModule(goodnessOfFit, "goodness", savedData = savedPlots)
}
