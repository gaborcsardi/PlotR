library("PlotR")
library("shiny")

function (input, output, session) {
  loadedFiles <- callModule(uploadFiles, "files")
  savedPlots <- callModule(runModel, "model", loadedFiles = loadedFiles)
  callModule(postProcessing, "post", savedData = savedPlots)
  callModule(stylePlot, "style", savedData = savedPlots)
  callModule(addMorePoints, "addPoints", savedData = savedPlots)
  callModule(downUploads, "downUpload", savedData = savedPlots, loadedFiles = loadedFiles)
  callModule(multiplePlots, "multiple", savedData = savedPlots)
  callModule(multiplePredictions, "multiplePreds", savedData = savedPlots,
             loadedFiles = loadedFiles)
  callModule(goodnessOfFit, "goodness", savedData = savedPlots)

  observeEvent(input$getHelp, {
    showModal(modalDialog(
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })
}
