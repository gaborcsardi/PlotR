library("PlotR")
library("shiny")

tagList(
    navbarPage(
        title = paste("PlotR", packageVersion("PlotR")),
        theme = shinythemes::shinytheme("flatly"),
        position = "fixed-top",
        collapsible = TRUE,
        id = "tab",
        uploadFilesUI("files", "File Import(s)"),
        runModelUI("model", "Run Model"),
        postProcessingUI("post", "Post Processing"),
        stylePlotUI("style", "Style Plot"),
        addMorePointsUI("addPoints", "Add Points"),
        downUploadsUI("downUpload", "Down-/Upload"),
        multiplePlotsUI("multiple", "Multiple Plots"),
        multiplePredictionsUI("multiplePreds", "Multiple Predictions")
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
)

