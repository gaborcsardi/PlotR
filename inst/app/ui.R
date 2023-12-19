library("PlotR")
library("shiny")

tagList(
    navbarPage(
        header = includeCSS("www/custom.css"),
        title = paste("PlotR", packageVersion("PlotR")),
        theme = shinythemes::shinytheme("flatly"),
        position = "fixed-top",
        collapsible = TRUE,
        id = "tab",
        uploadFilesUI("files", "File Import(s)"),
        runModelUI("model", "Plot Data & Run Model"),
        postProcessingUI("post", "Post Processing"),
        stylePlotUI("style", "Style Plot"),
        addMorePointsUI("addPoints", "Add Points"),
        downUploadsUI("downUpload", "Down-/Upload"),
        multiplePlotsUI("multiple", "Multiple Plots"),
        multiplePredictionsUI("multiplePreds", "Multiple Predictions"),
        goodnessOfFitUI("goodness", "Goodness of Fit")
    ),
    div(
      id = "header-right",
      div(
        id = "logo-mpi",
        tags$a(href = "https://www.mpg.de/en",
               img(src = "MPIlogo.png", alt = "Supported by the Max Planck society"),
               target = "_blank"
        )
      ),
      div(
        id = "logo-isomemo",
        tags$a(href = "https://isomemo.com/",
               img(src = "IsoMemoLogo.png", alt = "IsoMemo"),
               target = "_blank"
        )
      ),
      div(
        id = "help",
        actionButton("getHelp", "?")
      )
    ),
    shinyjs::useShinyjs()
)

