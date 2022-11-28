plotExport <- function(input, output, session, plotObj,
                       plotWidth = reactive(1280),
                       plotHeight = reactive(800)){
  observeEvent(input$export, {
    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      plotOutput(session$ns("plot"), height = "300px"),
      selectInput(
        session$ns("exportType"), "Filetype",
        choices = c(
          "png", "pdf", "svg", "tiff"
        )
      ),
      numericInput(session$ns("width"), "Width (px)", value = plotWidth()),
      numericInput(session$ns("height"), "Height (px)", value = plotHeight()),
      downloadButton(session$ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  output$plot <- renderPlot({
    replayPlot(plotObj())
  })

  output$exportExecute <- downloadHandler(
    filename = function(){
      paste0("plotR", ".", input$exportType)
    },
    content = function(file){
      switch(
        input$exportType,
        png = png(file, width = input$width, height = input$height),
        pdf = pdf(file, width = input$width / 72, height = input$height / 72),
        tiff = tiff(file, width = input$width, height = input$height),
        svg = svg(file, width = input$width / 72, height = input$height / 72)
      )
      replayPlot(plotObj())
      dev.off()
    }
  )
}
plotExportButton <- function(id){
  ns <- NS(id)
  actionButton(ns("export"), "Export Plot")
}
