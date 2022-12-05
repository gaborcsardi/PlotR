## Combined Input Module ----

#' Slider And Input UI
#'
#' UI of the Slider And Input module
#'
#' @param id id of module
#' @param label label
#' @param min (numeric) minumum
#' @param max (numeric) maximum
#' @param value (numeric) default value
#' @param dragRange (logical) dragRange
sliderAndNumericRangeUI <-
  function(id, label, min, max, value, dragRange) {
    ns <- NS(id)

    tagList(
      sliderInput(
        inputId = ns("sliderIn"),
        label = label,
        min = min,
        max = max,
        value = value,
        width = "100%",
        dragRange = dragRange
      ),
      fluidRow(
        column(
          width = 6,
          numericInput(
            inputId = ns("numInMin"),
            label = NULL,
            min = min,
            max = max,
            step = (max - min) / 20,
            value = value[1]
          )
        ),
        column(
          width = 6,
          numericInput(
            inputId = ns("numInMax"),
            label = NULL,
            min = min,
            max = max,
            step = (max - min) / 20,
            value = value[2]
          )
        )
      )
    )
  }

#' Slider And Input Server
#'
#' Server function of the Slider And Input module
#' @param id id of module
#' @param min (numeric) minumum
#' @param max (numeric) maximum
#' @param value (numeric) default value
sliderAndNumericRangeServer <- function(id,
                                        value,
                                        min,
                                        max) {
  moduleServer(id,
               function(input, output, session) {
                 result <- reactiveVal(c(0, 1))

                 observeEvent(list(value(), min(), max()), {
                   req(value(), min(), max())
                   updateSliderInput(
                     session = session,
                     "sliderIn",
                     value = value(),
                     min = min(),
                     max = max()
                   )

                   updateNumericInput(
                     session = session,
                     "numInMin",
                     value = value()[1],
                     min = min(),
                     max = max(),
                     step = (max() - min()) / 20,
                   )

                   updateNumericInput(
                     session = session,
                     "numInMax",
                     value = value()[2],
                     min = min(),
                     max = max(),
                     step = (max() - min()) / 20,
                   )

                   result(value())
                 })

                 observeEvent(input$sliderIn, {
                   req(
                     !identical(input$sliderIn[1], input$numInMin) ||
                       !identical(input$sliderIn[2], input$numInMax)
                   )
                   updateNumericInput(session = session,
                                      "numInMin",
                                      value = input$sliderIn[1])
                   updateNumericInput(session = session,
                                      "numInMax",
                                      value = input$sliderIn[2])
                   result(input$sliderIn)
                 })

                 observeEvent(input$numInMin, {
                   req(input$numInMin,
                       !identical(input$sliderIn[1], input$numInMin))
                   updateSliderInput(
                     session = session,
                     "sliderIn",
                     value = c(input$numInMin, input$numInMax)
                   )
                   result(c(input$numInMin, input$numInMax))
                 })

                 observeEvent(input$numInMax, {
                   req(input$numInMax,
                       !identical(input$sliderIn[2], input$numInMax))
                   updateSliderInput(
                     session = session,
                     "sliderIn",
                     value = c(input$numInMin, input$numInMax)
                   )
                   result(c(input$numInMin, input$numInMax))
                 })

                 return(result)
               })
}
