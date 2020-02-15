xyPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "xyplotdiv",
      fluidRow(
        plotOutput(ns("xyPlot"),width = "50%")
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("X"),
          label = "X-variable",
          choices = c("Select X axis: " = "", metriclist),
          selected = "elevdiv",
          multiple  = F,
          width = "auto",
          options = list(maxOptions = 12)
        ),
        selectizeInput(
          inputId = ns("Y"),
          label = "Y-variable",
          choices = c("Select Y axis: " = "", metriclist),
          selected = "intact",
          multiple  = F,
          width = "auto",
          options = list(maxOptions = 12)
        )
      )
    )
  )
    
}