appStarPlotUI <- function(id, live = T) {
  ns <- NS(id)
  if (isTRUE(live)) {
    tagList(
      div(
        class = "appStarPlot",
        plotlyOutput(ns("appStarPlot"),width = "25vw",inline = T)
      )
    )
  } else {
    tagList(
      div(
        class = "appStarPlot",
        plotOutput(ns("appStarPlot"),width = "100%",inline = F)
      )
    )
  }
}