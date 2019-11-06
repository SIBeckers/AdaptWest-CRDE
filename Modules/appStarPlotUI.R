appStarPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class="appStarPlot",
      plotlyOutput(ns("appStarPlot"),width="100%",inline=F)
    )
  )
}