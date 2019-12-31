appStarPlotUI <- function(id, live = T,reset=T,all=T) {
  source("./www/code/myicon.R")
  ns <- NS(id)
  if (isTRUE(live)) {
    if (isTRUE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("square",class="far"), style = "bordered"),
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-square",class="far"), style = "bordered")
          ),
          plotlyOutput(ns("appStarPlot"),width = "100%",inline = T)
          
        )
      )
    } else if (isTRUE(reset) & isFALSE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("square",class="far"), style = "bordered"),
          ),
          plotlyOutput(ns("appStarPlot"),width = "100%",inline = T)
          
        )
      )
    } else if (isFALSE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-square",class="far"), style = "bordered")
          ),
          plotlyOutput(ns("appStarPlot"),width = "100%",inline = T)
          
        )
      )
    }
  } else {
    if (isTRUE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("square",class="far"), style = "bordered"),
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-square",class="far"), style = "bordered")
          ),
          plotOutput(ns("appStarPlot"),width = "100%",inline = F,height = 500)
          
        )
      )
    } else if (isTRUE(reset) & isFALSE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("square",class="far"), style = "bordered"),
          ),
          plotOutput(ns("appStarPlot"),width = "100%",inline = F,height = 500)
          
        )
      )
    } else if (isFALSE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-square",class="far"), style = "bordered")
          ),
          plotOutput(ns("appStarPlot"),width = "100%",inline = F,height = 500)
          
        )
      )
    }
  }
}