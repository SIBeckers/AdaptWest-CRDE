appStarPlotUI <- function(id, live = T,reset=T,all=T,height=600) {
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
          br(),
          fluidRow(
            h5("The starplot below integrates multiple climate exposure metrics for 
              an area of interest to produce a composite 'fingerprint'
              representing factors affecting climate resilience, and contrasts between the intensity of different climate 
              exposure stressors."
            ),
            style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          withSpinner(plotlyOutput(ns("appStarPlot"),width = "100%",height=height,inline = F))
        )
      )
    } else if (isTRUE(reset) & isFALSE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("square",class="far"), style = "bordered"),
          ),
          br(),
          fluidRow(
            h5("The starplot below integrates multiple climate exposure metrics for 
              an area of interest to produce a composite 'fingerprint'
              representing factors affecting climate resilience, and contrasts between the intensity of different climate 
              exposure stressors."
            ),
            style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          withSpinner(plotlyOutput(ns("appStarPlot"),width = "100%",height=height,inline = F))
        )
      )
    } else if (isFALSE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-square",class="far"), style = "bordered")
          ),
          br(),
          fluidRow(
            h5("The starplot below integrates multiple climate exposure metrics for 
              an area of interest to produce a composite 'fingerprint'
              representing factors affecting climate resilience, and contrasts between the intensity of different climate 
              exposure stressors."
            ),
            style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          withSpinner(plotlyOutput(ns("appStarPlot"),width = "100%",height=height,inline = F))
        )
      )
    }
  } else {
    if (isTRUE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(id=ns("bttnRow"),
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("times-circle",class="far"), style = "jelly",
                       color="warning"),
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-circle",class="far"), style = "jelly",
                       color="success")
          ),
          br(),
          fluidRow(
            h5("The starplot below integrates multiple climate exposure metrics for 
              an area of interest to produce a composite 'fingerprint'
              representing factors affecting climate resilience, and contrasts between the intensity of different climate 
              exposure stressors."
            ),
            style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          withSpinner(plotOutput(ns("appStarPlot"),width = "100%",inline = F,height = height))
        )
      )
    } else if (isTRUE(reset) & isFALSE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(id=ns("bttnRow"),
            actionBttn(ns("resetSP"), label = "Clear All", icon = icon("times-circle",class="far"), style = "jelly",
                       color="warning")
          ),
          br(),
          fluidRow(
            h5("The starplot below integrates multiple climate exposure metrics for 
              an area of interest to produce a composite 'fingerprint'
              representing factors affecting climate resilience, and contrasts between the intensity of different climate 
              exposure stressors."
            ),
            style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          withSpinner(plotOutput(ns("appStarPlot"),width = "100%",inline = F,height = height))
        )
      )
    } else if (isFALSE(reset) & isTRUE(all)) {
      tagList(
        div(
          class = "appStarPlot",
          fluidRow(id=ns("bttnRow"),
            actionBttn(ns("getallSP"), label = "Select All", icon = icon("check-circle",class="far"), style = "jelly",
                       color="success")
          ),
          br(),
          fluidRow(
            h5("The starplot below integrates multiple climate exposure metrics for 
              an area of interest to produce a composite 'fingerprint'
              representing factors affecting climate resilience, and contrasts between the intensity of different climate 
              exposure stressors."
            ),
            style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          withSpinner(plotOutput(ns("appStarPlot"),width = "100%",inline = F,height = height))
        )
      )
    }
  }
}