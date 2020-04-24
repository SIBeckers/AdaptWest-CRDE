TableUI <- function(id,reset=T,all=T,pa=F){
  ns <- NS(id)
  if (isTRUE(reset) & isTRUE(all)) {
    tagList(
      div(
        class = "table",
        fluidRow(
          actionBttn(ns("resetSPtab"), label = "Clear All", icon = icon("times-circle",class="far"), style = "jelly",
                     color="warning"),
          actionBttn(ns("getallSPtab"), label = "Select All", icon = icon("check-circle",class="far"), style = "jelly",color="success")
        ),
        br(),
        withSpinner(tableOutput(ns("table")))
      )
    )
  } else if (isTRUE(reset) & isFALSE(all)) {
    tagList(
      div(
        class = "table",
        fluidRow(
          actionBttn(ns("resetSPtab"), label = "Clear All", icon = icon("times-circle",class="far"), style = "jelly",
                     color="warning"),
          
        ),
        br(),
        withSpinner(tableOutput(ns("table")))
      )
    )
  } else if (isFALSE(reset) & isTRUE(all)) {
    tagList(
      div(
        class = "table",
        fluidRow(
          actionBttn(ns("getallSPtab"), label = "Select All", icon = icon("check-circle",class="far"), style = "jelly",color="success")
          
        ),
        br(),
        withSpinner(tableOutput(ns("table")))
      )
    )
  }
}