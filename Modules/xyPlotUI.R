xyPlotUI <- function(id,reset=T,all=T,pa=F) {
  ns <- NS(id)
  if (isTRUE(reset) & isTRUE(all)) {
    tagList(
      div(class = "xyPlot",
        fluidRow(
          actionBttn(ns("resetSPxy"), label = "Clear All", icon = icon("times-circle",class="far"), style = "jelly",
                     color="warning"),
          actionBttn(ns("getallSPxy"), label = "Select All", icon = icon("check-circle",class="far"), style = "jelly",
                     color="success")
        ),
        br(),
        fluidRow(
          if(isTRUE(pa))
            h5("Use the dialog below to generate a scatter plot that compares different
                climate resilience metrics for the selected region(s) and the biome as a whole."
            )
          else{
            h5("Use the dialog below to generate a scatter plot that compares different
                climate resilience metrics for the selected region(s) and the ecoregion as a whole."
            )
          }
          ,style="padding-left:15px; padding-right: 15px;"
        ),
        br(),
        fluidRow(
          plotOutput(ns("xyPlot"),width = "100%")
        ),
        fluidRow(id=ns("xyBttnRow"),
          selectizeInput(
            inputId = ns("X"),
            label = "X-variable",
            choices = c("Select X axis: " = "", metriclist[1:8]),
            selected = "elevdiv",
            multiple  = F,
            width = "auto",
            options = list(maxOptions = 12)
          ),
          selectizeInput(
            inputId = ns("Y"),
            label = "Y-variable",
            choices = c("Select Y axis: " = "", metriclist[1:8]),
            selected = "intact",
            multiple  = F,
            width = "auto",
            options = list(maxOptions = 12)
          )
        )
      )
    )
  } else if (isTRUE(reset) & isFALSE(all)) {
    tagList(
      div(class = "xyPlot",
        fluidRow(
          actionBttn(ns("resetSPxy"), label = "Clear All", icon = icon("times-circle",class="far"), style = "jelly",
                     color="warning"),
        ),
        br(),
        fluidRow(
          if(isTRUE(pa))
            h5("Use the dialog below to generate a scatter plot that compares different
                climate resilience metrics for the selected region(s) and the biome as a whole."
            )
          else{
            h5("Use the dialog below to generate a scatter plot that compares different
                climate resilience metrics for the selected region(s) and the ecoregion as a whole."
            )
          }
          ,style="padding-left:15px; padding-right: 15px;"
        ),
        br(),
        fluidRow(
          plotOutput(ns("xyPlot"),width = "100%")
        ),
        fluidRow(id=ns("xyBttnRow"),
          selectizeInput(
            inputId = ns("X"),
            label = "X-variable",
            choices = c("Select X axis: " = "", metriclist[1:8]),
            selected = "elevdiv",
            multiple  = F,
            width = "auto",
            options = list(maxOptions = 12)
          ),
          selectizeInput(
            inputId = ns("Y"),
            label = "Y-variable",
            choices = c("Select Y axis: " = "", metriclist[1:8]),
            selected = "intact",
            multiple  = F,
            width = "auto",
            options = list(maxOptions = 12)
          )
        )
      )
    )
  } else if (isFALSE(reset) & isTRUE(all)) {
    tagList(
      div(class = "xyPlot",
          fluidRow(
            actionBttn(ns("getallSPxy"), label = "Select All", icon = icon("check-circle",class="far"), style = "jelly",
                       color="success")
          ),
          br(),
          fluidRow(
            if(isTRUE(pa))
              h5("Use the dialog below to generate a scatter plot that compares different
                climate resilience metrics for the selected region(s) and the biome as a whole."
              )
            else{
              h5("Use the dialog below to generate a scatter plot that compares different
                climate resilience metrics for the selected region(s) and the ecoregion as a whole."
              )
            }
            ,style="padding-left:15px; padding-right: 15px;"
          ),
          br(),
          fluidRow(
            plotOutput(ns("xyPlot"),width = "100%")
          ),
          fluidRow(id=ns("xyBttnRow"),
                   selectizeInput(
                     inputId = ns("X"),
                     label = "X-variable",
                     choices = c("Select X axis: " = "", metriclist[1:8]),
                     selected = "elevdiv",
                     multiple  = F,
                     width = "auto",
                     options = list(maxOptions = 12)
                   ),
                   selectizeInput(
                     inputId = ns("Y"),
                     label = "Y-variable",
                     choices = c("Select Y axis: " = "", metriclist[1:8]),
                     selected = "intact",
                     multiple  = F,
                     width = "auto",
                     options = list(maxOptions = 12)
                   )
          )
      )
    )
  } else if (isFALSE(reset) & isFALSE(all)) {
    tagList(
      div(class = "xyPlot",
        br(),
        fluidRow(
          h5("Use the dialog below to generate a scatter plot that compares different
            climate resilience metrics for the selected region(s) and the ecoregion as a whole."
          ),
          style="padding-left:15px; padding-right: 15px;"
        ),
        br(),
        fluidRow(
          plotOutput(ns("xyPlot"),width = "100%")
        ),
        fluidRow(id=ns("xyBttnRow"),
          selectizeInput(
            inputId = ns("X"),
            label = "X-variable",
            choices = c("Select X axis: " = "", metriclist[1:8]),
            selected = "fwvelref",
            multiple  = F,
            width = "auto",
            options = list(maxOptions = 12)
          ),
          selectizeInput(
            inputId = ns("Y"),
            label = "Y-variable",
            choices = c("Select Y axis: " = "", metriclist[1:8]),
            selected = "bwvelref",
            multiple  = F,
            width = "auto",
            options = list(maxOptions = 12)
          )
        )
      )
    )
  }
}