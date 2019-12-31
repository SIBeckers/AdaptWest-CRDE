xyPlot <- function(input, output, session,data){
  source("./www/code/xyplot.R")
  # library(dplyr)
  xvar <- reactiveVal(input$climExpX)
  yvar <- reactiveVal(input$climExpY)
  print(xvar())
  print(yvar())
  
  # if (is.null(data)) {
  #   return()
  # } else {
    # print(data)
  data <- data %>% select(starts_with(xvar()),starts_with(yvar())) %>% st_drop_geometry() %>% mutate_all(funs(as.numeric))
  print(data)
  sctplot <- xyPlot(data = data, xvar = xvar(), yvar = yvar())
  output$xyPlot <- renderPlot(sctplot)
  # }
  
  # output$hoverInfo <- renderPrint({
  #   if(!is.null(input$plot_hover)){
  #     hover=input$plot_hover
  #     dist=sqrt((hover$x-mtcars$mpg)^2+(hover$y-mtcars$disp)^2)
  #     cat("Weight (lb/1000)\n")
  #     if(min(dist) < 3)
  #       mtcars$wt[which.min(dist)]
  #   }
  # })
}