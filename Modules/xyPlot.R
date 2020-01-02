xyPlot <- function(input, output, session,data,namecol){
  source("./www/code/xyplot.R")
  # library(dplyr)
  xvar <- reactiveVal(input$"X")
  yvar <- reactiveVal(input$"Y")
  print(xvar())
  print(yvar())
  
  if (is.null(data) | is.null(xvar()) | is.null(yvar())) {
    return()
  } else if (xvar() != yvar()){
    print(data)
    data <- data %>% select(starts_with(xvar()),starts_with(yvar()),namecol) %>% mutate_at(.vars=1:2,funs(as.numeric)) %>% st_drop_geometry()
    print(data)
    sctplot <- xyplot(data = data, xvar = xvar(), yvar = yvar())
    output$xyPlot <- renderPlot(sctplot)
  } else{
    return()
  }
  
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