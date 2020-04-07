xyPlot <- function(input, output, session,data,data2,namecol,offset=0,pa=F,live=T){
  source("./www/code/xyplot.R")
  # library(dplyr)
  xvar <- reactiveVal(input$"X")
  yvar <- reactiveVal(input$"Y")
  
  metriclist<-metriclist[1:8]
  xvarname<-names(metriclist[which(metriclist==xvar())])
  yvarname<-names(metriclist[which(metriclist==yvar())])
  if (is.null(data) | is.null(xvar()) | is.null(yvar())) {
    return()
  } else if (!is.null(data) & !is.null(data2) & xvar() != yvar()) {
    # data <- data %>% select(starts_with(xvar()),starts_with(yvar()),namecol) %>% mutate_at(.vars=1:2,funs(as.numeric)) %>% st_drop_geometry()
    data <- data %>% st_drop_geometry()
    data2<- data2 %>% st_drop_geometry()
    # data2 <- data2 %>% select(starts_with(xvar()),starts_with(yvar()),namecol) %>% mutate_at(.vars=1:2,funs(as.numeric)) %>% st_drop_geometry()
    sctplot <- xyplot(data = data, data2=data2, xvar = xvar(), yvar = yvar(),xvarname=xvarname,yvarname=yvarname,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else if (!is.null(data) & !is.null(data2) & xvar() == yvar()) {
    # data <- data %>% select(starts_with(xvar()),namecol) %>% mutate_at(.vars=1,funs(as.numeric)) %>% st_drop_geometry()
    # data2 <- data2 %>% select(starts_with(xvar()),namecol) %>% mutate_at(.vars=1,funs(as.numeric)) %>% st_drop_geometry() 
    data <- data %>% st_drop_geometry()
    data2<- data2 %>% st_drop_geometry()
    sctplot <- xyplot(data = data,data2 = data2, xvar = xvar(), yvar = xvar(),xvarname=xvarname,yvarname=xvarname,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else if (!is.null(data) & is.null(data2) & xvar() != yvar()) {
    # data <- data %>% select(starts_with(xvar()),starts_with(yvar()),namecol) %>% mutate_at(.vars=1:2,funs(as.numeric)) %>% st_drop_geometry()
    data <- data %>% st_drop_geometry()
    sctplot <- xyplot(data = data, xvar = xvar(), yvar = yvar(),xvarname=xvarname,yvarname=yvarname,data2=NULL,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else if (!is.null(data) & is.null(data2) & xvar() == yvar()) {
    # data <- data %>% select(starts_with(xvar()),namecol) %>% mutate_at(.vars=1,funs(as.numeric)) %>% st_drop_geometry()
    data <- data %>% st_drop_geometry()
    sctplot <- xyplot(data = data, xvar = xvar(), yvar = xvar(),xvarname=xvarname,yvarname=xvarname,data2=NULL,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else {
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