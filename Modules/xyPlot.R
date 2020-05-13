xyPlot <- function(input, output, session,data,data2,namecol,offset=0,pa=F,live=T){
  xvar <- reactiveVal(input$"X")
  yvar <- reactiveVal(input$"Y")
  setBookmarkExclude(c("X","Y","getallSPxy","resetSPxy"))
  metriclist<-metriclist[1:8]
  if(is.null(xvar())){
    xvar("fwvelref")
  } else {
    xvar(input$"X")
  }
  if(is.null(yvar())){
    yvar("bwvelref")
  } else {
    yvar(input$"Y")
  }
  xvarname<-names(metriclist[which(metriclist==xvar())])
  yvarname<-names(metriclist[which(metriclist==yvar())])

  if (is.null(data) | is.null(xvar()) | is.null(yvar())) {
    return()
  } else if (!is.null(data) & !is.null(data2) & xvar() != yvar()) {
    data <- data %>% st_drop_geometry()
    data2<- data2 %>% st_drop_geometry()
    sctplot <- xyplot(data = data, data2=data2, xvar = xvar(), yvar = yvar(),xvarname=xvarname,yvarname=yvarname,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else if (!is.null(data) & !is.null(data2) & xvar() == yvar()) {
    data <- data %>% st_drop_geometry()
    data2<- data2 %>% st_drop_geometry()
    sctplot <- xyplot(data = data,data2 = data2, xvar = xvar(), yvar = xvar(),xvarname=xvarname,yvarname=xvarname,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else if (!is.null(data) & is.null(data2) & xvar() != yvar()) {
    data <- data %>% st_drop_geometry()
    sctplot <- xyplot(data = data, xvar = xvar(), yvar = yvar(),xvarname=xvarname,yvarname=yvarname,data2=NULL,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else if (!is.null(data) & is.null(data2) & xvar() == yvar()) {
    data <- data %>% st_drop_geometry()
    sctplot <- xyplot(data = data, xvar = xvar(), yvar = xvar(),xvarname=xvarname,yvarname=xvarname,data2=NULL,nam=namecol,offset=offset,pa=pa,live=live)
    if(isTRUE(live)){
      output$xyPlot <- renderPlotly(sctplot)
    } else {output$xyPlot <- renderPlot(sctplot)}
  } else {
    return()
  }
}