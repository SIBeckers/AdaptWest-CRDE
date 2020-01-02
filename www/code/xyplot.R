xyplot <- function(data,xvar, yvar) {
  library(ggplot2)
  library(colormap)
  cols <- colors2d(data[,1:2],c("blue","yellow","red","green"))
  p <- ggplot(data) +
    geom_point(aes_string(x = xvar,y =yvar),fill=cols,color=cols,size=3)
  
  return(p) 
  
}