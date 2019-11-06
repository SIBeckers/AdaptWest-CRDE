# # XY Plot Function for AdaptWest App
# xyPlot <- function(data,xvar="",yvar="", xlab="",ylab="",title="",caption="",interactive=T,marginPlot=F,fillColor="",strokeColor="") {
#   if(isFALSE(interactive)) {
#     library(ggplot2)
#     library(ggthemes)
#     library(ggExtra)
#     library(ggforce)
#     library(pals)
#     p<-ggplot(data)
#     p<-p+geom_point(aes(x=xvar,y=yvar,shape=21,size=6))+
#       ylab(ylab)+
#       xlab(xlab)+
#       ggtitle(title=title,subtitle=caption)
#       
#   } else {
#     library(plotly)
#     p<-plot_ly(
#       data = data, 
#       x = xvar,
#       y = yvar,
#       marker = list(
#         size=10,
#         color=fillColor,
#         line = list(
#           color = strokeColor,
#           width = 2)
#       )
#     )
#   }
# }