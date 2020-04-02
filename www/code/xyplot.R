xyplot <- function(data,data2,xvar, yvar,xvarname,yvarname,nam,offset=0,pa=F) {
  ggplotColours <- function(n = nrow(data2), h = c(0, 360)+15) {
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
    cols="grey"
  }
  if (!is.null(data2)) {
    fcol <- rep(NA,nrow(data2))
    col2 <- ggplotColours(n=(nrow(data2)+offset))[(1+offset):(offset+nrow(data2))]
    if(isFALSE(pa)){
      data[[nam]]<-" EcoRegion "
    } else {data[[nam]]<-"Biome"}
    data<-rbind(data,data2)
    p<- ggplot()+
      geom_point(data=data,aes_string(x=xvar,y=yvar,color=nam),show.legend=T,size=3)+scale_color_manual(values=c("grey",col2),name="Legend")+
      xlab(xvarname)+
      ylab(yvarname)+
      theme_bw()
  } else {
    if(isFALSE(pa)){
      data[[nam]]<-" EcoRegion "
    } else {data[[nam]]<-"Biome"}
    data[[nam]]<-"EcoRegion"
    p<- ggplot()+
      geom_point(data=data,aes_string(x=xvar,y=yvar,color=nam),show.legend=T,size=3)+scale_color_manual(values=c("grey"),name="Legend")+
      xlab(xvarname)+
      ylab(yvarname)+
      theme_bw()
  }
  return(p) 
}