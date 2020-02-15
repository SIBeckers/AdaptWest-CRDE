xyplot <- function(data,data2,xvar, yvar,nam) {
  library(ggplot2)
  source("./www/code/colours.R")
  library(viridis)
  ggplotColours <- function(n = nrow(data2), h = c(0, 360)+15) {
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }
  #STILL NEED TO FIX THE COLOURS!
  if (!is.null(data2)) {
    fcol <- rep(NA,nrow(data2))
    col2 <- ggplotColours(n=(nrow(data2)+1))[2:(1+nrow(data2))]
    print(col2)
    print(nrow(data2))
    if (ncol(data) == 3) {
      cols <- colors2d(data[,1:2],c("blue","yellow","red","green"))
      
      p <- ggplot() +
        geom_point(data=data,aes_string(x = xvar,y =yvar),fill=cols,color=cols,size=3)+theme_bw()+
        geom_point(data=data2,aes_string(x=xvar,y=yvar),fill=fcol,color=col2,size=7,stroke=2,pch=21,show.legend=F)
    } else if (ncol(data == 2)) {
      col2 <- col2[1:nrow(data2)]
      p <- ggplot() +
        geom_point(data=data,aes_string(x = xvar,y =yvar,fill=xvar,color=xvar),size=3)+
        scale_fill_viridis(discrete=F)+
        scale_color_viridis(discrete=F)+theme_bw()+
        geom_point(data=data2,aes_string(x=xvar,y=yvar),fill=fcol,color=col2,stroke=2,size=7,pch=21,show.legend=F)
    }
  } else {
    if (ncol(data) == 3) {
      cols <- colors2d(data[,1:2],c("blue","yellow","red","green"))
      p <- ggplot() +
        geom_point(data=data,aes_string(x = xvar,y =yvar),fill=cols,color=cols,size=3,show.legend=F)+theme_bw()
    } else if (ncol(data == 2)) {
      p <- ggplot() +
        geom_point(data=data,aes_string(x = xvar,y =yvar,fill=xvar,color=xvar),size=3,show.legend=F)+
        scale_fill_viridis(discrete=F)+
        scale_color_viridis(discrete=F)+theme_bw()
    }
  }
  
  
  return(p) 
  
}