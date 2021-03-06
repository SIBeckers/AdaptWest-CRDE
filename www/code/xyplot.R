xyplot <- function(data,data2,xvar, yvar,xvarname,yvarname,nam,offset=0,pa=F,live=T,word=F) {
  ggplotColours <- function(n = nrow(data2), h = c(0, 360)+15) {
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }
  if(isFALSE(live)){
    if (is.null(data2) | nrow(data2)<1){
      if(isFALSE(pa)){
        data[[nam]]<-" Surrounding Ecoregion "
        greys="grey"
      } else {
        data[[nam]]<-" Surrounding Biome(s) "
        greys="grey"
      }
      if(isFALSE(word)){
        p<- ggplot()+
          geom_point(data=data,aes_string(x=xvar,y=yvar,color=nam),show.legend=T,size=3)+#scale_color_manual(values=greys,name="Legend")+
          xlab(xvarname)+
          ylab(yvarname)+
          theme_bw()
      } else {
        p<- ggplot()+
          geom_point(data=data,aes_string(x=xvar,y=yvar,color=nam),show.legend=T,size=1)+#scale_color_manual(values=greys,name="Legend")+
          xlab(xvarname)+
          ylab(yvarname)+
          theme_bw()
      }
    } else if (!is.null(data2)) {
      col2 <- ggplotColours(n=(nrow(data2)+offset))[(1+offset):(offset+nrow(data2))]
      if(isFALSE(pa)){
        data[[nam]]<-" Surrounding Ecoregion "
        greys="grey"
      } else {
        data[[nam]]<-" Surrounding Biome(s) "
        greys="grey"
      }
      data<-bind_rows(data,data2)
      if(isFALSE(word)){
        p<- ggplot()+
          geom_point(data=data,aes_string(x=xvar,y=yvar,color=nam),show.legend=T,size=3)+scale_color_manual(values=c(greys,col2),name="Legend")+
          xlab(xvarname)+
          ylab(yvarname)+
          theme_bw()
      } else {
        p<- ggplot()+
          geom_point(data=data,aes_string(x=xvar,y=yvar,color=nam),show.legend=T,size=1)+scale_color_manual(values=c(greys,col2),name="Legend")+
          xlab(xvarname)+
          ylab(yvarname)+
          theme_bw()
      }
    }
  } else {
    if (is.null(data2) || nrow(data2)<1) {
      if(isFALSE(pa)){
        data$Group<-" Surrounding Ecoregion "
        greys="grey"
      } else {
        data$Group<-" Surrounding Biome(s) "
        greys="grey"
      }
      fig <- plot_ly()
      p <- fig %>% plotly::add_markers(
        data = data, x =~get(xvar),y = ~get(yvar) ,color = ~Group, 
        colors=c("grey"),legendgroup= ~Group,
        text = ~I(get(nam)),
        size=10,
        hovertemplate = paste(
          '<b>Name</b>: %{text}<br>',
          '<b>Forward Climate Refugia</b>: %{y:.3f}<br>',
          '<b>Backward Climate Refugia</b>: %{x:.3f}<br>'
        )
      ) %>% plotly::layout(xaxis = list(title = xvarname),yaxis=list(title=yvarname),
                           legend = list(orientation = 'h',x = 0, y = -0.25,yanchor="top",
                                         tracegroupgap=1,font=list(size="0.5em"))
      )
    } else if (!is.null(data2)) {
      col2 <- ggplotColours(n=(nrow(data2)+offset))[(1+offset):(offset+nrow(data2))]
      if(isFALSE(pa)){
        data$Group<-" Surrounding Ecoregion "
        greys="grey"
      } else {
        data$Group<-" Surrounding Biome(s) "
        greys="grey"
      }
      data2$Group<-data2[[nam]]
      data<-bind_rows(data,data2)
      data$Group<- factor(data$Group, levels = c(as.character(unique(data$Group))))
      fig <- plot_ly()
      p <- fig %>% plotly::add_markers(
        data = data, x = ~get(xvar),y = ~get(yvar),color = ~Group, 
        colors=c("grey",col2),legendgroup=~Group,
        text = ~I(get(nam)),
        size=10,
        hovertemplate = paste(
          '<b>Name</b>: %{text}<br>',
          '<b>Forward Climate Refugia</b>: %{y:.3f}<br>',
          '<b>Backward Climate Refugia</b>: %{x:.3f}<br>'
        ) 
      ) %>% plotly::layout(xaxis = list(title = xvarname),yaxis=list(title=yvarname),
                           legend = list(orientation = 'h',x = 0, y = -0.25,yanchor="top",
                                         tracegroupgap=1,font=list(size="0.5em"))
      ) 
    }
  }
  return(p) 
}