radarplot<-function(data,namecol="",removecols=c(1),interactive=T){
  if("sf" %in% class(data)){
    library(dplyr)
    library(tibble)
    library(scales)
    library(sf)
    data <- st_drop_geometry(data)
    data <- data %>%
      remove_rownames() %>%
      column_to_rownames(var=namecol) %>%
      as_tibble(rownames=namecol) %>%
      select(-removecols)
      
  }
  
  if(isFALSE(interactive)){
    library(ggplot2)
    library(ggradar)
    data<-data %>% mutate_at(vars(-namecol), rescale,to=c(0,1))
    p<-ggradar(
      data,
      legend.position="bottom"
    )
  } else {
    library(plotly)
    data<-data %>% mutate_at(vars(-namecol), rescale,to=c(0,100))
    names<-unlist(data %>% select(namecol),use.names=F)
    data<-data %>% select(-namecol)
    theta<-c(names(data),names(data)[1])
    
    p<-plot_ly(
      type="scatterpolar",
      fill="none",
      mode="lines+markers",
      width="100%",
    ) %>%
    add_trace(
      type='scatterpolar',
      mode="lines+markers",
      r=round(c(data %>% slice(1) %>% unlist(use.names=F),data %>% select(1) %>% slice(1) %>% unlist(use.names=F)),digits=1),
      theta=theta,
      name=names[1],
      hoverinfo = "all"
    ) %>%
    layout(
      polar=list(
        radialaxis=list(
          visible=T,
          range=c(0,100),
          angle=90,
          side="clockwise"
        ),
        angularaxis=list(
          direction="clockwise",
          rotation=90
        )
      ),
      legend = list(
        orientation="h"
      ),
      autosize=T,
    
      margin=list(l=0,r=0,b=0,t=100,pad=0)
    )
    for(i in 2:nrow(data)){
      p<-add_trace(
        p,
        type="scatterpolar",
        mode="lines+markers",
        r=round(c(data %>% slice(i) %>% unlist(use.names=F),data %>% select(1) %>% slice(i) %>% unlist(use.names=F)),digits=1),
        theta=theta,
        name=names[i]
      )
    }
  }
  return(p)
}
