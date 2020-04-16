radarplot <- function(data,namecol = "", removecols = NULL, interactive = T,offset=0,word=F) {
  ggplotColours <- function(n = nrow(data), h = c(0, 360)+15) {
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }
  if ("sf" %in% class(data)) {
    data <- st_drop_geometry(data)
  }
  if (!is.null(removecols)) {
  data <- data %>%
  remove_rownames() %>%
  column_to_rownames(var = namecol) %>%
  as_tibble(rownames = namecol) %>%
  select(-removecols)
  }
  data[[namecol]]<-str_wrap(data[[namecol]],35)
  if (isFALSE(interactive)) {
    mname <- sym(namecol)
    data <- data %>% mutate_at(vars(-namecol), rescale,na.rm=F) %>% rownames_to_column("ROWID") %>%
      filter(!!mname != "MIN" & !!mname != "MAX") %>% column_to_rownames(var = namecol) %>% 
      as_tibble(rownames = namecol)
    
    p <- ggRadar(data = data,aes(colour = ROWID),
                na.rm = F,
                rescale = F,
                legend.position = "none",
                alpha = 0,
                use.label = F,
                interactive = F,show.legend=F)+
      annotate("text", x = 1.5,
               y = seq(0, 1, 0.25),
               label = seq(0, 1, 0.25),size=rel(3)) +
      scale_y_continuous(breaks=seq(0, 1, 0.25),labels=seq(0, 1, 0.25),limits=c(-0.1,1),minor_breaks = seq(0,1,0.25))+
      theme_bw()+
      guides(color = guide_legend(title.position="top",ncol=3,byrow=T))+
      scale_fill_manual(values=ggplotColours(n=length(data$ROWID)),labels=data[[namecol]],name="Area:",aesthetics = c("color",'fill')) +
      theme(
        aspect.ratio=1,
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        panel.border= element_blank(),
        legend.position = "bottom",
        legend.margin = margin(0,10,0,10),
        plot.margin = margin(t=0,r=10,b=0,l=10),
        panel.spacing.x = unit(2,"lines"),
        axis.text.x = element_blank()
      )
    if(isTRUE(word)){
      p <- p+
        annotate("text", x = seq(1,8),
          y = 1.0,
          label = gsub(" ","\n",colnames(data[3:10])),angle=c(0,0,0,0,0,0,0,0),hjust=c(0.5,0.5,0.7,0.35,0.5,0.5,0.5,0.5),size=rel(2),vjust=c(1,1,1,0,1,1,1,1))
    } else{
      p <- p+
        annotate("text", x = seq(1,8),
                 y = 1.0,
                 label = colnames(data[3:10]),angle=c(0,0,0,0,0,0,0,0),hjust=c(0.5,0.5,0.7,0.35,0.5,0.5,0.5,0.5),size=rel(4),vjust=c(1,1,1,0,1,1,1,1))
    }
  } else {
    mname <-sym(namecol)
    data <- data %>% mutate_at(vars(-namecol), rescale,na.rm=F) %>% filter(!!mname != "MIN") %>% filter(!!mname != "MAX")
    names(data)<-gsub(" "," <br> ",names(data))
    names <- unlist(data %>% select(namecol),use.names = F)
    data <- data %>% select(-namecol)
    theta <- c(names(data),names(data)[1])
    col2 <- ggplotColours(n=(nrow(data)+offset))[(1+offset):(offset+nrow(data))]
    p <- plot_ly(
      type = "scatterpolar",
      fill = "none",
      mode = "lines+markers"
    ) %>%
    add_trace(
      type = 'scatterpolar',
      mode = "lines+markers",
      r = round(c(data %>% slice(1) %>% unlist(use.names = F),data %>% select(1) %>% slice(1) %>% unlist(use.names = F)),digits = 4),
      theta = theta,
      name = names[1],
      hoverinfo = "all",
      line=list(color=col2[1]),
      marker=list(color=col2[1])
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1),
          angle = 90,
          side = "clockwise"
        ),
        angularaxis = list(
          direction = "clockwise",
          rotation = 90,
          automargin=T
        )
      ),
      legend = list(
        orientation = "h"
      ),
      margin = list(r=60)
    )
    if(nrow(data)>1){
      for (i in 2:nrow(data)) {
        p <- add_trace(
          p,
          type = "scatterpolar",
          mode = "lines+markers",
          r = round(c(data %>% slice(i) %>% unlist(use.names = F),data %>% select(1) %>% slice(i) %>% unlist(use.names = F)),digits = 4),
          theta = theta,
          name = names[i],
          line=list(color=col2[i]),
          marker=list(color=col2[i])
        )
      }
    }
  }
  return(p)
}
