radarplot <- function(data,namecol = "", removecols = NULL, interactive = T) {
  library(dplyr)
  library(tibble)
  library(scales)
  library(sf)

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
    library(ggplot2)
    library(ggradar)
    library(ggiraphExtra)
    mname <- sym(namecol)
    data <- data %>% mutate_at(vars(-namecol), rescale) %>% 
      filter(!!mname != "MIN" & !!mname != "MAX") %>% column_to_rownames(var = namecol) %>% 
      as_tibble(rownames = namecol)
    # print(data)
    # p <- ggradar(
    #   data,
    #   legend.position = "bottom",
    #   plot.extent.x.sf = 1.4,
    #   plot.extent.y.sf = 1.2,
    #   base.size = 10,
    #   legend.text.size = 10,
    #   group.point.size = 2,
    #   group.line.width = 1,
    #   axis.label.size = 4,
    # )
    p <- ggRadar(data = data,aes(color = !!mname,group = !!mname),
                na.rm = T,
                rescale = F,
                legend.position = "bottom",
                alpha = 0,
                use.label = F,
                interactive = F)+
      annotate("text", x = 1.5,
               y = seq(0, 1, 0.25),
               label = seq(0, 1, 0.25),size=rel(3)) +
      scale_y_continuous(breaks=seq(0, 1, 0.25),labels=seq(0, 1, 0.25),limits=c(-0.1,1),minor_breaks = seq(0,1,0.25))+
      annotate("text", x = seq(1,8),
               y = 1.0,
               label = colnames(data[2:9]),angle=c(0,0,0,0,0,0,0,0),hjust=c(0.8,0.5,0.75,0.5,0.25,0.25,0.25,0),size=rel(4),vjust=c(1,1,1,0,1,1,1,1))+
      theme_bw()+
      guides(color = guide_legend(title.position="top",ncol=3,byrow=T))+
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
    # p <- ggplotly(p)
  } else {
    library(plotly)
    data <- data %>% mutate_at(vars(-namecol), rescale) %>% filter(NEWNAME != "MIN" & NEWNAME != "MAX")
    names <- unlist(data %>% select(namecol),use.names = F)
    data <- data %>% select(-namecol)
    theta <- c(names(data),names(data)[1])
    
    p <- plot_ly(
      type = "scatterpolar",
      fill = "none",
      mode = "lines+markers",
      width = "100%",
    ) %>%
    add_trace(
      type = 'scatterpolar',
      mode = "lines+markers",
      r = round(c(data %>% slice(1) %>% unlist(use.names = F),data %>% select(1) %>% slice(1) %>% unlist(use.names = F)),digits = 1),
      theta = theta,
      name = names[1],
      hoverinfo = "all"
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
          rotation = 90
        )
      ),
      legend = list(
        orientation = "h"
      ),
      autosize = T,
    
      margin = list(l = 0,r = 0,b = 0,t = 100,pad = 0)
    )
    for (i in 2:nrow(data)) {
      p <- add_trace(
        p,
        type = "scatterpolar",
        mode = "lines+markers",
        r = round(c(data %>% slice(i) %>% unlist(use.names = F),data %>% select(1) %>% slice(i) %>% unlist(use.names = F)),digits = 1),
        theta = theta,
        name = names[i]
      )
    }
  }
  return(p)
}
