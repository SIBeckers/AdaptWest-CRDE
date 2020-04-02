legendFunc(map,varname,opacity){
  proxy <- leafletProxy(mapId=map)
  if(!(varname %in% c("fwshpath","bwshpath"))){
    proxy %>% addLegend(
      position="bottomright",
      pal=colorNumeric(palette="RdYlBu",domain=c(0,100),na.color=NA,reverse=T),
      values = c(0,100),
      bins=5,
      opacity=opacity,
      title="Quantile",
      layerId="legend"
    )
  }
  
}