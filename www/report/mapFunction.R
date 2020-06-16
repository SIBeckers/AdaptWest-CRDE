mapFunction<-function(opacity=0.75,
                      OSM=F,
                      width=NULL,
                      height=NULL,baseMap="Esri Relief") {
  m <- leaflet(width=width,height=height,
    options = leafletOptions(
      minZoom = minZoom,
      maxZoom = maxZoom,
      worldCopyJump = T,
      preferCanvas = F,
      wheelDebounceTime = 100
    )
  ) %>%
    addProviderTiles(
      provider = providers$Esri.WorldShadedRelief ,
      layerId = "EsriWorldRelief", 
      group = "ESRI Relief", 
      options = tileOptions(
        zIndex = 0.0,
        minZoom = minZoom,
        maxZoom = maxZoom,
        opacity = 1
      )
    ) %>% 
    addProviderTiles(
      provider = providers$Esri.WorldTerrain, 
      layerId = "EsriWorldTerrain", 
      group = "ESRI Terrain", 
      options = tileOptions(
        zIndex = 0.0,
        minZoom = minZoom,
        maxZoom = maxZoom,
        opacity = 1
      )
    ) %>% 
    addProviderTiles(
      provider = "Stamen.TonerLabels",
      group = "Place labels",
      layerId = "Stamen Labels",
      options = tileOptions(
        zIndex = 2,
        minZoom = minZoom,
        maxZoom = maxZoom,
        opacity = 1
      )
    ) %>%
    addProviderTiles(
      provider = "Stamen.TonerLite",
      group="Stamen Street Map",
      layerId="StamenStreetMap",
      options = tileOptions(
        zIndex = 0,
        minZoom = minZoom,
        maxZoom = maxZoom,
        opacity = 1
      )
    ) %>%
    addProviderTiles(
      provider = "Esri.WorldStreetMap",
      group = "ESRI Street Map",
      layerId = "ESRIStreetMap",
      options = tileOptions(
        zIndex = 0,
        minZoom = minZoom,
        maxZoom = maxZoom,
        opacity = 1
      )
    ) %>%
    addMouseCoordinates() %>%
    addEasyButton(
      easyButton(id = "global",
                 icon = icon("globe", class = "fa-2x", lib = "font-awesome"),
                 title = "Global Extent",
                 onClick = JS("function(btn, map){map.setView(L.latLng(55,-100),3); }")
      )
    ) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(maxWidth = 250, imperial = T)
    ) %>%
    addLayersControl(
      position = c("topleft"),
      baseGroups = c("ESRI Relief","ESRI Terrain","ESRI Street Map","Stamen Street Map"),
      overlayGroups = c("Place labels"),
      options = layersControlOptions(collapsed = T, autoZIndex = F)
    ) %>%
    hideGroup("ESRI Imagery") %>% 
    showGroup(baseMap) %>%
    addMiniMap(
      minimized = F,
      toggleDisplay = T,
      collapsedWidth = 30,
      collapsedHeight = 30,
      position = "bottomleft"
    ) %>% 
    mapOptions(zoomToLimits = "first") %>%
    # clearBounds() %>%
    setView(lng = -124.6676, lat = 56.63539, zoom = 4)
  if (isTRUE(OSM)) {
    m <- m %>%
      addProviderTiles(
        provider = providers$OpenStreetMap.Mapnik,
        layerId = "Open Street Map", 
        group = "Open Street Map", 
        options = tileOptions(
          zIndex = 0,
          minZoom = minZoom,
          maxZoom = maxZoom,
          opacity = 1
        )
      ) %>% 
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("ESRI Relief","ESRI Terrain","Open Street Map"),
        overlayGroups = c("Place labels"),
        options = layersControlOptions(collapsed = T, autoZIndex = F)
      ) %>%
      hideGroup("Place labels")
  } else {
    m <- m
  }
  return(m)
}