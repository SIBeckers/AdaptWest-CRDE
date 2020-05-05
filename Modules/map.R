#map.R
map <- function(input, output, session, swipe = F, 
                layer1 = "'http://{s}.tile.osm.org/{z}/{x}/{y}.png'",
                layer2 = "'https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.png'",
                opacity=0.45,
                OSM=F,
                view = c(-124.6676, 56.63539, 4)){
  setBookmarkExclude(c("map_shape_click","map_groups","map_center",
                       "map_shape_mouseover","map_shape_mouseout","map_click",
                       "map_inputs_state","map_zoom","map_bounds"))
  if (isFALSE(swipe)) {
    mymap <- function() {
      m <- leaflet(
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
          opacity = 0.8
        )
      ) %>%
      addMouseCoordinates() %>%
      addEasyButton(
        easyButton(
          id = "global",
          icon = icon("globe", class = "fa-3x", lib = "font-awesome"),
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
        baseGroups = c("ESRI Relief","ESRI Terrain"),
        overlayGroups = c("Place labels"),
        options = layersControlOptions(collapsed = T, autoZIndex = F)
      ) %>%
      hideGroup("ESRI Imagery") %>% 
      addMiniMap(
        minimized = T,
        toggleDisplay = T,
        collapsedWidth = 30,
        collapsedHeight = 30,
        position = "bottomleft"
      ) %>% 
      mapOptions(zoomToLimits = "first")
      if(!is.null(view)){
        m <- m %>% 
          setView(lng=view[1],view[2],zoom=view[3])
      } else {
        m <- m
      }
      if (isTRUE(OSM)) {
        m <- m %>%
          addProviderTiles(
            provider = providers$OpenStreetMap.Mapnik,
            layerId = "Open Street Map", 
            group = "Open Street Map", 
            options = tileOptions(
              zIndex = 2.0,
              minZoom = minZoom,
              maxZoom = maxZoom,
              opacity = 1
            )
          ) %>% 
          addLayersControl(
            position = c("topleft"),
            baseGroups = c("ESRI Relief","ESRI Terrain"),
            overlayGroups = c("Place labels","Open Street Map"),
            options = layersControlOptions(collapsed = T, autoZIndex = F)
          ) %>%
          hideGroup("Place labels")
      } else {
        m <- m
      }
    }
  } else {
    mymap <- function() {
      m <- leaflet(
        options = leafletOptions(
          minZoom = minZoom,
          maxZoom = maxZoom,
          worldCopyJump = T,
          preferCanvas = F
        )
      ) %>%
      # mapOptions(zoomToLimits = "always") %>%
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
        layerId = "OSM Mapnik",
        options = tileOptions(
          zIndex = 2,
          minZoom = minZoom,
          maxZoom = maxZoom,
          opacity = 0.8
        )
      ) %>%
      addMouseCoordinates() %>%
      addEasyButton(
        easyButton(id = "global",
                   icon = icon("globe", class = "fa-3x", lib = "font-awesome"),
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
        baseGroups = c("ESRI Relief","ESRI Terrain"),
        overlayGroups = c("Place labels"),
        options = layersControlOptions(collapsed = T, autoZIndex = F)
      ) %>%
      hideGroup("ESRI Imagery") %>% 
      addMiniMap(
        minimized = T,
        toggleDisplay = T,
        collapsedWidth = 30,
        collapsedHeight = 30,
        position = "bottomleft"
      ) %>%
      registerPlugin(myLeafletSideBySidePlugin) %>%
      onRender(paste0("function(el,x) {
          var mylayer1 = L.tileLayer(",
                 layer1,
                 ",{maxzoom:",maxZoom,", opacity: ",opacity,", zIndex: 10, tms: true",
                 "}).addTo(this)
          var mylayer2 = L.tileLayer(",
              layer2,
              ",{maxZoom:",maxZoom,", opacity: ",opacity, ", zIndex: 10, tms: true",
              "}).addTo(this)
        L.control.sideBySide(mylayer1, mylayer2).addTo(this);}"
      )) %>%
      mapOptions(zoomToLimits = "first")
      if(!is.null(view)){
        m <- m %>% 
          setView(lng=view[1],view[2],zoom=view[3],options=list(animate=F))
      } else {
        m <- m
      }
      if (isTRUE(OSM)) {
        m <- m %>%
          addProviderTiles(
            provider = providers$OpenStreetMap.Mapnik,
            layerId = "Open Street Map", 
            group = "Open Street Map", 
            options = tileOptions(
              zIndex = 2.0,
              minZoom = minZoom,
              maxZoom = maxZoom,
              opacity = 1
            )
          ) %>% 
          addLayersControl(
            position = c("topleft"),
            baseGroups = c("ESRI Relief","ESRI Terrain"),
            overlayGroups = c("Place labels","Open Street Map"),
            options = layersControlOptions(collapsed = T, autoZIndex = F)
          ) %>%
          hideGroup("Place labels")
      } else {
        m <- m
      }
    }
  }
  
  output$map <- renderLeaflet({mymap()})
}