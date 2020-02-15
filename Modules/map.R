#map.R
map <- function(input, output, session, swipe = F, mapbox = F, 
                layer1 = "'http://{s}.tile.osm.org/{z}/{x}/{y}.png'",
                layer2 = "'https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.png'",
                opacity=0.45,
                OSM=F){
  setBookmarkExclude(
    c("bookmarkBttn","tour-nextBttn","tour-prevBttn","tour-startBttn","tour-stopBttn","tour-endBttn")
  )
  observeEvent(input$bookmarkBttn, {
    session$doBookmark()
  })
  if (isFALSE(mapbox)) {
    if (isFALSE(swipe)) {
      map <- function() {
        m <- leaflet(
          options = leafletOptions(
            minZoom = minZoom,
            maxZoom = maxZoom,
            worldCopyJump = T,
            preferCanvas = F,
            wheelDebounceTime = 100
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
     map <- function() {
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
        registerPlugin(LeafletSideBySidePlugin) %>%
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
          mapOptions(zoomToLimits = "first")%>%
          # clearBounds() %>%
          setView(lng = -124.6676, lat = 56.63539, zoom = 4)
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
  } else {
    
    map <- function(){
      m <- leaflet(
        options = leafletOptions(
          minZoom = minZoom,
          maxZoom = maxZoom,
          worldCopyJump = T,
          preferCanvas = F
        )
      ) 
      mapOptions(zoomToLimits = "first") %>%
        # clearBounds() %>%
      setView(lng = -120, lat = 55, zoom = 4)
    }
  }
  
  if (isTRUE(mapbox)) {
    output$map <- renderLeaflet({map()})
  } else {
  output$map <- renderLeaflet({map()})
  }
}