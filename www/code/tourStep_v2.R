tourStep <- function(mapid = "", tourinfo,tourid,rSwipe,view = NULL, shpdata = NULL,opac = NULL, OSM = F){
 
  #Setup a map proxy object that we can use to modify the map.
  setup <- tourinfo[tourId == tourid,]
  
  shp <- shpdata[which(shpdata$TOURID == setup$polygon),]
  # print(shp)
  bds <- unname(st_bbox(shp))
  print(bds)
  
  # print(setup)
  isSwipe = NULL
  if (isTRUE(setup$swipe)) {
    # if (isTRUE(rSwipe)) {
    #   str <- paste0("
    #       var mylayer1 = L.tileLayer(",
    #               paste0("'", setup$tile1url,"'"),
    #               ",{maxzoom:",maxZoom,", opacity: ",opacity,", zIndex: 10, tms: true",
    #               "}).addTo(this)
    #       var mylayer2 = L.tileLayer(",
    #               paste0("'", setup$tile2url,"'"),
    #               ",{maxZoom:",maxZoom,", opacity: ",opacity, ", zIndex: 10, tms: true",
    #               "}).addTo(this)
    #       L.control.sideBySide(mylayer1, mylayer2).addTo(this);}"
    #   )
    #   print("Modifying existing swipeable map")
    #   proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
    #   # # print(str(proxy))
    #   # # print(summary(proxy))
    #   proxy %>%
    #     registerPlugin(LeafletSideBySidePlugin) %>%
    #     shinyjs::runjs(str)
   # isSwipe = TRUE
   # } else {
     print("Creating new swipeable map")
     isSwipe = TRUE
     if (isTRUE(OSM)) {
      callModule(map, mapid, swipe = T,
                layer1 = paste0("'", setup$tile1url,"'"),
                layer2 = paste0("'", setup$tile2url,"'"),
                opacity = opac, OSM = T)
     } else {
       callModule(map, mapid, swipe = T,
                  layer1 = paste0("'", setup$tile1url,"'"),
                  layer2 = paste0("'", setup$tile2url,"'"),
                  opacity = opac, OSM = F)
     }
     proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
     proxy %>% 
       clearGroup("metrics") #%>%
       # flyToBounds(lng1 = bds[1],lat1 = bds[2],lng2 = bds[3],lat2 = bds[4])
     # proxy %>%
     #   clearGroup("metric_polys")
     
    
     isSwipe = TRUE
   # }
  } else if (isFALSE(setup$swipe)) {
    if (isFALSE(isolate(rSwipe))) {
      print("Modifying existing non-swipe map")
      isSwipe = FALSE
      proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
        proxy %>%
          clearGroup("metrics") %>%
          addTiles(
            urlTemplate = setup$tile1url,
            attribution = tilelist$tileAttribution[which(setup$tile1 == tilelist$tileName)],
            group = "metrics",
            layerId = tilelist$tileGroup[which(setup$tile1 == tilelist$tileName)],
            options = tileOptions(
              tms = T,
              minZoom = minZoom,
              maxZoom = maxZoom,
              unloadInvisibleTiles = T,
              noWrap = T,
              opacity = opac,
              zIndex = 9000
            )
          ) 
    } else if (isTRUE(isolate(rSwipe))) {
      if (!is.na(setup$tile1)) {
        print("Creating new non-swipe map")
        isSwipe = FALSE
        if (isTRUE(OSM)) {
          callModule(map, mapid, swipe = F,OSM = T)
        } else {
          callModule(map, mapid, swipe = F)
        }
        proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
          proxy %>%
            clearGroup("metrics")
          proxy %>%
          addTiles(
            urlTemplate = setup$tile1url,
            attribution = tilelist$tileAttribution[which(setup$tile1 == tilelist$tileName)],
            group = "metrics",
            layerId = tilelist$tileGroup[which(setup$tile1 == ilelist$tileName)],
            options = tileOptions(
              tms = T,
              minZoom = minZoom,
              maxZoom = maxZoom,
              unloadInvisibleTiles = T,
              noWrap = T,
              opacity = opac,
              zIndex = 9000
            )
          )
      } else if (is.na(setup$tile1)) {
        
        if (isTRUE(OSM)) {
          callModule(map, mapid, swipe = F,OSM = T)
        } else {callModule(map, mapid, swipe = F)}
      
        isSwipe = FALSE
      }
      
    } 
  }
    proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
    proxy %>%
      clearGroup("metric_polys") %>%
      addPolygons(
        group = "metric_polys",
        data = shp, 
        fillColor = NULL, 
        weight = 2,color = "blue",
        fillOpacity = 0.0
      )
    if (isFALSE(setup$zoomTo)) {
      proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
      proxy %>% setView(-122.8271,55.71267,5)
    } else if (isTRUE(setup$zoomTo)) {
      proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
      proxy %>% flyToBounds(lng1 = bds[1],lat1 = bds[2],lng2 = bds[3],lat2 = bds[4])
    }

  return(isSwipe) 
}