tourStep <- function(mapid = "", tourinfo,tourid,rSwipe,
                     shpdata = NULL,opac = NULL, OSM = F,showAll = F){
  if(isTRUE(showAll)){
    if(tourid <=2) {
      setup <- tourinfo
      shp <- shpdata
    } else {
      setup <- tourinfo[tourId == tourid,]
      shp <- shpdata[which(shpdata$TOURID == setup$polygon),]
    }
  } else {
      setup <- tourinfo[tourId == tourid,]
      shp <- shpdata[which(shpdata$TOURID == setup$polygon),]
  }
  bds <- unname(st_bbox(shp))
  isSwipe = NULL
  if (isFALSE(setup$zoomTo)) {
    view=c(-122.8271,55.71267,5)
  } else {
    view=NULL
  }
  if (isTRUE(setup$swipe)) {
     isSwipe = TRUE
     if (isTRUE(OSM)) {
      callModule(map, mapid, swipe = T,
                layer1 = paste0("'", setup$tile1url,"'"),
                layer2 = paste0("'", setup$tile2url,"'"),
                opacity = opac, OSM = T,view=view)
     } else {
       callModule(map, mapid, swipe = T,
                  layer1 = paste0("'", setup$tile1url,"'"),
                  layer2 = paste0("'", setup$tile2url,"'"),
                  opacity = opac, OSM = F,view=view)
     }
     proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
     proxy %>% 
       clearGroup("metrics")
    isSwipe = TRUE
  } else if (isFALSE(setup$swipe)) {
    if (isFALSE(isolate(rSwipe))) {
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
        isSwipe = FALSE
        if (isTRUE(OSM)) {
          callModule(map, mapid, swipe = F,OSM = T,view=view)
        } else {
          callModule(map, mapid, swipe = F,view=view)
        }
        proxy <- leafletProxy(mapId = paste0(mapid,"-map"))
          proxy %>%
            clearGroup("metrics")
          proxy %>%
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
      } else if (is.na(setup$tile1)) {
        if (isTRUE(OSM)) {
          callModule(map, mapid, swipe = F,OSM = T,view=view)
        } else {callModule(map, mapid, swipe = F,view=view)}
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
        layerId = paste0("shp",shp$TOURID),
        fillOpacity = 0.0,
        smoothFactor = 2.0
      )
  if (isTRUE(setup$zoomTo)) {
    proxy %>% flyToBounds(lng1 = bds[1],lat1 = bds[2],lng2 = bds[3],lat2 = bds[4])
  }
  return(isSwipe) 
}