#AdaptWestApp server.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 0.1
#Notes
source("global.R")

function(input, output, session) {

  
  #Run once ----  
  observeEvent(input$tabs=="paexpTab",{
    shinyjs::click("paexpacc-0-heading")
    callModule(map, "paexpMap", OSM=F) #Protected Areas Explorer Map
    callModule(ddownBttn,"paexpMapBttn") #Settings button on Protected Areas Explorer Map
  },once = T)  
  
  observeEvent(input$tab=="climexpTab",{
    shinyjs::click("climexpacc-0-heading")
    callModule(map, "climexpMap",OSM=F) # Climate Metrics Explorer Map server logic
    callModule(ddownBttn,"climexpMapBttn") #Drop Down Menu Button Server logic 
  },once = T)
  
  # 5). Home Page Logic ---- 
  observeEvent(input$tabs,{
    if (input$tabs == "homeTab") {
      shinyjs::onclick("climexpimg",  updateNavbarPage(session, inputId = "tabs", selected = "climexpTab"))
      shinyjs::onclick("climtourimg",  updateNavbarPage(session, inputId = "tabs", selected = "climtourTab"))
      shinyjs::onclick("climtourLink",  updateNavbarPage(session, inputId = "tabs", selected = "climtourTab"))
      shinyjs::onclick("climexpLink",  updateNavbarPage(session, inputId = "tabs", selected = "climexpTab"))
      shinyjs::onclick("paexpimg",  updateNavbarPage(session, inputId = "tabs", selected = "paexpTab"))
      shinyjs::onclick("patourimg",  updateNavbarPage(session, inputId = "tabs", selected = "patourTab"))
      shinyjs::onclick("patourLink",  updateNavbarPage(session, inputId = "tabs", selected = "patourTab"))
      shinyjs::onclick("paexpLink",  updateNavbarPage(session, inputId = "tabs", selected = "paexpTab"))
      
    } else if (input$tabs == "climtourTab") {
    # 6). Metric Tour Logic----
      # climtourView <- reactiveValues(view = NULL,opacity = NULL)
      # isSwipemetric <- reactiveVal(NULL)
      y2ytour <- callModule(tourPanel, "y2ytour", tourName = "y2y")
      shinyjs::click("y2ytour-stopBttn")
      callModule(ddownBttn,"y2ymapBttn") #Settings button on Y2Y Tour Map
      callModule(map, "y2ymap", swipe = F,OSM=F)
      proxy<-leafletProxy("y2ymap-map") 
      proxy %>% setView(-122.8271,55.71267,5)
      observeEvent(input$"y2ymap-snapshot",{
        mapshot(proxy,file="AdaptWest-CRDE-CM_Snapshot.png",removecontrols=c("zoomControl","layersControl","homeButton","global"))
      })
      isSwipemetric(F)
      observeEvent(y2ytour$id(),{runjs("window.scrollTo(0,0)")})
      observe({
        if (!is.null(input$"y2ymap-map_bounds")) {
          climtourView$view <- input$"y2ymap-map_bounds"}
        })
      observe({
        if (!is.null(input$"y2ymapBttn-opacity")) {
          climtourView$opacity <- input$"y2ymapBttn-opacity"}
      })
      observe({isSwipemetric(tourStep(mapid = "y2ymap",tourinfo = y2y,tourid = y2ytour$id(),rSwipe = isSwipemetric(),view = isolate(climtourView$view),
                                      shpdata=y2yshp, opac = climtourView$opacity,OSM=F))
        })
    
    } else if (input$tabs == "patourTab") {
      #7). PA Tour Logic ----
      # patourView <- reactiveValues(view = NULL,opacity = NULL)
      # isSwipepa <- reactiveVal(NULL)
      patour <- callModule(tourPanel, "patour", tourName = "pa")
      shinyjs::click("patour-stopBttn")
      callModule(map,"pamap", OSM = F)
      proxy <- leafletProxy(mapId = "pamap-map")
      callModule(ddownBttn,"pamapBttn") #Settings button on Protected Areas Tour Map
      observeEvent(patour$id(),{runjs("window.scrollTo(0,0)")})
      isSwipepa(F)
      observe({
        if (!is.null(input$"pamap-map_bounds")) {
          patourView$view = input$"pamap-map_bounds"}
      })
      observe({
        if (!is.null(input$"pamapBttn-opacity")) {
          patourView$opacity = input$"pamapBttn-opacity"}
      })
      observe({isSwipepa(tourStep(mapid="pamap",tourinfo=pa,tourid=patour$id(),rSwipe=isSwipepa(),view=isolate(patourView$view),shpdata=pashp,
                                opac=patourView$opacity, OSM = T, showAll=T))
      })
     
      observe({
        proxy %>% 
          clearGroup("metrics")
        if (input$patourLayer != "") {
          data <- tilelist[tileName %in% input$patourLayer,]
          proxy %>%
          addTiles(
            urlTemplate = data$tileSubdir,
            attribution = data$tileAttribution,
            group = "metrics",
            layerId = data$tileGroup,
            options = tileOptions(
              tms = T,
              minZoom = minZoom,
              maxZoom = maxZoom,
              unloadInvisibleTiles = T,
              noWrap = T,
              opacity = input$"pamapBttn-opacity",
              zIndex = 9000
            )
          )
        }
      })
  
    } else if (input$tabs == "climexpTab") {
      # polygroup <- reactiveVal("ecoregions")
      # clickedIds <- reactiveValues(ids = vector())
      # multiSelected_wds <- reactiveVal(c(NULL))
      # rwds <- reactiveVal(NULL)
      # eregion <- reactiveValues(edata = NULL,bds = NULL)
      #8). Metric Explorer ----
      #8a) Setup ----
      # shinyjs::click("climexpacc-0-heading")
      # wds <- read_sf(wdfile)
      selectMultiPolys <- function(mapId,calc=T,
                                   data = y2yshp[y2yshp$TOURID %in% y2y$tourId[y2ytour$id()],],
                                   idfield = "TOURID", addPolys = T, newId = "mp_", nameField = NULL, group = NULL) {
        
        proxy <- leafletProxy(mapId = mapId)
        if(isTRUE(calc)) {
          mpClick <- input[[paste0(mapId,"_shape_click")]]
          mpClick$id <- gsub(newId,"",mpClick$id)
          clickedIds$ids <- c(clickedIds$ids, mpClick$id)
          multipolys <- data[data[[idfield]] %in% clickedIds$ids,]
          if (anyDuplicated(clickedIds$ids)) {
            dups <- clickedIds$ids[duplicated(clickedIds$ids)]
            clickedIds$ids <- clickedIds$ids[clickedIds$ids != dups]
            multipolys <- multipolys[multipolys[[idfield]] != dups, ]
            proxy %>% removeShape(paste0(newId,dups))
          }
          if (is.null(nameField)) {
            nameField = idfield
          }
        } else if(isFALSE(calc)) {
          multipolys <- data[data[[idfield]] %in% clickedIds$ids,]
          if (anyDuplicated(clickedIds$ids)) {
            dups <- clickedIds$ids[duplicated(clickedIds$ids)]
            clickedIds$ids <- clickedIds$ids[clickedIds$ids != dups]
            multipolys <- multipolys[multipolys[[idfield]] != dups, ]
            proxy %>% removeShape(paste0(newId,dups))
          }
          if (is.null(nameField)) {
            nameField = idfield
          }
        }
        #Do we actually want to add the polygon to the map? or just capture them?
        if (isTRUE(addPolys)) {
          if(nrow(multipolys)>0){
            proxy %>%
              addPolygons(
                data = multipolys,
                fillColor = NULL,
                fillOpacity = 0.05,
                weight = 5,
                color = "black",
                stroke = T,
                label = ~multipolys[[nameField]],
                layerId = paste0(newId,multipolys[[idfield]]),
                group = group,
                highlightOptions=highlightOptions(
                  color="black",weight=1.5)
              )
          }
        }
        polygroup("wds")
        return(multipolys)
      }
      
      callModule(map, "climexpMap",OSM=F) # Climate Metrics Explorer Map server logic
      callModule(ddownBttn,"climexpMapBttn") #Drop Down Menu Button Server logic 
      proxy <- leafletProxy("climexpMap-map") # Represents the map so that we can make changes to it
      polygroup("ecoregions")
      #8b) Add tiles----
      observe({
        proxy %>% 
          clearGroup("metrics") 
        if (input$climExpLayer != "") {
          data <- tilelist[tileName %in% input$climExpLayer,]
          if(data$tileName == "fwshpath"){
            proxy %>%
              addTiles(
                urlTemplate = data$tileSubdir,
                attribution = data$tileAttribution,
                group = "metrics",
                layerId = data$tileGroup,
                options = tileOptions(
                  tms = T,
                  minZoom = minZoom,
                  maxZoom = maxZoom,
                  unloadInvisibleTiles = T,
                  noWrap = T,
                  opacity = input$"climexpMapBttn-opacity",
                  zIndex = 9000
                )
              ) %>%
              addLegend(
                position = "bottomright",
                colors=c(rgb(255,0,0,maxColorValue = 255),
                         rgb(255,127,127,maxColorValue = 255),
                         rgb(225,225,225,maxColorValue = 255)),
                values=c(150,450,750),
                labels=c("High","Medium","Low"),
                title = paste0("Legend: Tile Fill<br>",data$tileGroup),
                opacity = input$"climexpMapBttn-opacity",
                layerId = "climexpTileLegend",
                group = "pas",
                className= "info legend Legend"
              )
          } else if(data$tileName =="bwshpath"){
            proxy %>%
              addTiles(
                urlTemplate = data$tileSubdir,
                attribution = data$tileAttribution,
                group = "metrics",
                layerId = data$tileGroup,
                options = tileOptions(
                  tms = T,
                  minZoom = minZoom,
                  maxZoom = maxZoom,
                  unloadInvisibleTiles = T,
                  noWrap = T,
                  opacity = input$"climexpMapBttn-opacity",
                  zIndex = 9000
                )
              ) %>%
              addLegend(
                position = "bottomright",
                colors=c(rgb(0,112,255,maxColorValue = 255),
                         rgb(190,232,255,maxColorValue = 255),
                         rgb(225,225,225,maxColorValue = 255)),
                values=c(150,450,750),
                labels=c("High","Medium","Low"),
                title = paste0("Legend: Tile Fill <br>",data$tileGroup),
                opacity = input$"climexpMapBttn-opacity",
                layerId = "climexpTileLegend",
                group = "pas",
                className= "info legend Legend"
              )
          } else {
            cols<-colorNumeric(
              palette="RdYlBu",
              domain=c(0,100),
              reverse=F
            )
            proxy %>%
              addTiles(
                urlTemplate = data$tileSubdir,
                attribution = data$tileAttribution,
                group = "metrics",
                layerId = data$tileGroup,
                options = tileOptions(
                  tms = T,
                  minZoom = minZoom,
                  maxZoom = maxZoom,
                  unloadInvisibleTiles = T,
                  noWrap = T,
                  opacity = input$"climexpMapBttn-opacity",
                  zIndex = 9000
                )
              ) %>%
              addLegend(
                position = "bottomright",
                pal=cols,
                values=c(0:100),
                title = paste0("Legend: Tile Fill (Quantiles)<br>",data$tileGroup),
                opacity = input$"climexpMapBttn-opacity",
                layerId = "climexpTileLegend",
                group = "pas",
                className= "info legend Legend",
                labFormat = labelFormat(suffix = "%",transform = function(x) sort(x, decreasing = TRUE))
              )
          }
        }
      })
     
      #8c) Get All button press in Outputs ----
      observeEvent(input$"climexp-getallSP",{
        multiSelected_wds(rwds())
        msws<-isolate(multiSelected_wds())
        output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=(600+(nrow(msws)*15)))))
        output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=(500+(nrow(msws)*15)))))
        clickedIds$ids<-msws$FIDNUM2
        mswds <- st_drop_geometry(msws) %>% select(-c(1,11:14)) %>% mutate_at(2:9,as.numeric)
        edata <- eregion$edata
        names(mswds) <- c("Name",'Intactness','Topodiversity','Forward Climatic Refugia','Backward Climatic Refugia','Bird Refugia','Tree Refugia',
                          'Tree Carbon','Soil Carbon')
        spdat <- rbind(edata,mswds)
        callModule(appStarPlot,"climexp",data = spdat,namecol = "Name",removecols = NULL, live = T)
        callModule(Table,"climtable",tabdat=rbind(edata %>%slice(1) %>% select(c(9,1:8)),mswds %>% select(c(9,1:8))))
        proxy %>%
          addPolygons(
            data = msws,
            fillColor = "red",
            fillOpacity = 0.05,
            weight = 5,
            color = "black",
            stroke = T,
            label = ~NEWNAME,
            layerId = ~paste0("mp_",FIDNUM2),
            group = "swds",
            highlightOptions=highlightOptions(
              color="white",weight=1.5)
          )
      })
      
      observeEvent(input$"climexp-getallSPxy",{
        multiSelected_wds(rwds())
        msws<-isolate(multiSelected_wds())
        output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=(600+(nrow(msws)*15)))))
        output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=(500+(nrow(msws)*15)))))
        clickedIds$ids<-msws$FIDNUM2
        mswds <- st_drop_geometry(msws) %>% select(-c(1,11:14)) %>% mutate_at(2:9,as.numeric)
        edata <- eregion$edata
        names(mswds) <- c("Name",'Intactness','Topodiversity','Forward Climatic Refugia','Backward Climatic Refugia','Bird Refugia','Tree Refugia',
                          'Tree Carbon','Soil Carbon')
        spdat <- rbind(edata,mswds)
        callModule(appStarPlot,"climexp",data = spdat,namecol = "Name",removecols = NULL, live = T)
        callModule(Table,"climtable",tabdat=rbind(edata %>%slice(1) %>% select(c(9,1:8)),mswds %>% select(c(9,1:8))))
        proxy %>%
          addPolygons(
            data = msws,
            fillColor = "red",
            fillOpacity = 0.05,
            weight = 5,
            color = "black",
            stroke = T,
            label = ~NEWNAME,
            layerId = ~paste0("mp_",FIDNUM2),
            group = "swds",
            highlightOptions=highlightOptions(
              color="white",weight=1.5)
          )
      })
      
      observeEvent(input$"climtable-getallSPtab",{
        multiSelected_wds(rwds())
        msws<-isolate(multiSelected_wds())
        output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=(600+(nrow(msws)*15)))))
        output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=(500+(nrow(msws)*15)))))
        clickedIds$ids<-msws$FIDNUM2
        mswds <- st_drop_geometry(msws) %>% select(-c(1,11:14)) %>% mutate_at(2:9,as.numeric)
        edata <- eregion$edata
        names(mswds) <- c("Name",'Intactness','Topodiversity','Forward Climatic Refugia','Backward Climatic Refugia','Bird Refugia','Tree Refugia',
                          'Tree Carbon','Soil Carbon')
        spdat <- rbind(edata,mswds)
        callModule(appStarPlot,"climexp",data = spdat,namecol = "Name",removecols = NULL, live = T)
        callModule(Table,"climtable",tabdat=rbind(edata %>%slice(1) %>% select(c(9,1:8)),mswds %>% select(c(9,1:8))))
        proxy %>%
          addPolygons(
            data = msws,
            fillColor = "red",
            fillOpacity = 0.05,
            weight = 5,
            color = "black",
            stroke = T,
            label = ~NEWNAME,
            layerId = ~paste0("mp_",FIDNUM2),
            group = "swds",
            highlightOptions=highlightOptions(
              color="white",weight=1.5)
          )
      })
      
      #8d) Clear All button press in Output ----
      observeEvent(input$"climexp-resetSP",{
        multiSelected_wds(NULL)
        proxy %>% clearGroup('swds')
        mswds<-NULL
        spdat<-NULL
        clickedIds$ids<-NULL
        output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=600)))
        output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=500)))
        callModule(appStarPlot,"climexp",data = eregion$edata,namecol = "Name",removecols = NULL, live = T)
        callModule(xyPlot,"climexp",data = rwds(),data2=NULL,namecol = "NEWNAME",offset=1)
        callModule(Table,"climtable",tabdat=eregion$edata[1,] %>% select(c(9,1:8)))
      })
      
      observeEvent(input$"climexp-resetSPxy",{
          multiSelected_wds(NULL)
          proxy %>% clearGroup('swds')
          mswds<-NULL
          spdat<-NULL
          clickedIds$ids<-NULL
          output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=600)))
          output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=500)))
          removeUI(selector = "div:has(>#climtable", session = session)
          callModule(appStarPlot,"climexp",data = eregion$edata,namecol = "Name",removecols = NULL, live = T)
          callModule(xyPlot,"climexp",data = rwds(),data2=NULL,namecol = "NEWNAME",offset=1)
          callModule(Table,"climtable",tabdat=eregion$edata[1,] %>% select(c(9,1:8)))
        })
      
      observeEvent(input$"climtable-resetSPtab",{
        multiSelected_wds(NULL)
        proxy %>% clearGroup('swds')
        mswds<-NULL
        spdat<-NULL
        clickedIds$ids<-NULL
        output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=600)))
        output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=500)))
        removeUI(selector = "div:has(>#climtable", session = session)
        callModule(appStarPlot,"climexp",data = eregion$edata,namecol = "Name",removecols = NULL, live = T)
        callModule(xyPlot,"climexp",data = rwds(),data2=NULL,namecol = "NEWNAME",offset=1)
        callModule(Table,"climtable",tabdat=eregion$edata[1,] %>% select(c(9,1:8)))
      })
      
      
      #8e Add polygons and fill by mean value ----
      observeEvent(polygroup(),{
        observeEvent(
          {input$climFillPolys
          input$"climexpMapBttn-polyopacity"
          },{
          if(polygroup() == "ecoregions" & input$climFillPolys != ""){
            nam<-names(polyfillvect)[which(polyfillvect==input$climFillPolys)]
            var<-paste0(input$climFillPolys,"3")
            cols<-colorNumeric(
              palette="RdYlBu",
              domain=c(0,100),
              reverse=F
            )
            proxy %>%
              clearGroup("ecoregions") %>%
              addPolygons(
                data=ecos,
                fillColor = ~cols(rescale(na.omit(ecos[[var]]),to=c(100,0))),
                stroke = T,
                weight=1,
                color="white",
                fillOpacity=input$"climexpMapBttn-polyopacity",
                label = ~htmlEscape(ecos$NA_L3NAME),
                layerId = ~ecos$ecoreg3,
                group = "ecoregions",
                highlightOptions=highlightOptions(
                  color="white",weight=2.5)
              ) %>%
              addLegend(
                position = "bottomright",
                pal=cols,
                values=rescale(na.omit(ecos[[var]]),to=c(100,0)),
                title = paste0("Legend:Polygon Fill<br>",nam),
                opacity = input$"climexpMapBttn-polyopacity",
                layerId = "climexpPolyLegend",
                group = "wds",
                className= "info legend polyLegend",
                labFormat = labelFormat(transform = function(x) sort(pretty(rescale(x,to=range(na.omit(ecos[[var]])))), decreasing = T))
              )
          } else if(polygroup() == "ecoregions" & input$climFillPolys == ""){
            proxy %>%
              clearGroup("ecoregions") %>%
              addPolygons(
                data=ecos,
                fillColor = NA,
                stroke = T,
                color="blue",
                fill=T,
                fillOpacity=0,
                weight=1,
                label = ~htmlEscape(ecos$NA_L3NAME),
                layerId = ~ecos$ecoreg3,
                group = "ecoregions",
                highlightOptions=highlightOptions(
                  color="white",weight=2.5)
              )
          } else if (polygroup()=="wds" & input$climFillPolys =="") {
              proxy %>%
                clearGroup("ecoregions") %>%
                clearGroup("wds") %>%
                addPolygons(
                  data = rwds(),
                  fillColor = NA,
                  fillOpacity=0,
                  weight = 1,
                  color = "blue",
                  stroke = T,
                  label = ~htmlEscape(NEWNAME),
                  layerId = ~FIDNUM2,
                  group = "wds",
                  highlightOptions=highlightOptions(
                    color="white",weight=2.5)
                )
          } else if (polygroup()=="wds" & input$climFillPolys != ""){
            var<-input$climFillPolys
            nam<-names(polyfillvect)[which(polyfillvect==var)]
            if(is.null(rwds())){return()}
            data<-rwds()
            cols<-colorNumeric(
              palette="RdYlBu",
              domain=c(0,100),
              reverse=F
            )
            proxy %>%
              clearGroup("wds") %>%
              addPolygons(
                data = data,
                fillColor = ~cols(rescale(na.omit(data[[var]]),to=c(100,0))),
                fillOpacity=input$"climexpMapBttn-polyopacity",
                weight = 1,
                color = "white",
                stroke = T,
                label = ~htmlEscape(NEWNAME),
                layerId = ~FIDNUM2,
                group = "wds",
                highlightOptions=highlightOptions(
                  color="white",weight=2.5)
              ) %>%
              addLegend(
                position = "bottomright",
                pal=cols,
                values=rescale(na.omit(data[[var]]),to=c(100,0)),
                title = paste0("Legend:Polygon Fill<br>",nam),
                opacity = input$"climexpMapBttn-polyopacity",
                layerId = "climexpPolyLegend",
                group = "wds",
                className= "info legend polyLegend",
                labFormat = labelFormat(transform = function(x) sort(pretty(rescale(x,to=range(na.omit(data[[var]])))), decreasing = T))
              )
          } else if(is.null(polygroup()) | is.null(input$climFillPolys)){
          }
        })
      })
        
      #8f) Map Polygon Click Logic----
      observeEvent(input$"climexpMap-map_shape_click", {
        click <- input$"climexpMap-map_shape_click"
        #If the polygon is an ecoregion:
          # add starplot user interface
          # add return to ecoregions button
          # get the ecoregion data for the clicked polygon
        if (click$group == "ecoregions") {
          # 8e) i. If the clicked polygon is an ecoregion ----
          
          output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=600)))
          output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=500)))
          output$climtablediv <- renderUI(div(id="climtable",TableUI("climtable",all=T,reset=T)))
          output$b2ecoBttn <- renderUI(
            div(
              class = "b2eBttn",
              actionBttn(
                inputId = "climExpB2E",
                label = "Back to Ecoregions",
                icon = icon("undo", class = "fas fas-lg", lib = "font-awesome"),
                color = "primary", 
                size = "md",
                style = "unite",
                block = F
              )
            )
          )
          #e) Get the ecoregion data
          eregion$edata <- ecos[which(ecos$ecoreg3 == click$id),]
          min <- l3min[which(l3min$ecr3_id ==eregion$edata$ecoreg3),2:9]
          max <- l3max[which(l3max$ecr3_id == eregion$edata$ecoreg3),2:9]
          min$NEWNAME <- "MIN"
          max$NEWNAME <- "MAX"
          eregion$bds <- st_bbox(eregion$edata)
          #e) Get the watershed data for watersheds within that ecoregion.
          bds <- unname(eregion$bds)
          rwds(wds[wds$ecoreg3 == eregion$edata$ecoreg3,])
          foo <- isolate(rwds())
          # e) Fix any missing name issues
          if (any(is.na(foo$NEWNAME))) {
            foo$NEWNAME <- paste0("Watershed: ", foo$FIDNUM2)
            rwds(foo)
          }
          #e)Fix any duplicated name issues
          if(any(duplicated(foo$NEWNAME))){
            foo$NEWNAME[duplicated(foo$NEWNAME)]<-paste0(foo$NEWNAME[duplicated(foo$NEWNAME)],"_B")
            rwds(foo)
          }
          
          #e) Remove ecoregion polygons and add the watershed polygons
            proxy %>% 
              flyToBounds(lng1 = bds[1],lat1 = bds[2],lng2 = bds[3],lat2 = bds[4]) %>%
              clearGroup("ecoregions")
          polygroup("wds")
            
          #e) Do some data wrangling to get the starplot to plot
          eregion$edata <- eregion$edata %>% select(intact3,elevdiv3,fwvelref3,bwvelref3,brdref3,treref3,treec3,soilc3) %>% st_drop_geometry() %>% mutate_all(as.numeric)
          names(eregion$edata) <- c("intact","elevdiv","fwvelref" ,"bwvelref", "brdref","treref","treec","soilc")
          eregion$edata$NEWNAME <-  "Ecoregion Avg"
          
          eregion$edata <- rbind(eregion$edata,min,max)
          names(eregion$edata) <- c('Intactness','Topodiversity','Forward Climatic Refugia','Backward Climatic Refugia','Bird Refugia','Tree Refugia',
                            'Tree Carbon','Soil Carbon',"Name")
          callModule(xyPlot,"climexp",data=rwds(),data2= NULL, namecol="NEWNAME",offset=1)
          callModule(appStarPlot,"climexp",data = eregion$edata,namecol = "Name",removecols = NULL, live = T)
          callModule(Table,"climtable",tabdat=eregion$edata[1,] %>% select(c(9,1:8)))
          rm(click)
          # polygroup("wds")
          #8e) ii. If the clicked polygon is a watershed ----
        } else if (click$group == "wds" | click$group == "swds" ) {
          
          multiSelected_wds(
            selectMultiPolys(mapId = "climexpMap-map",data = rwds(),
                             idfield = "FIDNUM2", addPolys = T, newId = "mp_",nameField = "NEWNAME",group = "swds")
          )
          msds<-isolate(multiSelected_wds())
          output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = T,all=T,reset=T,height=(600+(nrow(msds)*15)))))
          output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp",all=T,reset=T,live=T,height=(500+(nrow(msds)*15)))))
          
          mswds <- st_drop_geometry(msds) %>% select(-c(1,11:14)) %>% mutate_at(2:9,as.numeric)
          edata <- eregion$edata
          names(mswds) <- c("Name",'Intactness','Topodiversity','Forward Climatic Refugia','Backward Climatic Refugia','Bird Refugia','Tree Refugia',
                            'Tree Carbon','Soil Carbon')
          
          spdat <- rbind(edata,mswds)
          if (nrow(spdat) > 1){
            callModule(report,"climReport",polys=rwds(),polys2=msds,data=wds,namecol="NEWNAME",pa=F)
            callModule(appStarPlot,"climexp",data = spdat,namecol = "Name",removecols = NULL, live = T)
            callModule(Table,"climtable",tabdat=rbind(edata %>%slice(1) %>% select(c(9,1:8)),mswds %>% select(c(9,1:8))))
          }
          rm(click)
          polygroup("wds")
        }
        
      })
        #8f) If the back to ecoregions button is pressed ----
      observeEvent(input$climExpB2E,{
        removeUI(selector = "div:has(>#climExpB2E)", session = session)
        removeUI(selector = "div:has(>#climexp-appStarPlot)", session = session)
        removeUI(selector = "div:has(>#climexpp-xyPlot)", session = session)
        removeUI(selector = "div:has(>#climtable", session = session)
        output$climexpStarplotDiv <- NULL
        output$climexpXYplotdiv <- NULL
        output$climtablediv <- NULL
        polygroup("ecoregions")
        mswds<-NULL
        spdat<-NULL
        rwds(NULL)
        multiSelected_wds(NULL)
        clickedIds$ids<-NULL
        proxy <- leafletProxy("climexpMap-map") %>% hideGroup("Place Labels") %>%
          clearGroup("wds") %>%
          clearGroup("swds") %>%
          setView(lng = -100, lat = 55, zoom = 3)
      })
      #8g) XYPlot logic ----
      observeEvent(rwds(),{
        if(!is.null(rwds())){
          callModule(xyPlot,"climexp",data=rwds(),
                     data2= NULL, namecol="NEWNAME",offset=1)
        }
      })
      
      observeEvent(multiSelected_wds(),{
        observeEvent(input$"climexp-X",{
          if (is.null(multiSelected_wds())) {
            callModule(xyPlot,"climexp",data=rwds(),
                       data2= NULL, namecol="NEWNAME",offset=1)
          } else {
            callModule(xyPlot,"climexp",data=rwds(),
                      data2= multiSelected_wds(), namecol="NEWNAME",offset=1)
          }
        })
        observeEvent(input$"climexp-Y",{
          if (is.null(multiSelected_wds())) {
            callModule(xyPlot,"climexp",data=rwds(),data2=NULL,namecol="NEWNAME",offset=1)
          } else {  
            callModule(xyPlot,"climexp",data=rwds(),data2=multiSelected_wds(),namecol="NEWNAME",offset=1)
          }
        })
      })
      #8h) Clear Tiles ----
      observeEvent(input$clearTileFill,{
        if(is.null(input$clearTileFill)){return()}
        updateSelectizeInput(
          session = session,
          inputId = "climExpLayer",
          selected= NULL,
          choices = c("Select metric: " = "", tilevect),
          options = list(maxOptions = 12)
        )
        proxy %>% removeControl("climexpTileLegend")
      })
      #8i) Clear Polygon Fill ----
      observeEvent(input$clearPolyFill,{
        if(is.null(input$clearPolyFill)){return()}
        updateSelectizeInput(
          session = session,
          inputId = "climFillPolys",
          selected= NULL,
          choices = c("Fill visible polygons with ..." = "",
                      polyfillvect),
          options = list(maxOptions = 12)
        )
        proxy %>% removeControl("climexpPolyLegend")
      })
      
    } else if (input$tabs == "paexpTab") {
    #9). PA Explorer Logic ----
      selectMultiPolysPAS <- function(mapId,calc=T,mapclick=NULL,
                                      data = y2yshp[y2yshp$TOURID %in% y2y$tourId[y2ytour$id()],],
                                      idfield = "TOURID", addPolys = T, newId = "", nameField = NULL, group = NULL) {
        
        proxy <- leafletProxy(mapId = mapId) %>% clearGroup("rpas")
        if(isTRUE(calc)){
          mpClickPAs <- mapclick#input[[paste0(mapId,"_shape_click")]]
          
          # mpClickPAs$id <- mpClickPAs$id
          clickedIdsPAs$ids <- c(clickedIdsPAs$ids, mpClickPAs$id)
          multipolysPAs <- data[data[[idfield]] %in% clickedIdsPAs$ids,]
          multipolysPAs <- multipolysPAs[match(clickedIdsPAs$ids,multipolysPAs[[idfield]]),]
          if (anyDuplicated(clickedIdsPAs$ids)) {
            dupsPAs <- clickedIdsPAs$ids[duplicated(clickedIdsPAs$ids)]
            clickedIdsPAs$ids <- clickedIdsPAs$ids[clickedIdsPAs$ids != dupsPAs]
            multipolysPAs <- multipolysPAs[multipolysPAs[[idfield]] != dupsPAs, ]
            proxy %>% removeShape(dupsPAs)
          }
          if (is.null(nameField)) {
            nameField = idfield
          }
        } else if(isFALSE(calc)) {
          multipolysPAs <- data[data[[idfield]] %in% clickedIdsPAs$ids,]
          if (anyDuplicated(clickedIdsPAs$ids)) {
            dupsPAs <- clickedIdsPAs$ids[duplicated(clickedIdsPAs$ids)]
            clickedIdsPAs$ids <- clickedIdsPAs$ids[clickedIdsPAs$ids != dupsPAs]
            multipolysPAs <- multipolysPAs[multipolysPAs[[idfield]] != dupsPAs, ]
            proxy %>% removeShape(dupsPAs)
          }
          if (is.null(nameField)) {
            nameField = idfield
          }
        }
        #Do we actually want to add the polygon to the map? or just capture them?
        if (isTRUE(addPolys)) {
          if(nrow(multipolysPAs)>0){
            proxy %>%
              addPolygons(
                data = multipolysPAs,
                fillColor = NULL,
                fillOpacity = 0.05,
                weight = 5,
                color = "black",
                stroke = T,
                label = ~ multipolysPAs[[nameField]],
                layerId = multipolysPAs[[idfield]],
                group = group,
                highlightOptions=highlightOptions(
                  color="black",weight=1.5),
                options=pathOptions(zIndex=1000)
              )
          }
        }
        return(multipolysPAs)
      }
      
      proxy <- leafletProxy("paexpMap-map")
      # proxy %>% clearGroup('rpas')
      # proxy %>% clearGroup("pas")
      #9a) Get protected area polyons that are in view and at scale ----
      inBounds_PAs <- reactive({
        if (is.null(input$"paexpMap-map_zoom"))
          return(NULL)
        if (is.null(input$"paexpMap-map_bounds"))
          return(NULL)
        bbx <- input$"paexpMap-map_bounds"
        if(bbx$west <= -180.) {bbx$west = -180}
        if(bbx$east >= 180.) {bbx$east = 180}
        clip <- st_sfc(st_polygon(
          x = list(rbind(
            c(bbx$west,bbx$south),
            c(bbx$east,bbx$south),
            c(bbx$east,bbx$north),
            c(bbx$west,bbx$north),
            c(bbx$west,bbx$south)
            ))
        ),check_ring_dir = T) %>% st_set_crs(4326)
        ind <- st_intersects(pts,clip,sparse=F)
        if (any(ind)) {
          data <- pas[ind,]
          data <- data[data$AREA >= zoomcuts[input$"paexpMap-map_zoom"],]
          if (length(data) < 1) {
            data <- pas[1,]
          }
        } else {
          data <-pas[1,]
        }
        return(data)
      }) %>% debounce(500)
      
      #9b) Add polygons to the map based on the view and scale.----
      observeEvent(inBounds_PAs(),{
        print("I'm triggered")
        rpas(inBounds_PAs())
        rps<-isolate(rpas())
        if(!is.null(rps)){
          if(!is.null(nrow(rps))){
          observeEvent(
            {
              input$paFillPolys
              input$"paexpMapBttn-polyopacity"
            },
            {
              if(input$paFillPolys!=""){
              var<-input$paFillPolys
              nam<-names(polyfillvect)[which(polyfillvect==var)]
              cols<-colorNumeric(
                palette="RdYlBu",
                domain=c(0,100),
                reverse=F
              )
              proxy %>%
                clearShapes() %>%
                addPolygons(
                  data = rps,
                  fillColor = ~cols(rescale(na.omit(rps[[var]]),to=c(100,0))),
                  fillOpacity = input$"paexpMapBttn-polyopacity",
                  weight = 1,
                  stroke = T,
                  color = "white",
                  label = ~htmlEscape(PA_NAME),
                  layerId = ~ gridcode,
                  group= "pas",
                  highlightOptions=highlightOptions(
                    color="white",weight=2.5)
                ) %>%
                addLegend(
                  position = "bottomright",
                  pal=cols,
                  values=rescale(na.omit(rps[[var]]),to=c(100,0)),
                  title = paste0("Legend:Polygon Fill<br>",nam),
                  opacity = input$"paexpMapBttn-polyopacity",
                  layerId = "paexpPolyLegend",
                  # group = "wds",
                  className= "info legend polyLegend",
                  labFormat = labelFormat(transform = function(x) sort(pretty(rescale(x,to=range(na.omit(rps[[var]])))), decreasing = T))
                )
              selectMultiPolysPAS(mapId = "paexpMap-map",calc=F,data = pas,
                               idfield = "gridcode", addPolys = T, newId = "",nameField = "PA_NAME",group = "rpas")
            } else {
              proxy %>%
                clearShapes() %>%
                addPolygons(
                  data = rps,
                  fillOpacity = 0,
                  weight = 1,
                  color = "blue",
                  stroke = T,
                  label = ~htmlEscape(PA_NAME),
                  layerId = ~ gridcode,
                  group= "pas",
                  highlightOptions=highlightOptions(
                    color="blue",weight=2.5)
                )
              selectMultiPolysPAS(mapId = "paexpMap-map",calc=F,data = pas,
                               idfield = "gridcode", addPolys = T, newId = "",nameField = "PA_NAME",group = "rpas")
              }
            })
          }
        }
      })
      
      #9c) Clear all button press in Outputs----
      observeEvent(input$"paexp-resetSP",{
        multiSelected_pas(NULL)
        dupsPAs<-NULL
        tabdat<-NULL
        proxy %>% clearGroup('rpas')
        mspas<-NULL
        padat<-NULL
        clickedIdsPAs$ids<-vector()
        multipolysPAs<-NULL
        mpClickPAs<-NULL
        rps<-NULL
        removeUI(selector = "div:has(>#paexp-appStarPlot)", session = session)
        removeUI(selector = "div:has(>#paexp-xyPlot)", session = session)
        removeUI(selector = "div:has(>#patable", session = session)
        output$paexpStarplotDiv <- NULL
        output$paexpXYplotdiv <- NULL
        output$patablediv<-NULL
      })
      observeEvent(input$"paexp-resetSPxy",{
        multiSelected_pas(NULL)
        dupsPAs<-NULL
        tabdat<-NULL
        proxy %>% clearGroup('rpas')
        mspas<-NULL
        padat<-NULL
        clickedIdsPAs$ids<-vector()
        multipolysPAs<-NULL
        mpClickPAs<-NULL
        rps<-NULL
        removeUI(selector = "div:has(>#paexp-appStarPlot)", session = session)
        removeUI(selector = "div:has(>#paexp-xyPlot)", session = session)
        removeUI(selector = "div:has(>#paexp-xyPlot)", session = session)
        removeUI(selector = "div:has(>#patable", session = session)
        output$paexpStarplotDiv <- NULL
        output$paexpXYplotdiv <- NULL
        output$patablediv<-NULL
      })
      observeEvent(input$"patable-resetSPtab",{
        multiSelected_pas(NULL)
        dupsPAs<-NULL
        multipolysPAs<-NULL
        tabdat<-NULL
        proxy %>% clearGroup('rpas')
        mspas<-NULL
        padat<-NULL
        clickedIdsPAs$ids<-vector()
        mpClickPAs<-NULL
        rps<-NULL
        removeUI(selector = "div:has(>#paexp-appStarPlot)", session = session)
        removeUI(selector = "div:has(>#paexp-xyPlot)", session = session)
        removeUI(selector = "div:has(>#paexp-xyPlot)", session = session)
        removeUI(selector = "div:has(>#patable", session = session)
        output$paexpStarplotDiv <- NULL
        output$paexpXYplotdiv <- NULL
        output$patablediv<-NULL
      })
      
      #9d) Add tiles from dropdown ----
      observe({
        proxy %>% 
          clearGroup("metrics")
        if (input$paExpLayer != "") {
          data <- tilelist[tileName %in% input$paExpLayer,]
          if(data$tileName == "fwshpath"){
            proxy %>%
            addTiles(
              urlTemplate = data$tileSubdir,
              attribution = data$tileAttribution,
              group = "metrics",
              layerId = data$tileGroup,
              options = tileOptions(
                tms = T,
                minZoom = minZoom,
                maxZoom = maxZoom,
                unloadInvisibleTiles = T,
                noWrap = T,
                opacity = input$"paexpMapBttn-opacity",
                zIndex = 9000
              )
            ) %>%
            addLegend(
              position = "bottomright",
              colors=c(rgb(255,0,0,maxColorValue = 255),
                       rgb(255,127,127,maxColorValue = 255),
                       rgb(225,225,225,maxColorValue = 255)),
              values=c(150,450,750),
              labels=c("High","Medium","Low"),
              title = paste0("Legend: Tile Fill<br>",data$tileGroup),
              opacity = input$"paexpMapBttn-opacity",
              layerId = "paexpTileLegend",
              group = "pas",
              className= "info legend Legend"
            )
          } else if(data$tileName =="bwshpath"){
            proxy %>%
            addTiles(
              urlTemplate = data$tileSubdir,
              attribution = data$tileAttribution,
              group = "metrics",
              layerId = data$tileGroup,
              options = tileOptions(
                tms = T,
                minZoom = minZoom,
                maxZoom = maxZoom,
                unloadInvisibleTiles = T,
                noWrap = T,
                opacity = input$"paexpMapBttn-opacity",
                zIndex = 9000
              )
            ) %>%
            addLegend(
              position = "bottomright",
              colors=c(rgb(0,112,255,maxColorValue = 255),
                       rgb(190,232,255,maxColorValue = 255),
                       rgb(225,225,225,maxColorValue = 255)),
              values=c(150,450,750),
              labels=c("High","Medium","Low"),
              title = paste0("Legend: Tile Fill <br>",data$tileGroup),
              opacity = input$"paexpMapBttn-opacity",
              layerId = "paexpTileLegend",
              group = "pas",
              className= "info legend Legend"
            )
          } else {
            cols<-colorNumeric(
              palette="RdYlBu",
              domain=c(0,100),
              reverse=F
            )
            proxy %>%
            addTiles(
              urlTemplate = data$tileSubdir,
              attribution = data$tileAttribution,
              group = "metrics",
              layerId = data$tileGroup,
              options = tileOptions(
                tms = T,
                minZoom = minZoom,
                maxZoom = maxZoom,
                unloadInvisibleTiles = T,
                noWrap = T,
                opacity = input$"paexpMapBttn-opacity",
                zIndex = 9000
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal=cols,
              values=c(0:100),
              title = paste0("Legend: Tile Fill (Quantiles)<br>",data$tileGroup),
              opacity = input$"paexpMapBttn-opacity",
              layerId = "paexpTileLegend",
              group = "pas",
              className= "info legend Legend",
              labFormat = labelFormat(suffix="%",transform = function(x) sort(x, decreasing = TRUE))
            )
          }
          
        }
      })
      
      #9e) Map polygon click logic ----
        #9e) i. Get the protected areas that have been clicked, do some data wrangling and create the starplot.
      mapclickPA<-reactive({
        print(input$"paexpMap-map_shape_click"$id)
        input$"paexpMap-map_shape_click"
      })
      observeEvent(mapclickPA(),{
        multiSelected_pas(
          selectMultiPolysPAS(mapId = "paexpMap-map",data = pas,mapclick=mapclickPA(),
                              idfield = "gridcode", addPolys = T, newId = "",nameField = "PA_NAME",group = "rpas")
        )
       
      })
      
      observeEvent(multiSelected_pas(),{
        mspas <- isolate(multiSelected_pas()) 
        output$paexpStarplotDiv <- renderUI(div(id = "paExpStarPlot", appStarPlotUI("paexp", live = T,all=F,reset=T,height=(600+(nrow(mspas)*15)))))
        output$paexpXYplotdiv <- renderUI(div(id = "paExpXYPlot", xyPlotUI("paexp",all=F,reset=T,pa=T,live=T,height=(500+(nrow(mspas)*15)))))
        output$patablediv <- renderUI(div(id="patable",TableUI("patable",all=F,reset=T)))
        if(nrow(mspas) >0){
          callModule(report,"paReport",polys=mspas,data=pas)
        }
        
        pamin <- l1min[which(l1min$ecr1_id %in% mspas$ecoreg1),2:9]
        pamin <- pamin %>% summarise_all(min,na.rm=F)
        pamax <- l1max[which(l1max$ecr1_id %in% mspas$ecoreg1),2:9]
        pamax <- pamax %>% summarise_all(max,na.rm=F)
        pamin$Name <- "MIN"
        pamax$Name <- "MAX"
        nams<-c('Intactness','Topodiversity','Forward Climatic Refugia',
                'Backward Climatic Refugia','Bird Refugia','Tree Refugia',
                'Tree Carbon','Soil Carbon',"Name")
        names(pamin) <- nams
        names(pamax) <- nams
        padat <- st_drop_geometry(mspas) %>% select(c(4:11,15)) %>% mutate_at(1:8,as.numeric)
        names(padat) <- nams
        tabdat<-padat %>% select(9,1:8)
        names(tabdat)[3]<-"Topo-Diversity"
        padat <- rbind(pamin,pamax,padat)
        if(nrow(padat)>2){
          callModule(appStarPlot,"paexp",data = padat,namecol = "Name",removecols = NULL, live = T)
          callModule(Table,"patable",tabdat=tabdat)
        }      
      }) 
      
      #9e) iii. Logic to handle if no polygons are selected but were in the past----
      # observe(
      #   if (!is.null(multiSelected_pas())) {
      #     print(multiSelected_pas())
      #     if (nrow(multiSelected_pas()) == 0) {
      #       removeUI(selector = "div:has(>#paexp-appStarPlot)", session = session)
      #       removeUI(selector = "div:has(>#paexp-xyPlot)", session = session)
      #       removeUI(selector = "div:has(>#patable)", session = session)
      #       output$paexpStarplotDiv <- NULL
      #       output$paexpXYplotdiv <- NULL
      #       output$patablediv<-NULL
      #       # multiSelected_pas<-NULL
      #     } 
      #   }
      # )
      #9f) XY Plot Logic ----
      observeEvent(multiSelected_pas(),{
      mspas <- isolate(multiSelected_pas())
       observeEvent(input$"paexp-X",{
          if (is.null(mspas)) {
           callModule(xyPlot,"paexp",data=pas,
                       data2= NULL, namecol="PA_NAME",offset=0,pa=T)
          } else if(nrow(mspas)>0) {
            biomeInd <- which(pas$ecoreg1 %in% mspas$ecoreg1)
            callModule(xyPlot,"paexp",data=pas[biomeInd,],
                         data2= mspas, namecol="PA_NAME",offset=0,pa=T)
          }
        })
        observeEvent(input$"paexp-Y",{
          if (is.null(mspas)) {
            callModule(xyPlot,"paexp",data=pas,data2=NULL,namecol="PA_NAME",offset=0,pa=T)
          } else if(nrow(mspas)>0) {
            biomeInd <- which(pas$ecoreg1 %in% mspas$ecoreg1)
            callModule(xyPlot,"paexp",data=pas[biomeInd,],data2=mspas,namecol="PA_NAME",offset=0,pa=T)
          }
        })
      })
      #9g) Clear Tiles ----
      observeEvent(input$paclearTileFill,{
        if(is.null(input$paclearTileFill)){return()}
        updateSelectizeInput(
          session = session,
          inputId = "paExpLayer",
          selected= "",
          choices = c("Select metric: " = "", tilevect),
          options = list(maxOptions = 12)
        )
        proxy %>% removeControl("paexpTileLegend")
      })
      #9h) Clear Polygon Fill ----
      observeEvent(input$paclearPolyFill,{
        if(is.null(input$paclearPolyFill)){return()}
        updateSelectizeInput(
          session = session,
          inputId = "paFillPolys",
          selected= NULL,
          choices = c("Fill visible polygons with ..." = "",
                      polyfillvect),
          options = list(maxOptions = 12)
        )
        proxy %>% removeControl("paexpPolyLegend")
      })
    }
  }) #End of observe which tab we are on.

  #WRITE REPORT STATS TO DROPBOX ----
  session$onSessionEnded(function() {
    downfrequency <- mydownloads %>% group_by(Name) %>% tally()
    repFrequency <- mydownloads %>% group_by(Format) %>% tally()
    interactiveFrequency <- mydownloads %>% group_by(Interactive) %>% tally()
    paFrequency <- mydownloads %>% group_by(ProtectedArea) %>% tally()
    write_csv(downfrequency,"./report_stats/downloadfrequency.csv")
    write_csv(repFrequency,"./report_stats/reportFrequency.csv")
    write_csv(interactiveFrequency,"./report_stats/interactiveFrequency.csv")
    write_csv(paFrequency,"./report_stats/paFrequency.csv")
    write_csv(mydownloads,"./report_stats/downloads.csv")
    if(isTRUE(reportStatsStatus)){
      drop_upload(file = './report_stats/downloadfrequency.csv',dtoken=token)
      drop_upload(file = "./report_stats/reportFrequency.csv",dtoken=token)
      drop_upload(file = "./report_stats/interactiveFrequency.csv",dtoken=token)
      drop_upload(file = "./report_stats/paFrequency.csv",dtoken=token)
      drop_upload(file = "./report_stats/downloads.csv",dtoken=token)
      file.remove(list.files(path="./report_stats",pattern=".csv",full.names=T))
    }
    reps_size<-sum(file.info(list.files(path=reportdir,all.files=T,recursive=T,full.names=T))$size)
    if(reps_size>1E7){
      print(utils:::format.object_size(reps_size, units="MB"))
      dirs<-list.dirs(reptmpdir,full.names=T,recursive = F)
      htmlfiles<-list.files(path=reptmpdir,full.names=T,recursive=F,pattern=".html")
      if(length(dirs)>0){
        lapply(dirs,unlink,recursive=T,force=T)
      }
      if(length(htmlfiles)>0){
        lapply(htmlfiles,unlink,force=T)
      }
    }
    if(reps_size>1E9){
      statreps<-list.files(path=repstaticdir,full.names=T)
      if(length(statreps)>0) {
        lapply(statsreps,unlink,force=T)
      }
    }
    if(reps_size>5.2E9) {
      interreps<-list.files(path=repinterdir,full.names=T)
      if(length(interreps)>0){
        lapply(interreps,unlink,force=T)
      }
      reps_size<-sum(file.info(list.files(path=reportdir,all.files=T,recursive=T,full.names=T))$size)
      if(reps_size>5.2E9) {
        imgs<-list.files(path=repimgdir,full.names=T,pattern=".png")
        if(length(imgs)>0){
          lapply(imgs,unlink,force=T)
        }
      }
    }
  })
  
  #Bookmarking Stuff ----
  excludeList<-c(
    "y2ymapBttn-opacity","climExpLayer","clearTileFill","clearPolyFill",
    "climFillPolys","patourLayer","paExpLayer","pamapBttn-opacity",
    "pamapBttn-polyopacity","paclearTileFill","paFillPolys","paclearPolyFill",
    "y2ymapBttn-bookmarkBttn","climexpMapBttn-bookmarkBttn","patour-endBttn",
    "paexpMapBttn-bookmarkBttn","pamapBttn-bookmarkBttn","paexpMapBttn-opacity",
    "paReport-reportFormat","paexpMapBttn-map_inputs","climexpMapBttn-opacity",
    "climexpMapBttn-polyopacity","paexpMapBttn-polyopacity","patour-prevBttn",
    "patour-startBttn","patour-stopBttn","patour-nextBttn","y2ytour-prevBttn",
    "paReport-reportInteractive","paReport-reportBttn_bttn","climExpB2E",
    "climReport-reportFormat","climReport-reportBttn_bttn","y2ytour-nextBttn",
    "pamapBttn-map_inputs","climexpMapBttn-map_inputs","y2ymapBttn-polyopacity",
    "y2ymapBttn-map_inputs","y2ytour-startBttn","y2ytour-endBttn","y2ytour-stopBttn",
    "climReport-reportInteractive"
  )
    
  observe({
    shinyJSexclude<- grep("shinyjs", names(input), value = TRUE)
    plotlyExclude <-grep("plotly", names(input), value = TRUE)
    setBookmarkExclude(c(excludeList,shinyJSexclude,plotlyExclude))
  })

  observeEvent(input$"paexpMapBttn-bookmarkBttn",{
      session$doBookmark()
  })
  observeEvent(input$"y2ymapBttn-bookmarkBttn",{
    session$doBookmark()
  })
  observeEvent(input$"climexpMapBttn-bookmarkBttn",{
    session$doBookmark()
  })
  observeEvent(input$"pamapBttn-bookmarkBttn",{
    session$doBookmark()
  })
  
}
