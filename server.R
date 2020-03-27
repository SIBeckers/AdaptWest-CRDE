#AdaptWestApp server.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 0.1
#Notes
source("global.R")

function(input, output, session) {
  # 1). LOGIN MENU ----
  if (isTRUE(loginMenu)) {
    showModal(
      tags$div(
        id = "div-passmodal",
        modalDialog(
          id = "passmodal",
          h2("NOTE: THIS APP IS CURRENTLY UNDER DEVELOPMENT!"),
          h3("FEATURES, LAYOUT, AND CONTENT MAY CHANGE."),
          br(),
          br(),
          textInput("user","User"),
          passwordInput("password","Password:"),
          actionButton("go", "Submit"),
          title = "Welcome, Please enter username and password",
          size = "l",
          easyClose = F,
          fade = F,
          footer = NULL
        )
      )
    )
    observe({
      req(input$go)
      user <- isolate(input$user)
      pass <- isolate(input$password)
      if (user != theuser | pass != thepassword){
        showModal(
          tags$div(
            id = "div-passmodal",
            modalDialog(id = "passmodal",
              h2("NOTE: THIS APP IS CURRENTLY UNDER DEVELOPMENT!"),
              h3("FEATURES, LAYOUT, AND CONTENT MAY CHANGE."),
              br(),
              br(),
              textInput("user","User"),
              passwordInput("password","Password:"),
              actionButton("go", "Submit"),
              title = "Welcome, Please enter username and password",
              size = "l",
              easyClose = F,
              fade = F,
              footer = NULL
            )
          )
        )
      } else{
        removeModal(session)
      }
    },priority=2)
  }
  #2). Common Reactives ----
  clickedIds <- reactiveValues(ids = vector())
  multiSelected <- reactiveVal()
  multiSelected_pas <- reactiveVal(c(NULL))
  multiSelected_wds <- reactiveVal(c(NULL))
  rwds <- reactiveVal(NULL)
  rpas <- reactiveVal(NULL)
  bpas <- reactiveVal(NULL)
  climtourView <- reactiveValues(view = NULL,opacity = NULL)
  patourView <- reactiveValues(view = NULL,opacity = NULL)
  isSwipemetric <- reactiveVal(NULL)
  isSwipepa <- reactiveVal(NULL)
  eregion <- reactiveValues(edata = NULL,bds = NULL)
  fillColor <-reactiveValues(fill = NULL)
  # Common Functions ----
    selectMultiPolys <- function(mapId,
                               data = y2yshp[y2yshp$TOURID %in% y2y$tourId[y2ytour$id()],],
                               idfield = "TOURID", addPolys = T, newId = "mp_", nameField = NULL, group = NULL) {
      proxy <- leafletProxy(mapId = mapId)
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
      print(multipolys)
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
                color="white",weight=1.5)
            )
        }
      }
      return(multipolys)
    }
  
  # 3). Gather Inputs/Outputs for Debugging ----
  AllInputs <- reactive({
    data.frame(Names = names(unlist(reactiveValuesToList(input, all.names = T))),
               Values = unlist((reactiveValuesToList(input, all.names = T))))
  })

  AllOut <- reactive({
    outs <- outputOptions(output)
    outs <- data.frame(unlist(lapply(names(outs), function(name) {
      outputOptions(output, name, suspendWhenHidden = FALSE)
    })))
    outs
  })

  output$show_outputs <- renderTable({AllOut()})
  output$show_inputs <- renderTable({AllInputs()})

  # 4). Cleanup objects if not on a certain tab. ----
  observe({
    if (input$tabs != "climtourTab") {
      if (exists("y2ytour")) {rm(y2ytour);isSwipemetric(NULL)}
    } else if (input$tabs != "patourTab") {
      if (exists("patour")) {rm(patour);isSwipepa(NULL)}
    } else if (input$tabs != "paexpTab") {
      if (exists("pas")) {rm(pas);multiSelected_pas(NULL);rpas(NULL)}
    } else if (input$tabs != "climexpTab") {
      if (exists("wds")) {rm(wds);multiSelected_wds(NULL);rwds(NULL)}
    }
  }, priority = 2)
     
  observeEvent(input$tabs,{
    # 5). Home Page Logic ----
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
      y2ytour <- callModule(tourPanel, "y2ytour", tourName = "y2y")
      shinyjs::click("y2ytour-stopBttn")
      callModule(ddownBttn,"y2ymapBttn") #Settings button on Y2Y Tour Map
      callModule(map, "y2ymap", swipe = F,OSM=F)
      leafletProxy("y2ymap-map") %>% setView(-122.8271,55.71267,5)
      isSwipemetric(F)
      observeEvent(y2ytour$id(),{runjs("window.scrollTo(0,0)")})
      observe({
        if (!is.null(input$"y2ymap-map_bounds")) {
          # print(input$"y2ymap-map_center")
          climtourView$view <- input$"y2ymap-map_bounds"}
        })
      observe({
        if (!is.null(input$"y2ymapBttn-opacity")) {
          climtourView$opacity <- input$"y2ymapBttn-opacity"}
      })
      observe({isSwipemetric(tourStep(mapid = "y2ymap",tourinfo = y2y,tourid = y2ytour$id(),rSwipe = isSwipemetric(),view = isolate(climtourView$view),
                                      shpdata=y2yshp, opac = climtourView$opacity,OSM=F))
        })
    
      observeEvent(input$"y2ymap-map_shape_click", {
        multiSelected(
          selectMultiPolys(mapId = "y2ymap-map",data = y2yshp[which(y2yshp$TOURID == y2ytour$id()),],
                           idfield = "TOURID", addPolys = T, newId = "mp_")
        )
      })
    }
    
    else if (input$tabs == "patourTab") {
      #7). PA Tour Logic ----
      patour <- callModule(tourPanel, "patour", tourName = "pa")
      shinyjs::click("y2ytour-stopBttn")
      callModule(map,"pamap", OSM = F)
      callModule(ddownBttn,"pamapBttn") #Settings button on Protected Areas Tour Map
      shinyjs::click("patour-stopBttn")
      observeEvent(patour$id(),{runjs("window.scrollTo(0,0)")})
      observeEvent(input$"pamap-map_shape_click", {
         multiSelected(
            selectMultiPolys(mapId = "pamap-map",data = pashp[which(pashp$TOURID == patour$id()),],
                             idfield = "TOURID", addPolys = T, newId = "mp_")
          )
      })
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
        proxy <- leafletProxy(mapId = "pamap-map")
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
      #8). Metric Explorer ----
      wds <- read_sf(wdfile)
      callModule(map, "climexpMap",OSM=F) # Climate Metrics Explorer Map server logic
      callModule(ddownBttn,"climexpMapBttn") #Drop Down Menu Button Server logic 
      proxy <- leafletProxy("climexpMap-map") # Represents the map so that we can make changes to it
      #8a) Add Ecoregion Polygons ----
      proxy %>% hideGroup("Place Labels") %>%
        setView(lng = -100, lat = 55, zoom = 3) %>%
        addPolygons(
          data = ecos,
          fillColor = NA,
          fillOpacity = 0,
          weight = 1,
          color = "blue",
          stroke = T,
          label = ~htmlEscape(ecos$NA_L3NAME),
          layerId = ~ecos$ecoreg3,
          group = "ecoregions",
          highlightOptions=highlightOptions(
            color="white",weight=1.5
          )
        )
      #8b) Add tiles----
      observe({
        proxy %>% 
          clearGroup("metrics") 
        if (input$climExpLayer != "") {
          data <- tilelist[tileName %in% input$climExpLayer,]
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
          )
        }
      })
      observeEvent(input$climFillPolys,{
        print(input$climFillPolys)
      })
      #8c) Get All button press in Outputs ----
      observeEvent(input$"climexp-getallSP",{
        multiSelected_wds(rwds())
        clickedIds$ids<-multiSelected_wds()$FIDNUM2
        mswds <- st_drop_geometry(multiSelected_wds()) %>% select(-c(1,11:14)) %>% mutate_at(2:9,funs(as.numeric))
        edata <- eregion$edata
        names(mswds) <- c("Name",'Intactness','Topodiversity','Forward Climatic Refugia','Backwards Climatic Refugia','Bird Refugia','Tree Refugia',
                          'Tree Carbon','Soil Carbon')
        spdat <- rbind(edata,mswds)
        callModule(appStarPlot,"climexp",data = spdat,namecol = "Name",removecols = NULL, live = F)
        proxy %>%
          addPolygons(
            data = multiSelected_wds(),
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
        callModule(appStarPlot,"climexp",data = eregion$edata,namecol = "Name",removecols = NULL, live = F)
        callModule(xyPlot,"climexp",data = rwds(),data2=NULL,namecol = "NEWNAME",offset=1)
      })
      
      #8e) Map Polygon Click Logic----
      observeEvent(input$"climexpMap-map_shape_click", {
        click <- input$"climexpMap-map_shape_click"
        #If the polygon is an ecoregion:
          # add starplot user interface
          # add return to ecoregions button
          # get the ecoregion data for the clicked polygon
        if (click$group == "ecoregions") {
          # 8e) i. If the clicked polygon is an ecoregion ----
          shinyjs::click(id = "climexpacc-2-heading")
          output$climexpStarplotDiv <- renderUI(div(id = "climExpStarPlot", appStarPlotUI("climexp", live = F,all=T,reset=T)))
          output$climexpXYplotdiv <- renderUI(div(id = "climExpXYPlot", xyPlotUI("climexp")))
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
          eregion$bds <- st_bbox(eregion$edata)
          #e) Get the watershed data for watersheds within that ecoregion.
          bds <- unname(eregion$bds)
          rwds(wds[wds$ecoreg3 == eregion$edata$ecoreg3,])
          foo <- rwds()
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
            clearGroup("ecoregions") %>%
              addPolygons(
                data = rwds(),
                fillColor = NA,
                fillOpacity = 0,
                weight = 1,
                color = "blue",
                stroke = T,
                label = ~htmlEscape(NEWNAME),
                layerId = ~FIDNUM2,
                group = "wds"
              )
          #e) Do some data wrangling to get the starplot to plot
          eregion$edata <- eregion$edata %>% select(intact3,elevdiv3,fwvelref3,bwvelref3,brdref3,treref3,treec3,soilc3) %>% st_drop_geometry() %>% mutate_all(funs(as.numeric))
          names(eregion$edata) <- c("intact","elevdiv","fwvelref" ,"bwvelref", "brdref","treref","treec","soilc")
          eregion$edata$NEWNAME <-  "Ecoregion Avg"
          eregion$edata <- rbind(eregion$edata,hucmin)
          names(eregion$edata) <- c('Intactness','Topodiversity','Forward Climatic Refugia','Backwards Climatic Refugia','Bird Refugia','Tree Refugia',
                            'Tree Carbon','Soil Carbon',"Name")
          callModule(appStarPlot,"climexp",data = eregion$edata,namecol = "Name",removecols = NULL, live = F)

          #8e) ii. If the clicked polygon is a watershed ----
        } else if (click$group == "wds" | click$group == "swds" ) {
          multiSelected_wds(
            selectMultiPolys(mapId = "climexpMap-map",data = rwds(),
                             idfield = "FIDNUM2", addPolys = T, newId = "mp_",nameField = "NEWNAME",group = "swds")
          )
          mswds <- st_drop_geometry(multiSelected_wds()) %>% select(-c(1,11:14)) %>% mutate_at(2:9,funs(as.numeric))
          edata <- eregion$edata
          names(mswds) <- c("Name",'Intactness','Topodiversity','Forward Climatic Refugia','Backwards Climatic Refugia','Bird Refugia','Tree Refugia',
                            'Tree Carbon','Soil Carbon')
          spdat <- rbind(edata,mswds)
          if (nrow(spdat) > 1){
            callModule(report,"climReport",polys=rwds(),data=wds,namecol="NEWNAME",pa=F)
            callModule(appStarPlot,"climexp",data = spdat,namecol = "Name",removecols = NULL, live = F)
          }
        }
      
      })
        #8f) If the back to ecoregions button is pressed ----
      observeEvent(input$climExpB2E,{
        removeUI(selector = "div:has(>#climExpB2E)", session = session)
        removeUI(selector = "div:has(>#climexp-appStarPlot)", session = session)
        shinyjs::click(id = "climexpacc-0-heading")
        output$climexpStarplotDiv <- NULL
        proxy <- leafletProxy("climexpMap-map") %>% hideGroup("Place Labels") %>%
          clearGroup("wds") %>%
          clearGroup("swds") %>%
          setView(lng = -100, lat = 55, zoom = 3) %>%
          addPolygons(
            data = ecos,
            fillColor = NA,
            fillOpacity = 0,
            weight = 1,
            color = "blue",
            stroke = T,
            label = ~htmlEscape(ecos$NA_L3NAME),
            layerId = ~ecos$ecoreg3,
            group = "ecoregions"
          )
      })
      #8g) Obersve xyplot inputs ----
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
    } else if (input$tabs == "paexpTab") {
    #9). PA Explorer Logic ----
      callModule(map, "paexpMap", OSM=F) #Protected Areas Explorer Map
      proxy <- leafletProxy("paexpMap-map") 
      callModule(ddownBttn,"paexpMapBttn") #Settings button on Protected Areas Explorer Map
      output$paexpXYplotdiv <- renderUI(div(id = "paExpXYPlot", xyPlotUI("paexp")))
      
      pas <- read_sf(pafile)
      pts <- read_sf(ptsfile)
      pas <- pas[-c(pas$PA_NAME == "Wildlife Habitat Protection"),]
      pts <- pts[-c(pts$PA_NAME == "Wildlife Habitat Protection"),]
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
        }
        return(data)
      })
      #9b) Add polygons to the map based on the view and scale.----
      observe({
        rpas(inBounds_PAs())
        if(!is.null(rpas())){
          if(!is.null(nrow(rpas()))){
            proxy %>%
              clearShapes() %>%
              addPolygons(
                data = rpas(),
                fillOpacity = 0.05,
                weight = 1,
                color = "blue",
                stroke = T,
                label = ~htmlEscape(PA_NAME),
                layerId = ~ gridcode,
                group= "pas",
                highlightOptions=highlightOptions(
                  color="white",weight=1.5)
              )
          }
        }
      })
      #9c) Clear all button press in Outputs----
      observeEvent(input$"paexp-resetSP",{
        multiSelected_pas(NULL)
        proxy %>% clearGroup('rpas')
        mspas<-NULL
        padat<-NULL
        clickedIds$ids<-NULL
        removeUI(selector = "div:has(>#paexp-appStarPlot)", session = session)
        shinyjs::click(id = "paexpacc-0-heading")
        output$paexpStarplotDiv <- NULL
      })
      #9d) Add tiles ----
      observe({
        proxy %>% 
          clearGroup("metrics")
        if (input$paExpLayer != "") {
          data <- tilelist[tileName %in% input$paExpLayer,]
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
          )
        }
      })
      #9e) Map polygon click logic ----
      #9e) i. Expand the output section if there are any selected polygons 
      observeEvent(input$"paexpMap-map_shape_click", {
        if (!is.null(multiSelected_pas())) {
          if(nrow(multiSelected_pas())==0){
          shinyjs::click(id = "paexpacc-2-heading")
          }
        }
      })
      #9e) ii. Expand the output section the first time a polygon is clicked
      observeEvent(input$"paexpMap-map_shape_click", {
        shinyjs::click(id = "paexpacc-2-heading")
      },once=T)
      
      #9e) iii. Get the protected areas that have been clicked, do some data wrangling and create the starplot.
      observeEvent(input$"paexpMap-map_shape_click", {
        output$paexpStarplotDiv <- renderUI(div(id = "paExpStarPlot", appStarPlotUI("paexp", live = F,all=F,reset=T)))
        multiSelected_pas(
          selectMultiPolys(mapId = "paexpMap-map",data = pas,
                           idfield = "gridcode", addPolys = T, newId = "mp_",nameField = "PA_NAME",group = "rpas")
        )
        
        mspas <- st_drop_geometry(multiSelected_pas()) %>% select(c(4:11,15)) %>% mutate_at(1:8,funs(as.numeric))
        names(mspas) <- c('Intactness','Topodiversity','Forward Climatic Refugia','Backwards Climatic Refugia','Bird Refugia','Tree Refugia',
                        'Tree Carbon','Soil Carbon',"Name")
        padat <- rbind(paminmax,mspas)
        if (nrow(padat) > 1){
          callModule(report,"paReport",polys=multiSelected_pas(),data=pas)
          callModule(appStarPlot,"paexp",data = padat,namecol = "Name",removecols = NULL, live = F)
  
        }
      })
      
      #9e) iv. Logic to handle if no polygons are selected but were in the past
      observe(
        if (!is.null(multiSelected_pas())) {
          if (nrow(multiSelected_pas()) == 0) {
            removeUI(selector = "div:has(>#paexp-appStarPlot)", session = session)
            shinyjs::click(id = "paexpacc-0-heading")
            output$paexpStarplotDiv <- NULL
          } 
        }
      )
      
      
      observeEvent(multiSelected_pas(),{
        observeEvent(input$"paexp-X",{
          if (is.null(multiSelected_pas())) {
            # print("IS NULL")
            callModule(xyPlot,"paexp",data=pas,
                       data2= NULL, namecol="PA_NAME",offset=0)
          } else {  
            callModule(xyPlot,"paexp",data=pas,
                       data2= multiSelected_pas(), namecol="PA_NAME",offset=0)
          }
        })
        observeEvent(input$"paexp-Y",{
          if (is.null(multiSelected_pas())) {
            # print("IS NULL")
            callModule(xyPlot,"paexp",data=pas,data2=NULL,namecol="PA_NAME",offset=0)
          } else {  
            callModule(xyPlot,"paexp",data=pas,data2=multiSelected_pas(),namecol="PA_NAME",offset=0)
          }
        })
      })
    }
  })

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
    if(isTRUE(tokenStatus)){
      drop_upload(file = './report_stats/downloadfrequency.csv',dtoken=token)
      drop_upload(file = "./report_stats/reportFrequency.csv",dtoken=token)
      drop_upload(file = "./report_stats/interactiveFrequency.csv",dtoken=token)
      drop_upload(file = "./report_stats/paFrequency.csv",dtoken=token)
      drop_upload(file = "./report_stats/downloads.csv",dtoken=token)
      file.remove(list.files(path="./report_stats",pattern=".csv",full.names=T))
    }
    reps_size<-sum(file.info(list.files(path=reportdir,all.files=T,recursive=T,full.names=T))$size)
    if(reps_size>1E4){
      print(utils:::format.object_size(reps_size, units="MB"))
      # dirs<-list.dirs(reptmpdir,full.names=T,recursive = F)
      # htmlfiles<-list.files(path=reptmpdir,full.names=T,recursive=F,pattern=".html")
      # pngs<-list.files(path=repimgdir,full.names=T,recursive=F,pattern=".png")
      #Now compare to the frequency list downloaded and updated and then drop the ones that are used least often.
      #I'm hoping this is never kicked on but we don't really want to get too big now do we.
    }
  })
}
