#AdaptWestApp server.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 0.1
#Notes
source("global.R")

function(input, output, session) {
  # LOGIN MENU ----
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
        removeModal()
      }
    })
  }
  # Common Reactives ----
  clickedIds <- reactiveValues(ids = vector())
  multiSelected <- reactiveVal()
  multiSelected_pas <- reactiveVal(c(NULL))
  multiSelected_wds <- reactiveVal(c(NULL))
  rwds <- reactiveVal(NULL)
  rpas <- reactiveVal(NULL)
  climtourView <- reactiveValues(view = NULL,opacity = NULL)
  patourView <- reactiveValues(view = NULL,opacity = NULL)
  isSwipemetric <- reactiveVal(NULL)
  isSwipepa <- reactiveVal(NULL)
  # Functions ----
  

  
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
    
    #Do we actually want to add the polygon to the map? or just capture them?
    if (isTRUE(addPolys)) {
      proxy %>%
        addPolygons(
          data = multipolys,
          fillColor = "red",
          weight = 5,
          color = "black",
          stroke = T,
          label = ~paste0(newId,multipolys[[nameField]]),
          layerId = paste0(newId,multipolys[[idfield]]),
          group = group
        )
    }
    return(multipolys)
  }
  
  # gClip <- function(sp, bb) {
  #   b_poly <-
  #     as(extent(bb), "SpatialPolygons") #spatial extent of the map window
  #   b_poly@proj4string <-
  #     #CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #Make sure it knows its WGS84
  #     CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #   #ms_simplify(raster::intersect(sp, b_poly))
  #   b_poly <- gBuffer(b_poly, byid = TRUE, width = 0.000001)
  #   sp <- gBuffer(sp, byid = T, width = 0.000001)
  #   raster::intersect(sp, b_poly)
  # }
  
  # Gather Inputs/Outputs for Debugging ----
  AllInputs <- reactive({
    data.frame(Names = names(unlist(reactiveValuesToList(input, all.names = T))),
               Values = unlist((reactiveValuesToList(input, all.names = T))))
  })

  AllOut <- reactive({
    outs <- outputOptions(output)
    outs <- data.frame(unlist(lapply(names(outs), function(name) {
      outputOptions(output, name, suspendWhenHidden = FALSE)
    })))
    print(outs)
    outs
  })

  output$show_outputs <- renderTable({AllOut()})
  output$show_inputs <- renderTable({AllInputs()})

  # Cleanup objects if not on a certain tab. ----
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
  },priority = 2)
     
  observeEvent(input$tabs,{
    #Home Page Logic ----
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
    # Y2Y TOUR----
      #Y2Y Tour
      y2ytour <- callModule(tourPanel, "y2ytour", tourName = "y2y")
      shinyjs::click("y2ytour-stopBttn")
      callModule(ddownBttn,"y2ymapBttn") #Settings button on Y2Y Tour Map
      callModule(map, "y2ymap", swipe = F,OSM=F)
      leafletProxy("y2ymap-map") %>% setView(-122.8271,55.71267,5)
      isSwipemetric(F)
      observeEvent(y2ytour$id(),{runjs("window.scrollTo(0,0)")})
      observe({
        if (!is.null(input$"y2ymap-map_bounds")) {
          print(input$"y2ymap-map_center")
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
      #PATour ----
      patour <- callModule(tourPanel, "patour", tourName = "pa")
      shinyjs::click("y2ytour-stopBttn")
      callModule(map,"pamap", OSM = T)
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
                                opac=patourView$opacity, OSM = T))
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
      #Climate Metric Explorer ----
      callModule(map, "climexpMap",OSM=F) # Climate Metrics Explorer Map
      callModule(ddownBttn,"climexpMapBttn") #Settings button on Climate Metrics Explorer Map
      wds <- read_sf("./Data/wds_comp.gpkg")
      proxy <- leafletProxy("climexpMap-map") %>% hideGroup("Place Labels") %>%
        setView(lng = -100, lat = 55, zoom = 3) %>%
        addPolygons(
          data = ecos,
          fillColor = NA,
          fillOpacity = 0,
          weight = 1,
          color = "blue",
          stroke = T,
          label = ~htmlEscape(ecos$NA_L3NAME),
          layerId = ~ecos$OID,
          group = "ecoregions"
        )
      observe({
        proxy %>% 
          clearGroup("metrics") 
        if (input$climExpLayer != "") {
          data <- tilelist[tileName %in% input$climExpLayer,]
          print(data)
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
      observeEvent(input$"climexpMap-map_shape_click", {
        click <- input$"climexpMap-map_shape_click"
        print(click)
        edata <- ecos[which(ecos$OID == click$id),]
         #To be replaced by a simpler filter based on ecoregion id.
        bds <- st_bbox(edata)
        if (click$group == "ecoregions") {
          pdata <- st_intersection(wds,edata)
          rwds(pdata)
          proxy %>% 
              fitBounds(lng1 = bds$xmin,lat1 = bds$ymin,lng2 = bds$xmax,lat2 = bds$ymax) %>%
              clearGroup("ecoregions") %>%
              addPolygons(
                data = pdata,
                fillColor = NA,
                fillOpacity = 0,
                weight = 1,
                color = "blue",
                stroke = T,
                label = ~htmlEscape(NEWNAME),
                layerId = ~FIDNUM2,
                group = "wds"
              )
        } else if (click$group == "wds" | click$group == "swds" ) {
          multiSelected_wds(
            selectMultiPolys(mapId = "climexpMap-map",data = rwds(),
                             idfield = "FIDNUM2", addPolys = T, newId = "mp_",nameField = "NEWNAME",group="swds")
          )
          print(multiSelected_wds())
          callModule(appStarPlot,"climexp",data = multiSelected_wds(),namecol = "NEWNAME",removecols = c(2,11:16))
        }
        # proxy %>% 
        #   fitBounds(lng1=bds$xmin,lat1=bds$ymin,lng2=bds$xmax,lat2=bds$ymax) %>%
        #   addPolygons(
        #     data = pdata,
        #     fillColor = NA,
        #     fillOpacity = 0,
        #     weight = 1,
        #     color = "blue",
        #     stroke = T,
        #     label = ~htmlEscape(NEWNAME),
        #     layerId = ~FIDNUM2,
        #     group="ecoregions")
      })
      # observeEvent(input$"climexpMap-map_shape_click", {
      
    } else if (input$tabs == "paexpTab") {
    #Protected Area Explorer ----
      inBounds_PAs <- reactive({
        if (is.null(input$"paexpMap-map_zoom"))
          return(pas[1,])
        # if (is.null(input$"paexpMap-map_bounds"))
        #   return(pas[1,])
        # bbx <- input$"paexpMap-map_bounds"
        # print(bbx)
        # if(bbx$west <= -180.) {bbx$west = -180}
        # if(bbx$east >= 180.) {bbx$east = 180}
        # clip <- st_sfc(st_polygon(
        #   x = list(rbind(
        #     c(bbx$west,bbx$south),
        #     c(bbx$east,bbx$south),
        #     c(bbx$east,bbx$north),
        #     c(bbx$west,bbx$north),
        #     c(bbx$west,bbx$south)
        #     )
        #   )
        # ),check_ring_dir = T)
        # st_crs(clip) = 4326
        data <- pas[pas$AREA >= zoomcuts[input$"paexpMap-map_zoom"],]
        # data <- st_intersection(data,clip)
        if (length(data) < 1) {
          data <- pas[1,]
        } 
        return(data)
        # return(pas[1,])
      })
      callModule(map, "paexpMap", OSM=F) #Protected Areas Explorer Map
      proxy <- leafletProxy("paexpMap-map") #%>% setView(lng = -100, lat = 55, zoom = 3)
      callModule(ddownBttn,"paexpMapBttn") #Settings button on Protected Areas Explorer Map
      # callModule(appStarPlot,"paexp",data=pas,names="PA_NAME",removecols=c(2,4:6))
      # shinyjs::onclick("cmexplink",  updateNavbarPage(session, inputId = "tabs", selected = "climexpTab"))
      # shinyjs::onclick("cmtourlink",  updateNavbarPage(session, inputId = "tabs", selected = "climtourTab"))
      pas <- read_sf("./Data/napamerc_comp.gpkg")
      pas <- pas[-c(pas$PA_NAME == "Wildlife Habitat Protection"),]
      observe({
        rpas(inBounds_PAs())
        print(multiSelected_pas())
        proxy %>%
          clearShapes() %>%
          addPolygons(
            data = rpas(),
            fillColor = ~colorFactor("Set3",IUCN_CAT),
            fillOpacity = 0.5,
            weight = 1,
            color = "blue",
            stroke = T,
            label = ~htmlEscape(PA_NAME),
            layerId = ~ Id,
            group="pas"
          )
        # }
      })

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
      observeEvent(input$"paexpMap-map_shape_click", {
        multiSelected_pas(
          selectMultiPolys(mapId = "paexpMap-map",data = rpas(),
                           idfield = "Id", addPolys = T, newId = "mp_",nameField = "PA_NAME",group = "rpas")
        )
      # if (nrow(multiSelected_pas()) >= 1) {
      #   proxy %>%
      #       clearGroup("papolys") %>%
      #       addPolygons(
      #         data = pas[pas$ecoreg1 == multiSelected_pas()$ecoreg1,],
      #         fillColor = ~colorFactor("Set3",IUCN_CAT),
      #         fillOpacity = 0.5,
      #         weight = 1,
      #         color = "blue",
      #         stroke = T,
      #         label = ~htmlEscape(PA_NAME),
      #         layerId = ~ Id
      #       )
      # }
      # print(head(multiSelected_pas()))
      # callModule(appStarPlot,"paexp",data=multiSelected_pas(),names="PA_NAME",removecols=c(2:7))
      })
    }
  })
}
