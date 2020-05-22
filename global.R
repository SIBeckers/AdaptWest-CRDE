#AdaptWestApp - global.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 1.0.0
#Notes
# - Let's enable bookmarking (could be useful to people)
#     - we will use URL bookmarking so that people can share a URL. 
# - Let's try using name states and render UI function to modularize the app
#
#SETUP OPTIONS (GO THROUGH) ----
#Detect if we are on shinyapps.io linux machine or a local machine
if(Sys.info()[['sysname']] == 'Linux'){
  reportStatsStatus = T  #Keep track of stats globally (T) (e.g. from Dropbox) or locally (F)
  webshot::install_phantomjs() #Needed for ShinyApps.io instance but not to run locally.
} else {
  reportStatsStatus = F  #Keep track of stats globally (T) (e.g. from Dropbox) or locally (F)
}
options(shiny.reactlog = F) #For reactivity logging and debugging locally.
options(shiny.jquery.version = 1) #Not sure this is needed but did see some issues with plotly early on when shiny switched to the new jquery.

appURL = "https://adaptwest.shinyapps.io/climate-resilience-data-explorer" #Where the app can be found.

# Libraries ----
  #Shiny stuff
  library(shiny)
  library(shinyWidgets)
  library(shinythemes)
  library(htmltools)
  library(htmlwidgets)
  library(bsplus)
  library(shinyjs)
  library(webshot)
  library(flextable)
  library(shinycssloaders)
  library(tinytex)

  #Map Stuff
  library(leaflet)
  library(leafem)
  library(leaflet.extras)
  library(sf)
  library(mapview)

  #Markdown Stuff
  library(markdown)
  library(knitr)
  library(rmarkdown)
  library(kableExtra)

  #Data and wrangling Stuff
  library(data.table)
  library(dplyr)
  library(stringr)
  library(rdrop2)
  library(utils)
  library(readr)
  library(tibble)
  library(scales)
  library(tidyselect)

  #Plot Stuff
  library(plotly)
  library(ggplot2)
  library(ggiraphExtra)
  library(viridis)
  library(scales)
  library(RColorBrewer)
  library(cowplot)

# Modules ----
  source("./Modules/tourPanelUI.R") #The story map user interface
  source("./Modules/tourPanel.R") #The story map server code
  source("./Modules/mapUI.R")
  source("./Modules/map.R")
  source("./Modules/ddownBttnUI.R")
  source("./Modules/ddownBttn.R")
  source("./Modules/appStarPlotUI.R")
  source("./Modules/appStarPlot.R")
  source("./Modules/xyPlotUI.R")
  source("./Modules/xyPlot.R")
  source("./Modules/reportUI.R")
  source("./Modules/report.R")
  source("./Modules/Table.R")
  source("./Modules/TableUI.R")

# Other Code -----
  source("www/code/tourStep.R")
  source("www/code/myicon.R")
  source("www/code/radarplot.R")
  source("www/code/xyplot.R")
  source("www/report/mapFunction.R")


# Global Resources ----
  aw_gh <- "https://sibeckers.github.io/AdaptWest-CRDE/" #github link
  aw_tw <- "https://twitter.com/adaptwest"
  enableBookmarking(store = "url")
  
  
# Fonts ----
  if(Sys.info()[['sysname']] == 'Linux'){
    dir.create('~/.fonts')
    file.copy("www/fonts/Roboto regular.ttf", "~/.fonts")
    file.copy("www/fonts/Roboto 500.ttf", "~/.fonts")
    file.copy("www/fonts/Work Sans 700.ttf", "~/.fonts")
    system('fc-cache -f ~/.fonts')
  }
  
  
# Protected Areas Data ---- 
  pafile = "./Data/pas.gpkg"
  ptsfile = "./Data/pas_centroids.gpkg"
  panames<-readRDS("./Data/panames.Rds")
  pas <- read_sf(pafile)
  pts <- read_sf(ptsfile)
  
  
# Watersheds Data ----
  wdfile = "./Data/wds.gpkg"
  wds <- read_sf(wdfile)
  
  
# Ecoregion Data ----
  ecos <- read_sf('./Data/ecos.gpkg')
  ecol1stats <- fread(file = "./Data/ecoregionlevel1mean.csv",
                      colClasses=list(character=1,numeric=2:9))
  ecol2stats <- fread(file = "./Data/ecoregionlevel2mean.csv",
                      colClasses = list(character = 1, numeric = 2:9))
  ecol3stats <- fread(file = "./Data/ecoregionlevel3mean.csv",
                      colClasses = list(character = 1, numeric = 2:9))
  l1min <- fread(file = "./Data/ecoregionlevel1min.csv", 
                 colClasses = list(numeric = 1:9))
  l1max <- fread(file = "./Data/ecoregionlevel1max.csv", 
                 colClasses = list(numeric = 1:9))
  l3min <- fread(file = "./Data/ecoregionlevel3min.csv", 
                 colClasses = list(numeric = 1:9))
  l3max <- fread(file = "./Data/ecoregionlevel3max.csv", 
                 colClasses = list(numeric = 1:9))
  eco1namekey <- fread(file = "./Data/eco1vals.csv",
                       colClasses = list(numeric = 1, character = 2))
  eco2namekey <- fread(file = "./Data/eco2vals.csv",
                       colClasses = list(numeric = 1, character = 2))                   
  eco3namekey <- fread(file = "./Data/eco3vals.csv",
                       colClasses = list(numeric = 1, character = 2))
  
  
# Global Map Settings ----
  minZoom = 0
  maxZoom = 9
  zoomcuts <- c(17500, 10000, 7500, 5000, 1000, 100, 10, 1, 1, 0)
  
  
# Tile info for online tiles: COMMENT OUT FOR LOCAL TILES----
  tiledir <- "http://www.cacpd.org.s3-website-us-west-2.amazonaws.com/tiledirectory" #FOR ONLINE TILES
  tilelist <- fread("./config_files/tilelist.txt") #Replaces above two lines.
  tilelist$tileSubdir <- file.path(tiledir, tilelist$tileSubdir) #FOR ONLINE TILES
  tilevect <- tilelist$tileName
  names(tilevect) <- tilelist$tileGroup
  metriclist <- c("intact", "elevdiv", "fwvelref", "bwvelref", "brdref", 
                  "treref", "treec", "soilc", "NEWNAME")
  names(metriclist) <- c('Intactness', 'Topodiversity', 
                         'Forward Climatic Refugia', 
                         'Backward Climatic Refugia', 'Bird Refugia', 
                         'Tree Refugia', 'Tree Carbon', 'Soil Carbon', "Name")
  polyfillvect <- c("elevdiv", "fwvelref", "bwvelref", "brdref", "treref", 
                    "treec", "soilc", "intact")
  names(polyfillvect) <- c("Topodiversity", "Forward Climatic Refugia",
                           "Backward Climatic Refugia", "Bird Refugia", 
                           "Tree Refugia", "Tree Carbon", "Soil Carbon",
                           "Intactness")                    
  
  
# Climate Metrics Tour Data -----
  y2yshp <- read_sf("./Data/parks9y2y2.gpkg")
  y2ybds <- st_bbox(y2yshp)
  y2y <- fread("./config_files/metrictourinputs.csv", na.strings = "")
  y2y$zoomTo <- as.logical(y2y$zoomTo)
  y2y$clear <- as.logical(y2y$clear)
  y2y$tourFileName <- file.path("./www/md/metrictour", y2y$tourFileName)
  y2y$tile1url <- ifelse(
    !is.na(y2y$tile1), #Test
    file.path(tiledir, paste0(y2y$tile1, "/{z}/{x}/{y}.png")), #if TRUE
    NA) #if FALSE
  y2y$tile2url <- ifelse(
    !is.na(y2y$tile2), #test
    file.path(tiledir, paste0(y2y$tile2, "/{z}/{x}/{y}.png")), #if TRUE
    NA) #if FALSE
  y2y$swipe <- ifelse(!is.na(y2y$tile1) & !is.na(y2y$tile2), T, F)
  
  
# Protected Areas Tour Data ----
  pashp <- y2yshp
  pa <- fread("./config_files/patourinputs.csv", na.strings = "")
  pa$zoomTo <- as.logical(pa$zoomTo)
  pa$clear <- as.logical(pa$clear)
  pa$tourFileName <- file.path("./www/md/patour", pa$tourFileName)
  pa$tile1url <- ifelse(
    !is.na(pa$tile1), #test
    file.path(tiledir, paste0(pa$tile1, "/{z}/{x}/{y}.png")), #if TRUE
    NA) #if FALSE
  pa$tile2url <- ifelse(
    !is.na(pa$tile2), #test
    file.path(tiledir, paste0(pa$tile2, "/{z}/{x}/{y}.png")), #if TRUE
    NA) #if FALSE
  pa$swipe <- ifelse(!is.na(pa$tile1) & !is.na(pa$tile2), T, F)
  
  
# Setup leaflet sidebyside plugin ----
  myLeafletSideBySidePlugin <- htmlDependency(
    name = "leaflet-side-by-side", version = "2.0.0",
    src = c("www/shared/leaflet-side-by-side-gh-pages"),
    script = "leaflet-side-by-side.js")

  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }

  
#Common Reactive Variables ----
  polygroup <- reactiveVal("ecoregions")
  clickedIds <- reactiveValues(ids = vector())
  multiSelected_wds <- reactiveVal(NULL)
  rwds <- reactiveVal(NULL)
  eregion <- reactiveValues(edata = NULL,bds = NULL)
  clickedIdsPAs <- reactiveValues(ids = vector())
  multiSelected_pas <- reactiveVal(NULL)
  rpas <- reactiveVal(NULL)
  isSwipemetric <- reactiveVal(NULL)
  isSwipepa <- reactiveVal(NULL)

  
#Report stuff -----
  reportdir<-"./www/report"
  reptmpdir<-"./www/report/tmp"
  repimgdir<-"./www/report/imgs"
  repstaticdir<-"./www/report/static_reports"
  repinterdir<-"./www/report/interactive_reports"
  #Configure the report output using /config_files/reportconfig.csv. 
  #Note that parameters in this file must also be in /www/report/report_template.Rmd
  reportconfig <- as.list(dcast(melt(fread("./config_files/reportconfig.csv"), id.vars = "parameter"), variable ~ parameter)[,-1])
  
  #Report Status Stuff
  if(reportStatsStatus==T){
    if(file.exists("./report_stats/token.rds")){
      token <- readRDS("./report_stats/token.rds")
      mydownloads <- drop_read_csv("downloads.csv",dest="./report_stats/",
                     dtoken=token)
      print(head(mydownloads))
    } else {
  
      if(!dir.exists("./report_stats")){dir.create("./report_stats")}
      mydownloads <- data.table(Name=character(),Date=numeric(),Interactive=integer(),Format=character(),ProtectedArea=integer(),stringsAsFactors = F)
    }
  } else {
    if(!dir.exists("./report_stats")){dir.create("./report_stats")}
    mydownloads <- data.table(Name=character(),Date=numeric(),Interactive=integer(),Format=character(),ProtectedArea=integer(),stringsAsFactors = F)
  }