#AdaptWestApp - global.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 0.1
#Notes
# - Let's enable bookmarking (could be useful to people)
#     - we will use URL bookmarking so that people can share a URL. 
# - Let's try using name states and render UI function to modularize the app
#
# Libraries ----
library(shinythemes)
library(mapview)
library(shiny)
library(shinyWidgets)
library(htmltools)
library(htmlwidgets)
library(bsplus)
library(leaflet)
library(leafem)
library(markdown)
library(knitr)
library(rmarkdown)
library(sf)
library(plotly)
library(leaflet.extras)
library(shinyjs)
library(data.table)
library(dplyr)
library(stringr)
library(kableExtra)
library(rdrop2)
library(utils)
library(readr)
library(webshot)
library(tibble)
library(scales)
library(ggplot2)
library(ggiraphExtra)
library(viridis)
library(scales)
library(combinat)
# webshot::install_phantomjs()
options(shiny.jquery.version = 1)


# Global Resources ----
aw_gh <- "https://github.com/SIBeckers/AdaptWest" #github link
aw_tw <- "https://twitter.com/adaptwest"
enableBookmarking(store = "url")
theuser = "AdaptWest"
thepassword <- "DataBasin2019"
loginMenu = F

# Protected Areas Data ---- 
pafile = "./Data/pas_20191230.gpkg"
ptsfile = "./Data/pas_centroids_20191230.gpkg"
paminmax <- fread('./Data/paminmax.csv')
paminmax$Name = c("MIN","MAX")
names(paminmax) <- c('Intactness','Topodiversity','Forward Climatic Refugia','Backwards Climatic Refugia','Bird Refugia','Tree Refugia',
                     'Tree Carbon','Soil Carbon',"Name")

# Watersheds Data ----
wdfile = "./Data/wds_20191230.gpkg"

# Ecoregion Data ----
ecos <- read_sf('./Data/ecos_20191230.gpkg')
ecol1stats <- fread("./Data/ecoregionlevel1mean.csv",colClasses=list(character=1,numeric=2:9))
ecol2stats <- fread("./Data/ecoregionlevel2mean.csv",colClasses=list(character=1,numeric=2:9))
ecol3stats <- fread("./Data/ecoregionlevel3mean.csv",colClasses=list(character=1,numeric=2:9))
hucmin <- fread("./Data/hucminmax.csv")
hucmin$NEWNAME = c("MIN","MAX")

# Map Settings ----
minZoom = 0
maxZoom = 9
zoomcuts <- c(20000, 15000, 15000, 10000, 1000, 100, 10, 1, 1,0)


# Tile info for online tiles: COMMENT OUT FOR LOCAL TILES----
tiledir <- "http://www.cacpd.org.s3-website-us-west-2.amazonaws.com/tiledirectory" #FOR ONLINE TILES
tilelist <- fread("./config_files/tilelist.txt") #Replaces above two lines.
tilelist$tileSubdir <- file.path(tiledir, tilelist$tileSubdir) #FOR ONLINE TILES
tilevect <- tilelist$tileName
names(tilevect) <- tilelist$tileGroup
metriclist <-names(hucmin)
names(metriclist) <- c('Intactness','Topodiversity','Forward Climatic Refugia',
                       'Backwards Climatic Refugia','Bird Refugia','Tree Refugia',
                       'Tree Carbon','Soil Carbon',"Name")

# Climate Metrics Tour Data -----
y2yshp <- read_sf("./Data/parks9y2y2.gpkg")
y2ybds <- st_bbox(y2yshp)
y2y <- fread("./config_files/metrictourinputs.csv",na.strings = "")
y2y$zoomTo <- as.logical(y2y$zoomTo)
y2y$clear <- as.logical(y2y$clear)
y2y$tourFileName <- file.path("./www/md/metrictour",y2y$tourFileName)
y2y$tile1url <- ifelse(!is.na(y2y$tile1),file.path(tiledir,paste0(y2y$tile1,"/{z}/{x}/{y}.png")),NA)
y2y$tile2url <- ifelse(!is.na(y2y$tile2),file.path(tiledir,paste0(y2y$tile2,"/{z}/{x}/{y}.png")),NA)
y2y$swipe <- ifelse(!is.na(y2y$tile1) & !is.na(y2y$tile2),T,F)

# Protected Areas Tour Data ----
pashp <- y2yshp
pa <- fread("./config_files/patourinputs.csv",na.strings = "")
pa$zoomTo <- as.logical(pa$zoomTo)
pa$clear <- as.logical(pa$clear)
pa$tourFileName <- file.path("./www/md/patour",pa$tourFileName)
pa$tile1url <- ifelse(!is.na(pa$tile1),file.path(tiledir,paste0(pa$tile1,"/{z}/{x}/{y}.png")),NA)
pa$tile2url <- ifelse(!is.na(pa$tile2),file.path(tiledir,paste0(pa$tile2,"/{z}/{x}/{y}.png")),NA)
pa$swipe <- ifelse(!is.na(pa$tile1) & !is.na(pa$tile2),T,F)


# Tile info for local tiles: UNCOMMENT FOR LOCAL TILES: -----
# tiledir = "C:/Users/jbeckers/Data/AdaptWestApp_TMPDIR/tiles" #USE THIS FOR LOCAL TILES
# tilelist <- fread("./tilelist.txt")
# tilelist$tileSubdir <- file.path(tiledir,tilelist$tileName) #FOR LOCAL TILES
# tilevect <- tilelist$tileName
# names(tilevect) <- tilelist$tileGroup
# for (i in 1:nrow(tilelist)) {
#   addResourcePath(prefix=tilelist$tileName[i],directoryPath = tilelist$tileSubdir[i])
#   tilelist$tileSubdir[i] <- paste0("/",tilelist$tileName[i],"/{z}/{x}/{y}.png")
# }
# y2y$tile1url <- paste0("/",y2y$tile1,"/{z}/{x}/{y}.png")
# y2y$tile2url <- paste0("/",y2y$tile2,"/{z}/{x}/{y}.png")
# pa$tile1url <- paste0("/",pa$tile1,"/{z}/{x}/{y}.png")
# pa$tile2url <- paste0("/",pa$tile2,"/{z}/{x}/{y}.png")


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

# Other Code -----
source("www/code/tourStep_v2.R")
source("www/code/myicon.R")
source("www/code/radarplot.R")
source("www/code/xyplot.R")
source("./www/code/colours.R")
source("./www/report/mapFunction.R")
# Setup leaflet sidebyside plugin ----
myLeafletSideBySidePlugin <- htmlDependency("leaflet-side-by-side","2.0.0",
                                          src = c("www/shared/leaflet-side-by-side-gh-pages"),
                                          script = "leaflet-side-by-side.js")


registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}




#List of resources for each tab, used to remove from other tabs/reload. I think this will speed things up. ----
#Not even being implemented right now, so still performance gains to be made I think.
# homeTablist <- list()
# metricTourlist <- list()
# metricExplist <- list("ecos","ecol1stats","ecol2stats","ecol3stats","wds")
# patourlist <- list("pa")
# paexplist <- list("pas")
# globallist <- list("aw_gh","aw_tw","minZoom","maxZoom","theuser","thepassword","zoomcuts","tiledir",
#                  "tilelist","tilevect","LeafletSideBySidePlugin","registerPlugin","tourStep")

#Report stuff -----
reportdir<-"./www/report"
reptmpdir<-"./www/report/tmp"
repimgdir<-"./www/report/imgs"
#Configure the report output using /config_files/reportconfig.csv. 
#Note that parameters in this file must also be in /www/report/report_template.Rmd
reportconfig <- as.list(dcast(melt(fread("./config_files/reportconfig.csv"), id.vars = "parameter"), variable ~ parameter)[,-1])

#Report Status Stuff
if(file.exists("./report_stats/token.rds")){
  token <- readRDS("./report_stats/token.rds")
  mydownloads <- drop_read_csv("downloads.csv",dest="./report_stats/",dtoken=token)
  tokenStatus=T
} else {
  tokenStatus=F
  mydownloads <- data.table(Name=character(),Date=numeric(),Interactive=integer(),Format=character(),ProtectedArea=integer(),stringsAsFactors = F)
}


# onStop(function() {
#   frequency <- mydownloads %>% group_by(Name) %>% tally()
#   write_csv(frequency,"./report_stats/downloadfrequency.csv")
#   write_csv(mydownloads,"./report_stats/downloads.csv")
#   drop_upload(file = './report_stats/downloadfrequency.csv',dtoken=token)
#   drop_upload(file = "./report_stats/downloads.csv",dtoken=token)
#   reps_size<-sum(file.info(list.files(path=reportdir,all.files=T,recursive=T,full.names=T))$size)
#   if(reps_size>1E4){
#     print(utils:::format.object_size(reps_size, units="MB"))
#     # dirs<-list.dirs(reptmpdir,full.names=T,recursive = F)
#     # htmlfiles<-list.files(path=reptmpdir,full.names=T,recursive=F,pattern=".html")
#     # pngs<-list.files(path=repimgdir,full.names=T,recursive=F,pattern=".png")
#     #Now compare to the frequency list downloaded and updated and then drop the ones that are used least often.
#     #I'm hoping this is never kicked on but we don't really want to get too big now do we.
#   }
# })