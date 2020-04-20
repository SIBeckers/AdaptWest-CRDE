#This little script will add a new pa to the existing pas geopackage. 
#Works if this is done when currenet working directory is the app directory,
#for more robustness put in the full path to pasfile and newpafile

#Filename of new file and path to existing file.
newpaFile = ""
pasfile = "./Data/pas.gpkg"
pasCentroidFile = "./Data/pas_centroids.gpkg"
pasNamesFile = "./Data/panames.Rds"
library(sf)
library(dplyr)

pas<-read_sf(pasfile)
newpaFile<-read_sf(newpaFile)
pas<-bind_rows(pas,newpaFile)
pas<-pas %>% arrange(desc(AREA))
pas_centroids<-sf::st_centroids(pas)
panames<-pas$PA_NAME

write_sf(pas,pasfile,append=F,delete_layer=T,overwrite=T)
write_sf(pas_centroids,"./Data/pas_centroids.gpkg",overwrite=T,delete_layer=T,append=F)
saveRDS(panames,"./Data/panames.Rds",overwrite=T,ascii=F,version=3,compress=F)