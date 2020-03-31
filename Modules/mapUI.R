# mapUI.R
mapUI <- function(id,mapbox = F) {
  ns <- NS(id)
  if (isFALSE(mapbox)) {
    tagList(
        leafletOutput(
          outputId = ns("map"), 
          width = "100%", 
          height = "100%"
      )
    )
  }
}