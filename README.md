[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3824539.svg)](https://doi.org/10.5281/zenodo.3824539)

# AdaptWest Climate Resilience Data Explorer Application Code
Conservation planners and practitioners increasingly wonder how they should revise conservation strategies in the face of the unprecedented threat to biodiversity from climate change. One key step is to consider the relatively consistent spatial patterns that characterize the “geography of climate exposure and resilience”. A qualitative understanding of these patterns can help planners craft conservation strategies that are more resilient to uncertainty regarding the future intensity of climate change.</span></p>

The Climate Resilience Data Explorer is a web application that allows users to view, summarize, and generate reports regarding 10 key metrics related to climate resilience. The goals of this Climate Resilience Data Explorer are to familiarize users with climate resilience concepts, explore information resources that can help conservation practitioners enhance climate resilience and connectivity and address climate change threats in the context of regional planning, and to support conservation management that increases the adaptive capacity of the landscape and its ability to support native species and ecosystems into the future by protecting areas which play a key role in facilitating climate adaptation and mitigation.

The web application consists of two type of modules. The two explorers allow viewing and report generation summarizing metrics for North America's watersheds and management areas, respectively. The tours, located below the explorers on the app's home page, provide tutorials on how the metrics are generated and how they can be interpreted.

**This application works best when viewed through a modern web-browser such as Chrome, Microsoft Edge, or Firefox. The application is still being developed and updated. Please check back frequently for the latest version.**

## About this repository
This repository contains the R code, javascript, html, and other files needed to create the AdaptWest Climate Resilience Data Explorer application.
The application can be found at [https://adaptwest.shinyapps.io/climate-resilience-data-explorer](https://adaptwest.shinyapps.io/climate-resilience-data-explorer),
or by going to the [AdaptWest page](https://adaptwest.databasin.org/pages/climate-resilience-data-explorer).

The data needed for the app or referenced by the app are available in the repostory or on the [AdaptWest page](https://adaptwest.databasin.org).
A release of the application has been archived with the Zenodo data repository [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3824539.svg)](https://doi.org/10.5281/zenodo.3824539).

The application was written by Justin F. Beckers and Carlos Carroll. 
The application logic, ui, plots, etc. were coded by Justin F. Beckers while 
Carlos Carroll provided scientific content, direction on features, feedback on 
the app. The application code here includes configuration for shinyapps.io and for 
running locally.

## R Packages Utilized
The application is built using [RStudio's](https://www.rstudio.com/)
[Shiny](https://www.rstudio.com/products/shiny/) open source R package augmented with
objects and code from the following packages.
```
shiny
shinyWidgets
shinythemes
htmltools
htmlwidgets
bsplus
shinyjs
webshot
flextable
shinycssloaders
tinytex
leaflet
leafem
leaflet.extras
sf
markdown
knitr
rmarkdown
kableExtra
data.table
dplyr
stringr
rdrop2
utils
readr
tibble
scales
tidyselect
plotly
ggplot2
ggiraphExtra
viridis
scales
RColorBrewer
cowplot
```

## Getting Help or Reporting an Issue
To get help with a bug or issue, please file an [bug/issue](https://github.com/SIBeckers/AdaptWest-CRDE/issues/new?assignees=&labels=&template=bug_report.md&title=).

To request a feature, please submit a [feature request](https://github.com/SIBeckers/AdaptWest-CRDE/issues/new?assignees=&labels=&template=feature_request.md&title=).