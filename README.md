[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# AdaptWest Climate Resilience Data Explorer Application Code

## About 
This repository contains the R code, javascript, html, and other files needed to create the AdaptWest Climate Resilience Data Explorer application.

The application can be found at [https://adaptwest.shinyapps.io/climate-resilience-data-explorer](https://adaptwest.shinyapps.io/climate-resilience-data-explorer),
or by going to [AdaptWest](https://adaptwest.databasin.org/pages/climate-resilience-data-explorer).



The data needed for the app are available in the repostory or on the [AdaptWest page](https://adaptwest.databasin.org)
A release of the application has been archived with the Zenodo data repository and issued a [doi](doilink).

The application was written by Justin F. Beckers and Carlos Carroll. 
The application logic, ui, plots, etc. were coded by Justin F. Beckers while 
Carlos Carroll provided scientific content, direction on features, feedback on 
the app.

The application code here includes configuration for shinyapps.io and for 
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