#ui.R
#AdaptWestApp ui.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 1.0.0
#Notes 
# - Using bookmarking. This means that the entire UI must be served by a single
#   function. But luckily this function can call many other functions or scripts.
# - Using GoogleAnalytics to track page views. We can in the future set up 
#   tracking on things like buttons, drop-down menus, tabs, etc. but I think we
#   don't really need to do that yet (we are also unlikely to deal with the data 
#   we would get from doing so).
#NavBarPage ----
ui <- function(request) {
  #Header ----
  header <- tagList(
    includeHTML("config_files/google-analytics.html"),
    includeScript("www/shared/resizeMap.js"),
    includeScript("www/shared/togglePanelIcon.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/tour.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/map.css"),
    tags$link(href = 'https://fonts.googleapis.com/css?family=Work+Sans:700', rel = 'stylesheet'),
    tags$link(href = 'https://fonts.googleapis.com/css?family=Roboto:500', rel = 'stylesheet'),
    includeScript("www/shared/plotly-binding-4.9.0/plotly.js"),
    includeScript("www/shared/typedarray-0.1/typedarray.min.js"),
    includeScript("www/shared/crosstalk-1.0.1/js/crosstalk.min.js"),
    includeCSS("www/shared/crosstalk-1.0.1/css/crosstalk.css"),
    includeCSS("www/shared/plotly-htmlwidgets-css-1.46.1/plotly-htmlwidgets.css"),
    includeScript("www/shared/plotly-main-1.46.1/plotly-latest.min.js"),
    tags$script(
      HTML(
        '$(document).keyup(function(event) {
            if ($("#password").is(":focus") && (event.key == "Enter")) {
              $("#go").click();
            }
          });'
      )),
    tags$script("
      Shiny.addCustomMessageHandler('resetValue', function(variableName) {
        Shiny.onInputChange(variableName, null);
      });
    ")
  )
  
  #Title ----
  title = div(
    id = "titlediv",
    "Climate Resilience Data Explorer",
    tags$a(href = "https://adaptwest.databasin.org/",
            tags$img(src = "./logos/adaptwestbanner_svg.svg", style = "height: 46.5px; font-family: Roboto;"),
      class = "titleimg", style="font-family: Roboto, Helvetica Neue, sans-serif !important;")
  )
  
  #Home Tab ----
  homeTab <- tabPanel(
    title = "Home",
    value = "homeTab",
    icon = icon("home", class = "fas", lib = "font-awesome"),
    sidebarPanel(
      width = 4,
      id = "homeSidebar",
      class = "mysidebar",
      if (file.exists("./www/md/homeTab/sideBar.html")) {
        includeHTML("./www/md/homeTab/sideBar.html")
      } else if (file.exists("./www/md/homeTab/sideBar.Rmd")) {
        includeHTML(markdown::markdownToHTML(knit("./www/md/homeTab/sideBar.Rmd",quiet = T),fragment.only = T))
      }
    ),
    mainPanel(
      id = "homeMain",
      #Explor Icon Row
      fluidRow(
        id="homeRow1",
        class="homeRow",
        column(
          width = 6,
          class = "homeColL", 
          fluidRow(
            img(id = "climexpimg", src = "img/HomeScreen/metricexplorerthumb169.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;")
          ),
          fluidRow(
            div(
              h5("Climate Metrics Explorer – Explore and summarize climate resilience data by watershed." ,class = "homeTextHead"), 
              id = "climexpLink", style = "text-align: center;cursor:pointer;text-align: justify"
            )
          )
        ),
        column(
          width = 6,
          class = "homeColR",
          fluidRow(
            img(id = "paexpimg",src = "img/HomeScreen/paexplorerthumb169.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;")
          ),
          fluidRow(
            div(
              h5("Protected Area Explorer – Explore and compare the climate vulnerability and resilience of North America’s protected areas.",class = "homeTextHead"), 
              id = "paexpLink",style = "text-align: center; cursor:pointer;text-align: justify;"
            )
          )
        ),
        style = "text-align: center; height: 33.625vh"
      ),
      #Tour Icon Row
      fluidRow(
        id="homeRow2",
        class="homeRow",
        column(
          width = 6,
          class = "homeColL",
          fluidRow(
            img(id = "climtourimg", src = "img/HomeScreen/metrictourthumb169.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;")
          ),
          fluidRow(
            div(
              h5("Climate Metrics Tour – Learn about the concepts and methods underlying key climate resilience metrics.",class = "homeTextHead"), 
              id = "climtourLink", style = " text-align: center;cursor:pointer; text-align: justify;"
            )
          )
        ),
        column(
          width = 6,
          class = "homeColR",
          fluidRow(
            img(id = "patourimg",src = "img/HomeScreen/patourthumb169.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;")
          ),
          fluidRow(
            div(
              h5("Protected Area Tour – Learn about key climate resilience metrics via a comparison across the major protected areas of the Yellowstone-to-Yukon region.", class = "homeTextHead"), 
              id = "patourLink", style = "text-align: center;cursor:pointer; text-align: justify;"
            )
          )
        ),
        style = "text-align: center; height: 33.625vh"
      )
    )
  )
  
  #Climate Metric Tour Tab ----
  climTourTab <- tabPanel(
    title = "Climate Metrics Tour",
    value = "climtourTab",
    icon = NULL,
    tags$script("
      Shiny.addCustomMessageHandler('resetValue', function(variableName) {
        Shiny.onInputChange(variableName, null);
      });
    "),
    sidebarPanel(
      id = "climtourSide",
      class = "mysidebar",
      width = 4,
      tourPanelUI("y2ytour")
    ),
    mainPanel(
      id = "climtourMain",
      width = 8,
      mapUI("y2ymap"),
      ddownBttnUI("y2ymapBttn", poly=F)
    )
  )
  
  #Climate Metric Explorer Tab ----
  climExpTab <- tabPanel(
    title = "Climate Metrics Explorer",
    value = "climexpTab",
    icon = NULL,
    tags$script("
      Shiny.addCustomMessageHandler('resetValue', function(variableName) {
        Shiny.onInputChange(variableName, null);
      });
    "),
    sidebarPanel(
      id = "climexpSide",
      class = "mysidebar",
      width = 4,
      fluidRow(
        id = "climexpSideR1",
        h4("Menu",style="padding-left: 15px")
      ),
      fluidRow(
        id = "climexpSideR2",
        bs_accordion(id = "climexpacc") %>%
          bs_set_opts(panel_type = "info", use_heading_link = T) %>%
          bs_append(
            title = "1. About",
            includeHTML("./www/md/explorers/metrictourexplorermainpanel.html")
          ) %>%
          bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
          bs_append(
            title = "2. Optional map inputs",
            content =
              list(
                fluidRow(
                  selectizeInput(
                    inputId = "climExpLayer",
                    label = NULL,
                    choices = c("Select metric: " = "", tilevect),
                    selected = NULL,
                    multiple  = F,
                    width = "75%",
                    options = list(maxOptions = 12)
                  ),
                  actionBttn(
                    inputId = "clearTileFill",
                    label = "Clear Layer",
                    icon= NULL,
                    color="default",
                    size="s"
                  ),
                  style="padding-left:15px; padding-right: 15px;"
                ),
                fluidRow(
                  selectizeInput(
                    inputId = "climFillPolys",
                    label = NULL,
                    choices = c("Fill visible polygons with ..." = "",
                                polyfillvect),
                    selected = NULL,
                    multiple = F,
                    width = "75%",
                    options = list(maxOptions = 12)
                  ),
                  actionBttn(
                    inputId = "clearPolyFill",
                    label = "Clear Layer",
                    icon= NULL,
                    color="default",
                    size="s"
                  ),
                  style="padding-left:15px; padding-right: 15px;"
                )
              )
          ) %>%
          bs_set_opts(panel_type = "warning", use_heading_link = T) %>%
          bs_append(
            title = "3. Required map inputs",
            content = list(
              fluidRow(h4("In order to view any plots or tables, or to generate a report, you must
                          select an ecoregion on the map and then select one or more watersheds."),
                       style="padding-left:15px; padding-right: 15px;")
            )
          ) %>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "4. Starplot of Metrics", 
            content = 
              list(
                uiOutput("climexpStarplotDiv",inline = T)
              )
          )%>%
          bs_append(
            title = "5. Scatterplot comparison", 
            content = 
              list(
                uiOutput("climexpXYplotdiv",inline = T)
              )
          )%>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "6. Table of metrics ",
            content=list(uiOutput("climtablediv"))
          ) %>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "7. Generate a report", 
            content = 
              list(reportUI('climReport')
              )
          )
      )
    ),
    mainPanel(
      width = 8,
      id = "climexpMain",
      mapUI("climexpMap"),
      ddownBttnUI("climexpMapBttn"),
      uiOutput("b2ecoBttn")
    )
  )
  
  #Protected Areas Tour Tab ----
  paTourTab <- tabPanel(
    title = "Protected Areas Tour",
    value = "patourTab",
    icon = NULL,
    tags$head(includeScript("www/shared/togglePanelIcon.js")),
    sidebarPanel(
      class = "mysidebar",
      id = "patourSide",
      width = 4,
      tourPanelUI("patour")
    ),
    mainPanel(
      id = "patourMain",
      width = 8,
      mapUI("pamap"),
      ddownBttnUI("pamapBttn",poly=F),
      absolutePanel(
        id = "patourPanel",
        top = 20,
        left = 25,
        right = "auto",
        bottom = "auto",
        width = "19vw",
        height = "auto",
        draggable = T,
        fixed = F,
        cursor = "inherit",
        HTML('<i class="fa fa-minus fa-lg mycolIcon" id="patourPanelIcon" data-toggle="collapse" data-target="#patourpaneldiv"></i><strong style="padding:2px"> Data Layer </strong>'),
        br(),
        tags$div(
          id = 'patourpaneldiv',  class = "collapse in",
          selectInput(
            inputId = "patourLayer",
            label = NULL,
            choices = c("Select metric: " = "",tilevect),
            selected = "",
            multiple  = F,
            width = "100%",
            selectize = T
          )
        )
      )
    )
  )
  
  #Protected Areas Explorer Tab ----
  paExpTab <- tabPanel(
    title = "Protected Areas Explorer",
    value = "paexpTab",
    icon = NULL,
    # tags$head(includeScript("www/shared/togglePanelIcon.js")),
    sidebarPanel(
      id = "paexpSide",
      class = "mysidebar",
      width = 4,
      fluidRow(
        id = "paexpSideR1",
        h4("Menu",style="padding-left: 15px")
       ),
      fluidRow(
        id = "paexpSideR2",
        bs_accordion(id = "paexpacc") %>%
          bs_set_opts(panel_type = "info", use_heading_link = T) %>%
          bs_append(
            title = "1. About",
            content = 
              includeHTML("./www/md/explorers/paexplorermainpanel.html")
          ) %>%
          bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
          bs_append(
            title = "2. Optional map inputs", 
            content =
              list(
                fluidRow(
                  selectizeInput(
                    inputId = "paExpLayer",
                    label = NULL,
                    choices = c("Select metric: " = "", tilevect),
                    selected = "",
                    multiple  = F,
                    width = "75%",
                    options = list(maxOptions = 12)
                  ),
                  actionBttn(
                    inputId = "paclearTileFill",
                    label = "Clear Layer",
                    icon= NULL,
                    color="default",
                    size="s"
                  ),
                  style="padding-left:15px; padding-right: 15px;"
                ),
                fluidRow(
                  selectizeInput(
                    inputId = "paFillPolys",
                    label = NULL,
                    choices = c("Fill visible polygons with ..." = "",
                                polyfillvect),
                    selected = NULL,
                    multiple = F,
                    width = "75%",
                    options = list(maxOptions = 12)
                  ),
                  actionBttn(
                    inputId = "paclearPolyFill",
                    label = "Clear Layer",
                    icon= NULL,
                    color="default",
                    size="s"
                  ),
                  style="padding-left:15px; padding-right: 15px;"
                )
              )
          ) %>%
          bs_set_opts(panel_type = "warning", use_heading_link = T) %>%
          bs_append(
            title = "3. Required map inputs",
            content = list(
              fluidRow(h4("In order to view any plots or tables, or to generate a report, you must
                          select one or more protected areas on the map.",
                          style="padding-left:15px; padding-right: 15px;"))
            )
          ) %>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "4. Starplot of metrics",
            content=list(
              uiOutput("paexpStarplotDiv",inline = T)
            )
          )%>%
          bs_append(
            title = "5. Scatterplot of metrics ",
            content=list(
              uiOutput("paexpXYplotdiv",inline = T)
            )
          )%>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "6. Table of metrics ",
            content=list(uiOutput("patablediv"))
          ) %>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "7. Generate a report", 
            content = 
              list(
                reportUI('paReport')
              )
          )
      )
    ),
    mainPanel(
      id = "paexpMain",
      width = 8,
      mapUI("paexpMap"),
      ddownBttnUI("paexpMapBttn")
    )
  )
  gh<-tabPanel(
    title=
      shinydashboardPlus::socialButton(url = aw_gh, type = "github")
  )
  tw<-tabPanel(
    title= shinydashboardPlus::socialButton(url = aw_tw, type = "twitter")
  )
  #NavBar Page ----
  tagList(
    useShinyjs(),
    navbarPage(
      title = title,
      windowTitle = "AdaptWest Climate Resilience Data Explorer",
      id = "tabs",
      selected = "homeTab",
      position = "static-top",
      header = header,
      # footer = footer,
      collapsible = T,
      inverse = F,
      fluid = T,
      theme = shinytheme("cerulean"),
      homeTab,
      climTourTab,
      climExpTab,
      paTourTab,
      paExpTab,
      gh,
      tw
    )
  ) 
}




