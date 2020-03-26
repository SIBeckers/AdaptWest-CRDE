#ui.R
#AdaptWestApp ui.R
#Author - Justin F. Beckers
#Start Date - March 8, 2019
#Version - 0.2
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
    includeHTML("google-analytics.html"),
    includeScript("www/js/resizeMap.js"),
    includeScript("www/js/togglePanelIcon.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/tour.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "./css/map.css"),
    tags$link(href = 'https://fonts.googleapis.com/css?family=Work+Sans:700', rel = 'stylesheet'),
    tags$link(href = 'https://fonts.googleapis.com/css?family=Roboto:500', rel = 'stylesheet'),
    includeScript("www/lib/plotly-binding-4.9.0/plotly.js"),
    includeScript("www/lib/typedarray-0.1/typedarray.min.js"),
    includeScript("www/lib/crosstalk-1.0.1/js/crosstalk.min.js"),
    includeCSS("www/lib/crosstalk-1.0.1/css/crosstalk.css"),
    includeCSS("www/lib/plotly-htmlwidgets-css-1.46.1/plotly-htmlwidgets.css"),
    includeScript("www/lib/plotly-main-1.46.1/plotly-latest.min.js"),
    tags$script(
      HTML(
        '$(document).keyup(function(event) {
            if ($("#password").is(":focus") && (event.key == "Enter")) {
              $("#go").click();
            }
          });'
      ))
  )
  
  #Title ----
  title = div(
    id = "titlediv",
    "Climate Resilience Data Explorer",
    tags$a(href = "https://adaptwest.databasin.org/",
            tags$img(src = "./logos/adaptwestbanner.jpg", style = "height: 47px;"),
      class = "titleimg")
  )
  
  #Footer ----
  footer = tagList(
    div(class="footer",
      shinydashboardPlus::socialButton(url = aw_gh, type = "github"),
      shinydashboardPlus::socialButton(url = aw_tw, type = "twitter")
    )
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
      column(
        width = 6,
        id = "homeCol1",
        class = "homeCol",
        fluidRow(
          div(h5("Climate Metrics Tour – Learn about the concepts and methods underlying key climate resilience metrics.",class = "homeText"), 
              id = "climtourLink", style = "cursor:pointer; text-align: justify;"),
          style = "height: 10.5vh; max-height: 100%;"
        ), 
        fluidRow(
          img(id = "climtourimg", src = "img/HomeScreen/metrictourthumbnail.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;"),
          style = "text-align: center; height: 33.625vh;"
        ),
        fluidRow(
          div(h5("Climate Metrics Explorer – Explore and summarize climate resilience data by watershed." ,class = "homeText"), 
              id = "climexpLink", style = "cursor:pointer;text-align: justify"),
          style = "height: 10.5vh; max-height: 100%;"
        ),
        fluidRow(
          img(id = "climexpimg", src = "img/HomeScreen/watershed.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;"),
          style = "text-align: center; height: 33.625vh;"
        )
      ),
      column(
        width = 6,
        id = "homeCol2",
        class = "homeCol",
        fluidRow(
          id = "patourLink",
          div(h5("Protected Area Tour – Learn about key climate resilience metrics via a comparison across the major protected areas of the Yellowstone-to-Yukon region.", class = "homeText"), 
            style = "cursor:pointer; text-align: justify;"
          ),
          style = "height: 10.5vh; max-height: 100%;"
        ),
        fluidRow(
          id = "patourimg", 
          img(src = "img/HomeScreen/patourthumbnail.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;"),
          style = "text-align: center; height: 33.625vh;"
        ),
        fluidRow(
          id = "paexpLink", 
          div(h5("Protected Area Explorer – Explore and compare the climate vulnerability and resilience of North America’s protected areas.",class = "homeText"), 
            style = "cursor:pointer;text-align: justify;"
          ),
          style = "height: 10.5vh; max-height: 100%;"
        ),
        fluidRow(
          id = "paexpimg", 
          img(src = "img/HomeScreen/protectedarea.png",style = "cursor:pointer; max-width: 100%; max-height: 100%; height: auto; width: auto;"),
          style = "text-align: center; height: 33.625vh;"
        )
      )
    )
  )
  
  #Climate Metric Tour Tab ----
  climTourTab <- tabPanel(
    title = "Climate Metrics Tour",
    value = "climtourTab",
    icon = NULL,
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
      ddownBttnUI("y2ymapBttn")
    )
  )
  
  #Climate Metric Explorer Tab ----
  climExpTab <- tabPanel(
    title = "Climate Metrics Explorer",
    value = "climexpTab",
    icon = NULL,
    sidebarPanel(
      id = "climexpSide",
      class = "mysidebar",
      width = 4,
      fluidRow(
        id = "climexpSideR1",
        HTML('<i class="fa fa-minus fa-lg mycolIcon" id="climexpPanelIcon" data-toggle="collapse" data-target="#climexpSideR2"></i><strong style="padding:2px"> Menu </strong>')
      ),
      fluidRow(
        id = "climexpSideR2",
        bs_accordion(id = "climexpacc") %>%
          bs_set_opts(panel_type = "info", use_heading_link = T) %>%
          bs_append(
            title = "About",
            includeHTML("./www/md/explorermainpanelcontent/metrictourexplorermainpanel.html")
          ) %>%
          bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
          bs_append(
            title = "Inputs", 
            content =
              list(
                selectizeInput(
                  inputId = "climExpLayer",
                  label = NULL,
                  choices = c("Select metric: " = "", tilevect),
                  selected = NULL,
                  multiple  = F,
                  width = "auto",
                  options = list(maxOptions = 12)
                ),
                selectizeInput(
                  inputId = "climFillPolys",
                  label = NULL,
                  choices = c("Fill visible polygons with ..." = "",
                              tilevect),
                  selected = NULL,
                  multiple = F,
                  width = "auto",
                  options = list(maxOptions = 12)
                )
              )
          ) %>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "Plots and tables", 
            content = 
              list(
                uiOutput("climexpStarplotDiv",inline = T),
                uiOutput("climexpXYplotdiv",inline = T)
              )
          )%>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "Generate Report", 
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
    tags$head(includeScript("www/js/togglePanelIcon.js")),
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
      ddownBttnUI("pamapBttn"),
      absolutePanel(
        id = "patourPanel",
        top = 350,
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
    # tags$head(includeScript("www/js/togglePanelIcon.js")),
    sidebarPanel(
      id = "paexpSide",
      class = "mysidebar",
      width = 4,
      fluidRow(
        id = "paexpSideR1",
        HTML('<i class="fa fa-minus fa-lg mycolIcon" id="paexpPanelIcon" data-toggle="collapse" data-target="#paexpSideR2"></i><strong style="padding:2px"> Menu </strong>')
      ),
      fluidRow(
        id = "paexpSideR2",
        bs_accordion(id = "paexpacc") %>%
          bs_set_opts(panel_type = "info", use_heading_link = T) %>%
          bs_append(
            title = "About",
            content = 
              includeHTML("./www/md/explorermainpanelcontent/paexplorermainpanel.html")
          ) %>%
          bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
          bs_append(
            title = "Inputs", 
            content =
              selectInput(
                inputId = "paExpLayer",
                label = NULL,
                choices = c("Select metric: " = "",tilevect),
                selected = "",
                multiple  = F,
                width = "100%",
                selectize = T
              )
          ) %>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "Plots and Tables", 
            content=list(
              uiOutput("paexpStarplotDiv",inline = T),
              uiOutput("paexpXYplotdiv",inline = T)
            )
          )%>%
          bs_set_opts(panel_type = "success", use_heading_link = T) %>%
          bs_append(
            title = "Generate Report", 
            content = 
              list(reportUI('paReport')
                
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
      footer = footer,
      collapsible = T,
      inverse = F,
      fluid = T,
      theme = shinytheme("cerulean"),
      homeTab,
      climTourTab,
      climExpTab,
      paTourTab,
      paExpTab
    )
  ) 
}




