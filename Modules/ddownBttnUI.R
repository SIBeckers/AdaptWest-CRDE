ddownBttnUI <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "ddownBttn",
      dropdownButton(#The dropdown menu button in the top right. It doesn't do anything right now.
        inputId = ns("map_inputs"),
        label = "Settings",
        icon = icon("gears"),
        status = "danger",
        size =  "default",
        circle = T,
        up = F,
        right = T,
        width = "250px",
        h3("Settings:"),
        sliderInput(
          inputId = ns("opacity"),
          label = "Image Opacity",
          min = 0,
          max = 1.0,
          value = 0.45
        ),
        sliderInput(
          inputId = ns("polyopacity"),
          label = "Polygon Fill Opacity",
          min = 0,
          max = 1.0,
          value = 0.85
        ),
        bookmarkButton(
          label = "Bookmark App",
          icon = icon("bookmark", lib = "font-awesome"),
          style = "font-size: 18px; class=bookmarkButtn",
          id = ns("bookmarkBttn")
        )
      )
    )
  )
}
