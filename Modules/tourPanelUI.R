#tourPanelUI.R
tourPanelUI<-function(id){
  ns<-NS(id)
  tagList(
    # div(id=ns("tour"),class="tour",
      fluidRow(id = ns("Col1Row1"),#Unique ID for CSS styling. The story will go here.
               class = "tourCol1Row1",
       #Here we will put the story content
        # div(
        #   id = ns("story"), #Unique ID for CSS styling
          # textOutput(ns("bbtn")),
          uiOutput(ns("sidebar"),inline=F)
        # )
      ),
      # Story Buttons ---------------------------------------------------------------
      fluidRow(#The story control buttons will go here
        id = ns("Col1Row2"), #Unique ID for CSS styling. 
        class = "tourCol1Row2",
        actionBttn( #Goes back to the start
          inputId = ns("startBttn"),
          label = "",
          icon = icon("fast-backward", class = "fa-lg",lib = "font-awesome"),
          style = "unite",
          color = "primary",
          block = F,
          size = "sm"
        ),
        actionBttn(#Step backward 1
          inputId = ns("prevBttn"),
          label = "",
          icon = icon("arrow-circle-left", class = "fa-lg",lib = "font-awesome"),
          style = "unite",
          color = "default",
          block = F,
          size = "sm"
        ),
        actionBttn(#Goes back to null
          inputId = ns("stopBttn"),
          label = "",
          icon = icon("stop", class = "fa-lg",lib = "font-awesome"),
          style = "unite",
          color = "primary",
          block = F,
          size = "sm"
        ),
        actionBttn(#Step forward 1
          inputId = ns("nextBttn"),
          label = "",
          icon = icon("arrow-circle-right", class = "fa-lg", lib = "font-awesome"),
          style = "unite",
          color = "default",
          block = F,
          size = "sm"
        ),
        actionBttn(#Go to End
          inputId = ns("endBttn"),
          label = "",
          icon = icon("fast-forward", class = "fa-lg",lib = "font-awesome"),
          style = "unite",
          color = "primary",
          block = F,
          size = "sm"
        )
      )
    # )
  )
}
