#tourPanel.R
tourPanel <- function(input, output, session, tourId = 1, tourName = "y2y") {
  # Reactive values--------------------------------------------------------------
  
  tourIndex <- reactiveValues(id = tourId,tourName = tourName,tourMax = NULL,tour = NULL)
  setBookmarkExclude(
    c("nextBttn","prevBttn","startBttn","stopBttn","endBttn")
  )
  
  # Event/Observers -------------------------------------------------------------
  observe({
    if (tourIndex$tourName == "y2y") {
      tourIndex$tour <- y2y$tourFileName
      
    } else if (tourIndex$tourName == "pa") {
      tourIndex$tour <- pa$tourFileName
    } else {
      tourIndex$tour <- get(get(tourIndex$tourName)$tourFileName)
    }
    tourIndex$tourMax <- length(tourIndex$tour)
  })
  observeEvent(input$startBttn,{
    tourIndex$id <- 1
  })
  
  observeEvent(input$endBttn,{
    tourIndex$id <- tourIndex$tourMax
  })
  
  observeEvent(input$nextBttn,{
    if (isolate(tourIndex$id) < tourIndex$tourMax) {
      tourIndex$id <- tourIndex$id + 1
    } else {
      tourIndex$id <- tourIndex$tourMax
    }
  })
  
  observeEvent(input$prevBttn,{
    if (isolate(tourIndex$id) > 1 ) {
      tourIndex$id <- tourIndex$id - 1
    } else {
      tourIndex$id <- 1
    }
  })
  
  observeEvent(input$stopBttn,{
    tourIndex$id <- 1
  })
  
  observe({
    output$sidebar <- renderUI({
      ns <- session$ns
     if (file.exists(gsub("Rmd","html",tourIndex$tour[tourIndex$id])) == T) {
       includeHTML(gsub("Rmd","html",tourIndex$tour[tourIndex$id]))
     } else if (file.exists(gsub("Rmd","md",tourIndex$tour[tourIndex$id])) == T) {
       HTML(markdown::markdownToHTML(
         gsub("Rmd","md",tourIndex$tour[tourIndex$id]),output = gsub("Rmd","html",tourIndex$tour[tourIndex$id]),
         fragment.only = TRUE)
       )
     } else if (file.exists(gsub("Rmd","md",tourIndex$tour[tourIndex$id])) == F) {
       HTML(markdown::markdownToHTML(
          knit(tourIndex$tour[tourIndex$id],output = gsub("Rmd","md",tourIndex$tour[tourIndex$id]), quiet = T),
          fragment.only = TRUE)
        )
      }
    })
  })
  return(list(id = reactive({tourIndex$id})))
}
