report<- function(input, output, session,paramslist,outname="AdaptWest_Metrics_Report") {

  outputDir<-"./www/report" #Should work when running the app locally.
  #outputDir <- normalizePath(tempdir()) #For when loading to shinyapps.io
  output$storymap <- downloadHandler(
    filename = function() {
      paste0(outname,".",
             switch(input$reportFormat,PDF="pdf",Word="docx",Powerpoint="pptx",HTML="html",RMD="Rmd"))
    },
    content = function(file) {
      withProgress(message = "Generating report ...",{
        tempReport <- file.path(outputDir, "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- paramslist
        
        out <- rmarkdown::render(
          tempReport,
          params = params,
          envir = new.env(parent = globalenv())
        )
        file.copy(file.path(outputDir,paste0("report.",switch(input$reportFormat,PDF="pdf",Word="docx",
                                             Powerpoint="pptx",HTML="html",
                                             RMD = "tour_en.Rmd"))),out)
      })
    }
  )
}