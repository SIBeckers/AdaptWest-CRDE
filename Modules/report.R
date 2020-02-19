report<- function(input, output, session,pa=T,polys=NULL,data,namecol="PA_NAME",paramslist,outname="AdaptWest_Metrics_Report") {
  ROIdata<-reactiveValues(roi=NULL)
  
  observeEvent(!is.null(polys),{
    # print(names(data))
    if(isTRUE(pa)){
      output$selPolys<-renderUI({
        ns <- session$ns
        selectizeInput(
          inputId = ns("selPoly4Report"),
          label = "",
          choices = list(
            "Select protected area for report..." ="",
            "Selected in map"=as.list(polys[[namecol]]),
            "All other polygons" = as.list(setdiff(data[[namecol]],polys[[namecol]]))
          ),
          multiple=F,
          selected="",
          width="100%",
          options=list(maxOptions=4000,hideSelected=T)
        )
      })
    } else {
      output$selPolys<-renderUI({
        ns <- session$ns
        selectizeInput(
          inputId = ns("selPoly4Report"),
          label = "",
          choices = list(
            "Select watershed for report..." ="",
            "Selected in map"=as.list(polys[[namecol]]),
            "All other polygons" = as.list(setdiff(data[[namecol]],polys[[namecol]]))
          ),
          multiple=F,
          selected="",
          width="100%",
          options=list(maxOptions=3000)
        )
      })
    }
  })
  
  observe({
    foo<-input$selPoly4Report
    ROIdata$roi <- na.omit(data[data[[namecol]]==foo,])
    print(ROIdata$roi)
  })
  
  
  
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