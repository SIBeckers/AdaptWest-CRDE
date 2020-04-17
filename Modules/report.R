report<- function(input, output, session,pa=T,polys=NULL,polys2=NULL,data,namecol="PA_NAME",outname="AdaptWest_Metrics_Report") {
  ROIdata<-reactiveValues(roi=NULL)
  outputDir<-"./www/report" #Should still work when running the app locally.
 
  observeEvent(!is.null(polys),{
    if(isTRUE(pa)){
      n<-nrow(polys)
      output$selPolys<-renderUI({
        ns <- session$ns
        selectizeInput(
          inputId = ns("selPoly4Report"),
          label = "",
          choices = list(
            "Select protected area for report..." ="",
            "Selected in map"=as.list(polys[[namecol]],sorted=F),
            "All other polygons" = as.list(setdiff(data[[namecol]],polys[[namecol]]),sorted=F)
          ),
          multiple=F,
          selected=polys[n,][[namecol]],
          width="100%",
          options=list(maxOptions=4000,hideSelected=T)
        )
      })
    } else {
      n<-nrow(polys2)
      output$selPolys<-renderUI({
        ns <- session$ns
        selectizeInput(
          inputId = ns("selPoly4Report"),
          label = "",
          choices = list(
            "Select watershed for report..." ="",
            "Selected in map..." = as.list(polys2[n,][[namecol]]),
            "In the same ecoregion"=as.list(setdiff(polys[[namecol]],polys2[[namecol]])),
            "All other polygons" = as.list(setdiff(data[[namecol]],polys[[namecol]]))
          ),
          multiple=F,
          selected=polys2[n,][[namecol]],
          width="100%",
          options=list(maxOptions=3000)
        )
      })
    }
  })
  
  observe({
    foo<-input$selPoly4Report
    bar <- which(data[[namecol]]==foo)
    bar<-data[bar,]
    rm(foo)
    names(bar)[which(names(bar)==namecol)]<-"Name"
    ROIdata$roi<-bar
    rm(bar)
  })
  
  
  output$reportBttn <- downloadHandler(
    filename = function() {
      paste0(outname,"_",ROIdata$roi$Name,".",
             switch(input$reportFormat,PDF="pdf",Word="docx",HTML="html"))
    },
    content = function(file) {
      rpfm =  switch(
        input$reportFormat,PDF="pdf_document",Word="word_document",
        HTML="html_document"
      )
      paramslist<-reportconfig
      paramslist$poly = ROIdata$roi
      paramslist$html = input$reportInteractive
      paramslist$fmt = rpfm
      paramslist$data2 = data
      paramslist$pa  = pa
      if(!(rpfm=="html_document")){paramslist$html<-FALSE}
      
      mydownloads<<-rbindlist(
        list(mydownloads,
             data.table(ROIdata$roi$Name,as.numeric(Sys.time()),
                        as.integer(input$reportInteractive),rpfm,
                        as.integer(pa))),use.names=F,fill = F)
      
      withProgress(message = "Generating report ...",{
        tempReport <- file.path(outputDir, "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        if(paramslist$html){
          if(file.exists(file.path(outputDir,"interactive_reports",
              paste0(outname,"_",ROIdata$roi$Name,".",
                switch(input$reportFormat,PDF="pdf",Word="docx",
                        HTML="html")))) 
          ){
            shiny::setProgress(0.75,message="Found an existing report; getting it...")
            file.copy(
              from= file.path(
                outputDir,"interactive_reports",
                paste0(outname,"_",ROIdata$roi$Name,".",
                       switch(input$reportFormat,PDF="pdf",Word="docx",
                              HTML="html"))),
              to=file
            )
          } else {
            out <- rmarkdown::render(
              tempReport,
              output_format = rpfm,
              # clean = T,
              output_file = file,
              # intermediates_dir = "/www/report/tmp",
              params = paramslist,
              envir = new.env(parent = globalenv())
            )
            file.copy(
              from = out,
              to = file.path(
                outputDir,"interactive_reports",
                  paste0(outname,"_",ROIdata$roi$Name,".",
                    switch(input$reportFormat,PDF="pdf",Word="docx",
                      HTML="html"
                    )))
            )
          }
        } else {
          if(file.exists(file.path(outputDir,"static_reports",
              paste0(outname,"_",ROIdata$roi$Name,".",
                 switch(input$reportFormat,PDF="pdf",Word="docx",
                        HTML="html"))))) 
          {
            shiny::setProgress(0.75,message="Found an existing report; getting it...")
            file.copy(
              from = file.path(
                outputDir,"static_reports",
                paste0(outname,"_",ROIdata$roi$Name,".",
                       switch(input$reportFormat,PDF="pdf",Word="docx",
                              HTML="html"))),
              to = file
            )
          } else {
            out <- rmarkdown::render(
              tempReport,
              output_format = rpfm,
              output_file = file,
              params = paramslist,
              envir = new.env(parent = globalenv())
            )
            file.copy(
              from = out,
              to = file.path(
                outputDir,"static_reports",
                paste0(outname,"_",ROIdata$roi$Name,".",
                       switch(input$reportFormat,PDF="pdf",Word="docx",
                              HTML="html")))
            )
            file.copy(out, file)
          }
        
        }
      })#end of with progress
    } #end of content
  )#end of download handler
}