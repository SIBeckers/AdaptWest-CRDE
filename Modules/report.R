report<- function(input, output, session,pa=T,polys=NULL,data,namecol="PA_NAME",outname="AdaptWest_Metrics_Report") {
  ROIdata<-reactiveValues(roi=NULL)
  observeEvent(!is.null(polys),{
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
    bar <- na.omit(data[data[[namecol]]==foo,])
    rm(foo)
    names(bar)[which(names(bar)==namecol)]<-"Name"  
    print(bar %>% 
            # st_drop_geometry() %>% 
            select(Name,intact,elevdiv,fwvelref,bwvelref,brdref,treref,
                                                       treec,soilc))
    ROIdata$roi<-bar
    rm(bar)
  })
  outputDir<-"./www/report" #Should work when running the app locally.
 
  output$reportBttn <- downloadHandler(
    filename = function() {
      paste0(outname,"_",ROIdata$roi$Name,".",
             switch(input$reportFormat,PDF="pdf",Word="docx",Powerpoint="pptx",HTML="html",Markdown="md"))
    },
    content = function(file) {
      
      rpfm =  switch(input$reportFormat,PDF="pdf_document",Word="word_document",
                     Powerpoint="powerpoint_presentation",HTML="html_document",
                     MD="md_document")
      paramslist<-reportconfig
      paramslist$poly = ROIdata$roi
      paramslist$html = input$reportInteractive
      paramslist$fmt = rpfm
      if(!(rpfm=="html_document")){paramslist$html<-FALSE}
      mydownloads<<-rbindlist(list(mydownloads,data.table(ROIdata$roi$Name,as.numeric(Sys.time()),as.integer(input$reportInteractive),rpfm,as.integer(pa))),use.names=F,fill = F)
      
      withProgress(message = "Generating report ...",{
        tempReport <- file.path(outputDir, "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        if(paramslist$html){
          if(file.exists(file.path(outputDir,"interactive_reports",
              paste0(outname,"_",ROIdata$roi$Name,".",
                switch(input$reportFormat,PDF="pdf",Word="docx",
                        Powerpoint="pptx",HTML="html",Markdown="md"
                )))))
          {
            shiny::setProgress(0.75,message="Found an existing report; getting it...")
            file.copy(
              from= file.path(
                outputDir,"interactive_reports",
                paste0(outname,"_",ROIdata$roi$Name,".",
                       switch(input$reportFormat,PDF="pdf",Word="docx",
                              Powerpoint="pptx",HTML="html",Markdown="md"
                       )
                )
              ),
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
                      Powerpoint="pptx",HTML="html",Markdown="md"
                    )
                  )
                )
              )
          }
        } else {
          if(file.exists(file.path(outputDir,"static_reports",
              paste0(outname,"_",ROIdata$roi$Name,".",
                 switch(input$reportFormat,PDF="pdf",Word="docx",
                        Powerpoint="pptx",HTML="html",Markdown="md"
                 ))))) 
          {
            shiny::setProgress(0.75,message="Found an existing report; getting it...")
            file.copy(
              from = file.path(
                outputDir,"static_reports",
                paste0(outname,"_",ROIdata$roi$Name,".",
                       switch(input$reportFormat,PDF="pdf",Word="docx",
                              Powerpoint="pptx",HTML="html",Markdown="md"
              ))),
              to = file
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
                outputDir,"static_reports",
                paste0(outname,"_",ROIdata$roi$Name,".",
                       switch(input$reportFormat,PDF="pdf",Word="docx",
                              Powerpoint="pptx",HTML="html",Markdown="md"
                       )
                )
              )
            )
          }
        file.copy(out,file)
        }
      })
    }
  )
}