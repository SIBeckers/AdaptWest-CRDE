Table <- function(input, output, session,tabdat=NULL){
  names(tabdat)[3]<-"Topo-diversity"
 
  setBookmarkExclude(c("resetSPtab","getallSPtab"))
  output$table<- function(){
    knitr::kable(
      x = tabdat,format = "html") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        fixed_thead = T, font_size = 12
     )
  }
}