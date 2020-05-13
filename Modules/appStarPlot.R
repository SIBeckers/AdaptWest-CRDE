appStarPlot <- function(input, output, session, data, namecol = "", removecols = NULL, live = F,offset=0) {
  source("./www/code/radarplot.R")
  source("./www/code/myicon.R")
  rplot <- radarplot(data = data, namecol = namecol, removecols = removecols, interactive = live,offset=offset)
  if (isTRUE(live)) {
    output$appStarPlot <- renderPlotly(rplot)
  } else {
    output$appStarPlot <- renderPlot(rplot)
  }
  setBookmarkExclude(c("resetSP","getallSP"))
}