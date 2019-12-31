appStarPlot <- function(input, output, session, data, namecol = "", removecols = NULL, live = F) {
  source("./www/code/radarplot.R")
  source("./www/code/myicon.R")
  rplot <- radarplot(data = data, namecol = namecol, removecols = removecols, interactive = live)
  if (isTRUE(live)) {
    output$appStarPlot <- renderPlotly(rplot)
  } else {
    output$appStarPlot <- renderPlot(rplot)
  }
}