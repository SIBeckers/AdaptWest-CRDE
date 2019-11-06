appStarPlot <- function(input, output, session, data, namecol = "", removecols = c(1)) {
  source("./www/code/radarplot.R")
  rplot <- radarplot(data = data, namecol = namecol, removecols = removecols, interactive = T)
  output$appStarPlot <- renderPlotly(rplot)
}