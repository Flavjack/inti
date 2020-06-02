# wanuy -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

shinyServer(function(input, output) {

ft <- inti::getData(dir = "https://docs.google.com/spreadsheets/d/1XBezmc0S1T0EQj7zuZjTAOlPXosdDgTTotJK2jPAQAI/edit#gid=1020115876")


fert <- ft %>%
  dplyr::select(fertilizante, elemento, porcentage) %>%
  tidyr::spread(key = elemento, value = porcentage)

output$frt <- DT::renderDataTable(fert, server = F, filter = 'top', option = list(pageLength = 50, autoWidth = F))

# select row & colums: https://rstudio.github.io/DT/shiny.html

output$sfrt = renderPrint({
  s = input$frt_rows_selected
  if (length(s)) {
    cat(fert$fertilizante[s], sep = '\n')
  }
})


})


