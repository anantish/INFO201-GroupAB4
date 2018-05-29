# Source Code for interactive choropleth map of Seattle Crime data. 
# By Olena Gavrishchuk

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(leaflet)
library(ggmap)

ui <- fluidPage()

server <- function(input, output) {

  
}

shinyApp(ui = ui, server = server)