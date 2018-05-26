# Source Code for interactive choropleth map of Seattle Crime data. 
# By Olena Gavrishchuk

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)

ui <- fluidPage()

server <- function(input, output) {
  seattle_map <- ggplot(data = emissions_data) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group,
                               fill = emissions_catergories))
}

shinyApp(ui = ui, server = server)