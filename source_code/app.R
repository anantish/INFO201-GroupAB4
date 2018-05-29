# load the appropriate libraries that we may choose to use 
library("ggplot2")
library("httr")
library("jsonlite")
library("dplyr")
library("knitr")
library("readr")
library("tidyr")
library("maps")
library("RColorBrewer")
library("shiny")

# first, source the basic code that reads CSV etc. - always set working directory though
source("basecode.r")

# now, source the UI code
source("shootings_ui.r")

# now, source the server code
source("shootings_bar_plot.r")

# Run the application 
shinyApp(ui = ui, server = server)
