library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(leaflet)
library(ggmap)
library(sp)
library(geojsonio)

seattle_boundaries <- geojson_read("census-tracts.geojson", what = "sp")

data <- read.csv(file = "../data/SPD_Officer_Involved_Shooting__OIS__Data.csv", 
                 stringsAsFactors = FALSE)
data$Longitude <- as.numeric(data$Longitude)
data$Latitude <- as.numeric(data$Latitude)

data_sp <- SpatialPointsDataFrame(data[,c(6, 7)], 
                                  data[,-c(6, 7)])

seattle_map <- leaflet() %>% 
               addTiles() %>% 
          
               addMarkers(data = data, lng = ~Longitude, lat = ~Latitude, 
                          popup = ~paste("<h3>Details</h3>", "Fatal: ", Fatal, 
                                         "<br>", "Date: ",  Date, sep = " ")) %>% 
               setView(lng = -122.335167, lat = 47.608013, zoom = 11,
                       options = NULL)
seattle_map


