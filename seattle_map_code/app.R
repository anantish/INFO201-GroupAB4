
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(leaflet)
library(ggmap)
library(sp)
library(geojsonio)

ui <- fluidPage(
  leafletOutput("seattle_map")
)

server <- function(input, output)
{
data <- read.csv(file = "../data/SPD_Officer_Involved_Shooting__OIS__Data.csv", 
                 stringsAsFactors = FALSE)


data <- select(data, GO, Date, Longitude, Latitude, Officer.Race, Subject.Race, 
               Subject.Weapon, Fatal) %>% 
        group_by(GO) %>%
        summarise(Date = paste(unique(Date), collapse = " , "),
                  Longitude = paste(unique(Longitude), collapse = " , "),
                  Latitude = paste(unique(Latitude), collapse = " , "),
                  OfficerRace = paste(Officer.Race, collapse = " , "),
                  SubjectRace = paste(unique(Subject.Race), collapse =" , "),
                  SubjectArmed = paste(unique(Subject.Weapon), collapse =" , "),
                  Fatal = paste(unique(Fatal), collapse =" , "))

data$Longitude <- as.numeric(data$Longitude)
data$Latitude <- as.numeric(data$Latitude)
            

#data_sp <- SpatialPointsDataFrame(data[,c(6, 7)], 
                                  #data[,-c(6, 7)])

output$seattle_map <- renderLeaflet({
 
           
 
               seattle_map<- leaflet() %>% 
               addTiles() %>% 
          
               addAwesomeMarkers(data = data, lng = ~Longitude, lat = ~Latitude,
                          popup = ~paste("<h3>Details</h3>", "Fatal: ", Fatal, 
                                         "<br>", "Date: ",  Date, sep = " ", 
                          color = ~pal(type)),
                          clusterOptions = markerClusterOptions()) %>%

               setView(lng = -122.335167, lat = 47.608013, zoom = 11,
                       options = NULL)

})

}
  

shinyApp(ui, server)


