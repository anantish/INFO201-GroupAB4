library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(leaflet)
library(ggmap)
library(sp)
library(geojsonio)

source("basecode.R")

ui <- fluidPage(
  leafletOutput("seattle_map"), 

  titlePanel("Police Shooting Data Comparison (SEA & NYC)"), 
  tabsetPanel(
    tabPanel("White/Non-White",
      sidebarLayout(
    
        # Sidebar panel for inputs ----
        sidebarPanel(
      
          h3("Please choose the combination of control parameters below:"),
      
          # Input: Provide the first metric of interest ----
          radioButtons("studied_type2", label = h3("Fatal or Non-Fatal Shooting"),
                       c("Fatal Shooting" = "fatal2",
                         "Non-Fatal Shooting" = "non-fatal2")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: Provide the second metric of interest ----
          radioButtons("studied_race2", label = h3("White or Non-White Shooting Target"),
                       c("White Target" = "white2",
                         "Non-White Target" = "non-white2")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: check box group for the years to be selected ----
          checkboxGroupInput("yearCheckGroup2", label = h3("Selected Years"),
                             choices = list("2005" = "2005", "2006" = "2006", "2007" = "2007", "2008" = "2008",
                                            "2009" = "2009", "2010" = "2010", "2011" = "2011", "2012" = "2012",
                                            "2013" = "2013", "2014" = "2014", "2015" = "2015", "2016" = "2016"),
                             selected = list("2005", "2007", "2010", "2011", "2013", "2014", "2015"))
        ),
        mainPanel(
          dataTableOutput("table_seattle2"),
          dataTableOutput("table_nyc2"),
          plotOutput("distPlot_seattle2"),
          plotOutput("distPlot_nyc2")
        )
      )
    ),

    tabPanel("Armed/Unarmed",
      sidebarLayout(
    
        # Sidebar panel for inputs ----
        sidebarPanel(
      
          h3("Please choose the combination of control parameters below:"),
      
          # Input: Provide the first metric of interest ----
          radioButtons("studied_type1", label = h3("Fatal or Non-Fatal Shooting"),
                       c("Fatal Shooting" = "fatal1",
                         "Non-Fatal Shooting" = "non-fatal1")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: Provide the second metric of interest ----
          radioButtons("studied_armed1", label = h3("Armed or Unarmed Shooting Target"),
                       c("Armed Target" = "armed1",
                         "Unarmed Target" = "unarmed1")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: check box group for the years to be selected ----
          checkboxGroupInput("yearCheckGroup1", label = h3("Selected Years"),
                             choices = list("2005" = "2005", "2006" = "2006", "2007" = "2007", "2008" = "2008",
                                            "2009" = "2009", "2010" = "2010", "2011" = "2011", "2012" = "2012",
                                            "2013" = "2013", "2014" = "2014", "2015" = "2015", "2016" = "2016"),
                             selected = list("2005", "2007", "2010", "2011", "2013", "2014", "2015"))
        ),
    
        # Main panel for displaying outputs ----
        mainPanel(
          dataTableOutput("table_seattle1"),
          dataTableOutput("table_nyc1"), 
          plotOutput("distPlot_seattle1"), 
          plotOutput("distPlot_nyc1")
        )
      )
    )
  )
)

server <- function(input, output)
{
  data <- read.csv(file = "SPD_Officer_Involved_Shooting__OIS__Data.csv",
                   stringsAsFactors = FALSE)
  data$Longitude <- as.numeric(data$Longitude)
  data$Latitude <- as.numeric(data$Latitude)
  
  data_sp <- SpatialPointsDataFrame(data[,c(6, 7)], data[,-c(6, 7)])
  
  output$seattle_map <- renderLeaflet({
    seattle_map <- leaflet() %>% addTiles() %>%
                    addMarkers(data = data, lng = ~Longitude, lat = ~Latitude, 
                               popup = ~paste("<h3>Details</h3>", "Fatal: ", Fatal,
                                              "<br>", "Date: ",  Date, sep = " "),
                               clusterOptions = markerClusterOptions()) %>%
                              setView(lng = -122.335167, lat = 47.608013, zoom = 11, options = NULL)
    
})
  
  # chose the 2 dataframes of interest that will be used based on user selection
  # the first data frame will be for Seattle and the second for NYC
  chosen_metric_seattle2 <- reactive({
    if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_fatal_by_year_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_fatal_by_year_non_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of non-fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_non_fatal_by_year_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of non-fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_non_fatal_by_year_non_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    }
  })
  
  chosen_metric_nyc2 <- reactive({
    if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_fatal_by_year_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_fatal_by_year_non_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of non-fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_non_fatal_by_year_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of non-fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_non_fatal_by_year_non_White %>% filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    }
  })
  
  # table will contain the data table to be rendered - assign it here
  output$table_seattle2 <- renderDataTable({chosen_metric_seattle2()},
                                          options = list(pageLength = 5, lengthMenu = list(c(5, 10, -1), list('5', '10', 'All'))))
  
  output$table_nyc2 <- renderDataTable({chosen_metric_nyc2()},
                                      options = list(pageLength = 5, lengthMenu = list(c(5, 10, -1), list('5', '10', 'All'))))
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_seattle2 <- renderPlot({
    
    p <- ggplot(chosen_metric_seattle2(), aes(y = NumberShootings, x = Date, fill = Date)) + geom_col(position = "dodge") +
      labs(x = "Year(s) Of Measurement", # x-axis label (with units!)
           y = "Number Shootings", # y-axis label (with units!)
           color = "Date") + # legend label for the "color" property
      theme (
        panel.background = element_blank(), # remove gray background
        panel.grid.major = element_line(colour = "grey50"), # gray grid lines
        axis.line = element_line(size = 3), # thick axis lines
        axis.text = element_text(colour = "purple") # blue text!
      )
    
    p
  })
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_nyc2 <- renderPlot({
    
    p <- ggplot(chosen_metric_nyc2(), aes(y = NumberShootings, x = Date, fill = Date)) + geom_col(position = "dodge") +
      labs(x = "Year(s) Of Measurement", # x-axis label (with units!)
           y = "Number Shootings", # y-axis label (with units!)
           color = "Date") + # legend label for the "color" property
      theme (
        panel.background = element_blank(), # remove gray background
        panel.grid.major = element_line(colour = "grey50"), # gray grid lines
        axis.line = element_line(size = 3), # thick axis lines
        axis.text = element_text(colour = "purple") # blue text!
      )
    
    p
  })
  
  # chose the 2 dataframes of interest that will be used based on user selection
  # the first data frame will be for Seattle and the second for NYC
  chosen_metric_seattle1 <- reactive({
    if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of non-fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_non_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of non-fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_non_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    }
  })
  
  chosen_metric_nyc1 <- reactive({
    if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of non-fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_non_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of non-fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_non_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    }
  })
  
  # table will contain the data table to be rendered - assign it here
  output$table_seattle1 <- renderDataTable({chosen_metric_seattle1()},
                                          options = list(pageLength = 5, lengthMenu = list(c(5, 10, -1), list('5', '10', 'All'))))
  
  output$table_nyc1 <- renderDataTable({chosen_metric_nyc1()},
                                      options = list(pageLength = 5, lengthMenu = list(c(5, 10, -1), list('5', '10', 'All'))))
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_seattle1 <- renderPlot({
    
    p <- ggplot(chosen_metric_seattle1(), aes(y = NumberShootings, x = Date, fill = Date)) + geom_col(position = "dodge") +
      labs(x = "Year(s) Of Measurement", # x-axis label (with units!)
           y = "Number Shootings", # y-axis label (with units!)
           color = "Date") + # legend label for the "color" property
      theme (
        panel.background = element_blank(), # remove gray background
        panel.grid.major = element_line(colour = "grey50"), # gray grid lines
        axis.line = element_line(size = 3), # thick axis lines
        axis.text = element_text(colour = "purple") # blue text!
      )
    
    p
  })
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_nyc1 <- renderPlot({
    
    p <- ggplot(chosen_metric_nyc1(), aes(y = NumberShootings, x = Date, fill = Date)) + geom_col(position = "dodge") +
      labs(x = "Year(s) Of Measurement", # x-axis label (with units!)
           y = "Number Shootings", # y-axis label (with units!)
           color = "Date") + # legend label for the "color" property
      theme (
        panel.background = element_blank(), # remove gray background
        panel.grid.major = element_line(colour = "grey50"), # gray grid lines
        axis.line = element_line(size = 3), # thick axis lines
        axis.text = element_text(colour = "purple") # blue text!
      )
    
    p
  })
}

shinyApp(ui, server)
