library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(leaflet)
library(ggmap)
library(sp)
library(geojsonio)
library(shinythemes)
source("basecode.R")

ui <- fluidPage(theme = shinytheme("lumen"),

  titlePanel("Case Studies of Police Shootings Based on Seattle and NYC Data"), 
  tabsetPanel(
    tabPanel(
      p("With the assistance of social media as a rapid form of communication, there has been increasing attention and awareness on the pervasiveness of many social issues, both nationally and internationally. In recent years in the United States, there has been a national outcry calling attention to the disproportionate amount of African Americans, particularly unarmed black men, that are being shot (and often killed) by police, at rates much higher compared to their white counterparts. African Americans comprise 13% of the national population, yet, according to a recent study done in 2017 by Vox using available FBI data, they comprise 31% of all victims of police violence (Vox, 2017). In addition, racial minorities make up the overwhelming majority of victims fatally shot while unarmed. Although socioeconomic factors may play a role in regards to the higher rates of minorities involved in crime, research still asserts that racial bias is still primarily responsible for such proportions.Our data mainly focuses on crime based in the city of Seattle, located in the largely democratically leaning state of Washington. In addition, Seattle, we have included data from the New York City Police Department, another state that is frequently described as both liberal and democratic. We wanted to explore whether political stance or liberal sentiments had any correlation with regards to police brutality across racial lines. Often this issue is treated as if it is only prevalent in states with larger populations that identify as politically conservative, but we wanted to see how police violence against racial minorities was enacted closer to home, in places that are often considered politically progressive in a national context. According to the 2010 census, African Americans consist of roughly 7.9% of the population in Seattle. In New York City, African Americans make up 24% of the population. These statistics must be taken into account when considering our findings.")
    ),
    tabPanel("Subject Race",
      sidebarLayout(
    
        # Sidebar panel for inputs ----
        sidebarPanel(
      
          h3("Please choose the combination of control parameters below:"),
      
          # Input: Provide the first metric of interest ----
          radioButtons("studied_type2", label = h3("Fatality of Shooting"),
                       c("Fatal" = "fatal2",
                         "Non-Fatal" = "non-fatal2")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: Provide the second metric of interest ----
          radioButtons("studied_race2", label = h3("Race of Subject"), 
                       c("White" = "white2",
                         "Non-White" = "non-white2")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: check box group for the years to be selected ----
          checkboxGroupInput("yearCheckGroup2", label = h3("Selected Years"),
                             choices = list("2005" = "2005", "2006" = "2006", 
                                            "2007" = "2007", "2008" = "2008",
                                            "2009" = "2009", "2010" = "2010", 
                                            "2011" = "2011", "2012" = "2012",
                                            "2013" = "2013", "2014" = "2014", 
                                            "2015" = "2015", "2016" = "2016"),
                             selected = list("2005", "2006", "2007", "2008", 
                                             "2009", "2010", "2011"))
        ),
        mainPanel(
          h2("Race Comparison (Seattle)"),
          dataTableOutput("table_seattle2"),
          h2("Race Comparison (New York)"),
          dataTableOutput("table_nyc2"),
          h2("Race Plot Comparison (Seattle)"),
          plotOutput("distPlot_seattle2"),
          h2("Race Plot Comparison (New York)"),
          plotOutput("distPlot_nyc2")
        )
      )
    ),

    tabPanel("Presence of Weapon",
      sidebarLayout(
    
        # Sidebar panel for inputs ----
        sidebarPanel(
      
          h3("Please choose the combination of control parameters below:"),
      
          # Input: Provide the first metric of interest ----
          radioButtons("studied_type1", label = h3("Fatality of Shooting"), 
                       c("Fatal" = "fatal1",
                         "Non-Fatal" = "non-fatal1")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: Provide the second metric of interest ----
          radioButtons("studied_armed1", label = h3("Presence of Weapon by Subject"),
                       c("Armed Subject" = "armed1",
                         "Unarmed Subject" = "unarmed1")),
      
          # br() element to introduce extra vertical spacing ----
          br(),
      
          # Input: check box group for the years to be selected ----
          checkboxGroupInput("yearCheckGroup1", label = h3("Selected Years"),
                             choices = list("2005" = "2005", "2006" = "2006", 
                                            "2007" = "2007", "2008" = "2008",
                                            "2009" = "2009", "2010" = "2010", 
                                            "2011" = "2011", "2012" = "2012",
                                            "2013" = "2013", "2014" = "2014", 
                                            "2015" = "2015", "2016" = "2016"),
                             selected = list("2005", "2007", "2010", "2011", 
                                             "2013", "2014", "2015"))
        ),
    
        # Main panel for displaying outputs ----
        mainPanel(
          h2("Armed vs. Unarmed Comparision (Seattle)"), 
          dataTableOutput("table_seattle1"),
          h2("Armed vs. Unarmed Comparision (New York City)"), 
          dataTableOutput("table_nyc1"), 
          h2("Armed vs. Unarmed Plot Comparision (Seattle)"), 
          plotOutput("distPlot_seattle1"), 
          h2("Armed vs. Unarmed Plot Comparision (Seattle)"), 
          plotOutput("distPlot_nyc1")
        )
      )), 
      
      tabPanel("Seattle Shootings Map",
               sidebarLayout(
             
                 sidebarPanel(
                   checkboxGroupInput("map_fatal", label = h3("Select"), 
                                      choices = list("Fatal" = "Fatal", 
                                                     "Non-Fatal" = "Non-Fatal")), 
                   checkboxGroupInput("map_race", label = h3("Select"), 
                                      choices = list("White" = "White", 
                                                     "Person-of-Color" = "Black"))
                 ),
                 mainPanel(
                   leafletOutput("seattle_map")
                          )
                            )
  )
))

server <- function(input, output)
{
  data <- read.csv(file = "../data/SPD_Officer_Involved_Shooting__OIS__Data.csv", 
                   stringsAsFactors = FALSE) %>% 
          select(GO, Date, Longitude, Latitude, Officer.Race, Subject.Race, 
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
  
  
  output$seattle_map <- renderLeaflet({
    seattle_map <- leaflet() %>% 
      addTiles() %>%
      addMarkers(data = data, lng = ~Longitude, lat = ~Latitude, 
                 popup = ~paste("<h3>Details</h3>", "Fatal: ", Fatal, 
                                "<br>", "Date: ",  Date, sep = " "),
                 clusterOptions = markerClusterOptions()) %>% 
      setView(lng = -122.335167, lat = 47.608013, zoom = 11,
              options = NULL)
})

  
  # chose the 2 dataframes of interest that will be used based on user selection
  # the first data frame will be for Seattle and the second for NYC
  chosen_metric_seattle2 <- reactive({
    if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_fatal_by_year_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_fatal_by_year_non_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of non-fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_non_fatal_by_year_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of non-fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle2 <- seattle_non_fatal_by_year_non_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    }
  })
  
  chosen_metric_nyc2 <- reactive({
    if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_fatal_by_year_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_fatal_by_year_non_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "white2")) {
      
      # combination of non-fatal shooting & white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_non_fatal_by_year_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    } else if ((input$studied_type2 == "non-fatal2") & (input$studied_race2 == "non-white2")) {
      
      # combination of non-fatal shooting & non-white target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc2 <- new_york_non_fatal_by_year_non_White %>% 
        filter(grepl(paste(input$yearCheckGroup2, collapse="|"), Date))
      
    }
  })
  
  # table will contain the data table to be rendered - assign it here
  output$table_seattle2 <- renderDataTable({chosen_metric_seattle2()},
                                          options = list(pageLength = 5, 
                                                         lengthMenu = list(c(5, 10, -1), 
                                                          list('5', '10', 'All'))))
  
  output$table_nyc2 <- renderDataTable({chosen_metric_nyc2()},
                                      options = list(pageLength = 5, 
                                                     lengthMenu = list(c(5, 10, -1), 
                                                      list('5', '10', 'All'))))
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_seattle2 <- renderPlot({
    
    p <- ggplot(chosen_metric_seattle2(), aes(y = NumberShootings, x = Date, fill = Date)) + 
      geom_col(position = "dodge") +
      labs(x = "Year", # x-axis label (with units!)
           y = "Number of Shootings", # y-axis label (with units!)
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
    
    p <- ggplot(chosen_metric_nyc2(), aes(y = NumberShootings, x = Date, fill = Date)) + 
      geom_col(position = "dodge") +
      labs(x = "Year", # x-axis label (with units!)
           y = "Number of Shootings", # y-axis label (with units!)
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
      filtered_chosen_dataframe_seattle1 <- seattle_fatal_by_year_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_fatal_by_year_non_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of non-fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_non_fatal_by_year_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of non-fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle1 <- seattle_non_fatal_by_year_non_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    }
  })
  
  chosen_metric_nyc1 <- reactive({
    if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_fatal_by_year_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_fatal_by_year_non_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "armed1")) {
      
      # combination of non-fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_non_fatal_by_year_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    } else if ((input$studied_type1 == "non-fatal1") & (input$studied_armed1 == "unarmed1")) {
      
      # combination of non-fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc1 <- new_york_non_fatal_by_year_non_armed %>% 
        filter(grepl(paste(input$yearCheckGroup1, collapse="|"), Date))
      
    }
  })
  
  # table will contain the data table to be rendered - assign it here
  output$table_seattle1 <- renderDataTable({chosen_metric_seattle1()},
                                          options = list(pageLength = 5, 
                                          lengthMenu = list(c(5, 10, -1), 
                                                list('5', '10', 'All'))))
  
  output$table_nyc1 <- renderDataTable({chosen_metric_nyc1()},
                                      options = list(pageLength = 5, 
                                      lengthMenu = list(c(5, 10, -1), 
                                              list('5', '10', 'All'))))
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_seattle1 <- renderPlot({
    
    p <- ggplot(chosen_metric_seattle1(), aes(y = NumberShootings, x = Date, fill = Date)) + 
      geom_col(position = "dodge") +
      labs(x = "Year", # x-axis label (with units!)
           y = "Number of Shootings", # y-axis label (with units!)
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
    
    p <- ggplot(chosen_metric_nyc1(), aes(y = NumberShootings, x = Date, fill = Date)) + 
      geom_col(position = "dodge") +
      labs(x = "Year", # x-axis label (with units!)
           y = "Number of Shootings", # y-axis label (with units!)
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
