# Define server logic required to draw the bar plot
server <- function(input, output) {
  
  # chose the 2 dataframes of interest that will be used based on user selection
  # the first data frame will be for Seattle and the second for NYC
  chosen_metric_seattle <- reactive({
    if ((input$studied_type == "fatal") & (input$studied_armed == "armed")) {
      
      # combination of fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle <- seattle_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    } else if ((input$studied_type == "fatal") & (input$studied_armed == "unarmed")) {
      
      # combination of fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle <- seattle_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    } else if ((input$studied_type == "non-fatal") & (input$studied_armed == "armed")) {
      
      # combination of non-fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle <- seattle_non_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    } else if ((input$studied_type == "non-fatal") & (input$studied_armed == "unarmed")) {
      
      # combination of non-fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_seattle <- seattle_non_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    }
  })
  
  chosen_metric_nyc <- reactive({
    if ((input$studied_type == "fatal") & (input$studied_armed == "armed")) {
      
      # combination of fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc <- new_york_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    } else if ((input$studied_type == "fatal") & (input$studied_armed == "unarmed")) {
      
      # combination of fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc <- new_york_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    } else if ((input$studied_type == "non-fatal") & (input$studied_armed == "armed")) {
      
      # combination of non-fatal shooting & armed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc <- new_york_non_fatal_by_year_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    } else if ((input$studied_type == "non-fatal") & (input$studied_armed == "unarmed")) {
      
      # combination of non-fatal shooting & unarmed target which is then
      # filtered by the selected years
      filtered_chosen_dataframe_nyc <- new_york_non_fatal_by_year_non_armed %>% filter(grepl(paste(input$yearCheckGroup, collapse="|"), Date))
      
    }
  })
  
  # table will contain the data table to be rendered - assign it here
  output$table_seattle <- renderDataTable({chosen_metric_seattle()},
                                          options = list(pageLength = 5, lengthMenu = list(c(5, 10, -1), list('5', '10', 'All'))))
  
  output$table_nyc <- renderDataTable({chosen_metric_nyc()},
                                      options = list(pageLength = 5, lengthMenu = list(c(5, 10, -1), list('5', '10', 'All'))))
  
  # use ggplot in order to supply the plot to be rendered (conditional upon user selection)
  output$distPlot_seattle <- renderPlot({
    
    p <- ggplot(chosen_metric_seattle(), aes(y = NumberShootings, x = Date, fill = Date)) + geom_col(position = "dodge") +
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
  output$distPlot_nyc <- renderPlot({
    
    p <- ggplot(chosen_metric_nyc(), aes(y = NumberShootings, x = Date, fill = Date)) + geom_col(position = "dodge") +
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
