# Define UI for application - will create a table and a plot (as requested)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Police Shooting Data Comparison (SEA & NYC)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h3("Please choose the combination of control parameters below:"),
      
      # Input: Provide the first metric of interest ----
      radioButtons("studied_type", label = h3("Fatal or Non-Fatal Shooting"),
                   c("Fatal Shooting" = "fatal",
                     "Non-Fatal Shooting" = "non-fatal")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Provide the second metric of interest ----
      radioButtons("studied_race", label = h3("White or Non-White Shooting Target"),
                   c("White Target" = "white",
                     "Non-White Target" = "non-white")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: check box group for the years to be selected ----
      checkboxGroupInput("yearCheckGroup", label = h3("Selected Years"), 
                         choices = list("2005" = "2005", "2006" = "2006", "2007" = "2007", "2008" = "2008",
                                        "2009" = "2009", "2010" = "2010", "2011" = "2011", "2012" = "2012",
                                        "2013" = "2013", "2014" = "2014", "2015" = "2015", "2016" = "2016"),
                         selected = list("2005", "2007", "2010", "2011", "2013", "2014", "2015"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ table, and plot ----
      tabsetPanel(type = "tabs",
                  tabPanel("Tables For Chosen Metrics", h4(textOutput("table_tab_header")),
                           fluidRow(
                             column(8, h4("Annual Police Shootings For Seattle"), dataTableOutput("table_seattle")),
                             # br() element to introduce extra vertical spacing ----
                             br(),
                             # br() element to introduce extra vertical spacing ----
                             br(),
                             br(),
                             # br() element to introduce extra vertical spacing ----
                             br(),
                             br(),
                             # br() element to introduce extra vertical spacing ----
                             br(),
                             column(8, h4("Annual Police Shootings For New York"), dataTableOutput("table_nyc")))),
                  tabPanel("Plots For Chosen Metrics", h4(textOutput("plot_tab_header")),
                           fluidRow(
                             column(12, h4("Annual Police Shootings For Seattle"), plotOutput("distPlot_seattle")),
                             # br() element to introduce extra vertical spacing ----
                             br(),
                             # br() element to introduce extra vertical spacing ----
                             br(),
                             column(12, h4("Annual Police Shootings For New York"), plotOutput("distPlot_nyc"))))),
      
      
      # br() element to introduce extra vertical spacing ----
      br(),
      # br() element to introduce extra vertical spacing ----
      br(),

      # this is the footer we wish to display      
      print("This research uses data from TBD!")
      
    )
  )
)
