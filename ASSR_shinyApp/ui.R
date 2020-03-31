###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################
usePackage<-function(p){
  # load a package if installed, else load after installation.
  # Args:
  #   p: package name in quotes
  
  if (!is.element(p, installed.packages()[,1])){
    print(paste('Package:',p,'Not found, Installing Now...'))
    install.packages(p, dep = TRUE)}
  print(paste('Loading Package :',p))
  require(p, character.only = TRUE)  
}

usePackage("leaflet")
usePackage("shiny")
usePackage("dplyr")
usePackage("data.table")
usePackage("sp")
usePackage("rgeos")
usePackage("raster")
usePackage("rgdal")
usePackage("GISTools")
usePackage("ShinyApp")
usePackage("plotly")
usePackage("shiny.semantic")
usePackage("semantic.dashboard")
usePackage("ggplot2")
usePackage("ggridges")
usePackage("lubridate")
usePackage("dbplyr")
usePackage("dygraphs")
usePackage("xts")
usePackage("forcats")


###########################################################################################
#                                                                                         #
#                                    USER-INTERFACE CODES                                 #
#                                                                                         #
###########################################################################################
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Chicago Taxi Trips 2019", inverted = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "travelPatterns", "Travel Patterns"),
      menuItem(tabName = "originDestination", "Origins and Destinations"),
      menuItem(tabName = "operations", "Operational Efficiency"),
      menuItem(tabName = "comparison", "Performance Comparison"),
      menuItem(tabName = "farePrediction", "Fare Prediction")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "travelPatterns",
        fluidRow(
          box(
            width = 16,
            title = "Travel Patterns",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
            
            fluidRow(
                
              selectInput('choice1', 'Time Type', 
                          choices =  c(`Month` = 'Month',
                                       `Day of Week`='Day_of_Week',
                                       `Time Group`='time_indicator',
                                       `Holiday`='Holiday',
                                       `Season`='Season'),
                          multiple = FALSE),
              ),
            
            fluidRow(
              valueBoxOutput("TripsCount"),
              valueBoxOutput("TotalDistance"),
            ),
                
            fluidRow(
              column(width= 6 ,plotOutput("dynamic_plot")),
              column(width= 6 ,plotOutput("box_plot"))
              ),
            
            
            
            fluidRow(
              selectInput('choice2', 'Time Type', 
                          choices =  c(`Month` = 'Month',
                                       `Day of Week`='Day_of_Week',
                                       `Time Group`='time_indicator',
                                       `Holiday`='Holiday',
                                       `Season`='Season'),
                          multiple = FALSE)),
            
            fluidRow(
              column(width = 12, plotOutput("TwoFactorPlot"))
              
            ),
          )
        )
      ),
      tabItem(
        tabName = "Chicago Taxi Travel Patterns Dashboard",
        fluidRow(
          box(
            width = 16,
            title = "Origins and Destinations",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
            
           # fluidRow(
            #  tags$li("This dashboard shows the various aspects of Chicago taxi trips in 2019 (time, mileage etc) for an origin. Choose the following to view the travel patterns."),
             # h2(),
            #  selectInput("pickup","Pickup Community Area",
            #              choices = community$community),
            #  selectInput("cal", "Weekday/weekends",
            #              choices = list('Weekdays', 'Weekend/Holidays') 
            #  ),
            #  selectInput("time","Time Period",
            #              choices = list('AM Period', 'Lunch Period', 'PM Period', 'Night')),
            #  
            #  selectInput("ind","Travel Indicators",
            #              choices = list("Average Trips","Average Time", "Average Fare")),
            #  tags$li("If the destination is empty on the map, it could be due to no trips or extreme values of the travel indicators."),
            #  width = 2, height = 1  
            #),
            
          #  fluidRow(leafletOutput("map", width="900px", height = "600px"))
          )
        )
      ),
      tabItem(
        tabName = "operations",
        fluidRow(
          box(
            width = 16,
            title = "Operational Efficiency",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE
          )
        )
      ),
      tabItem(
        tabName = "comparison",
      #  fluidRow(
      #    selectInput(inputId = "companyA", label = "Select first company:", 
      #                    choices = levels(as.factor(fare_data$company)), selected = "Top Cab Affiliation"),
      #    div(style="display: inline-block; width: 300px; margin-left: 50px",
      #        selectInput(inputId = "companyB", label = "Select second company:",
      #                    choices = levels(as.factor(fare_data$company)), selected = "Taxi Affiliation Services")
       #   )
      #  ),
        fluidRow(
          box(
            width = 16,
            title = "Fare Distributions",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
           # plotOutput("fare_ridgeplot", height = 270)
          )
        )
      ),
      tabItem(
        tabName = "farePrediction",
        fluidRow(
          box(
            width = 16,
            title = "Fare Prediction",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE
          )
        )
      )
    )
  ), theme = "cosmo"
)


