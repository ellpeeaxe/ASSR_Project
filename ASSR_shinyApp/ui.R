###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################

library(plotly)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(ggplot2)
library(ggridges)

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
      selected = 0,
      tabItem(
        tabName = "travelPatterns",
        fluidRow(
          box(
            width = 16,
            title = "Travel Patterns",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
            
            fluidRow(
              valueBoxOutput("TripsCount"),
              valueBoxOutput("TotalDistance"),
              valueBoxOutput("TaxiCount"),
              ),
            
            fluidRow(
              plotOutput("distPlot")
              ),
            
            fluidRow(
              plotOutput("dayHour",height ="300px"),
              plotOutput("dayMonth",height ="300px"),
             )
          )
        )
      ),
      tabItem(
        tabName = "originDestination",
        fluidRow(
          box(
            width = 16,
            title = "Origins and Destinations",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE
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


