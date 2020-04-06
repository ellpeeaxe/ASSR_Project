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
usePackage("png")
usePackage("base64enc")

community <- fread("../data/community area.csv")
hypothesis_data <- read.csv("../data/hypothesis.csv")
anova_img <- base64enc::dataURI(file="../data/anova.png", mime="image/png")
rank_img <- base64enc::dataURI(file="../data/anova_rank.png", mime="image/png")

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
        div(style="width:1200px",
          fluidRow(
            div(style="display: inline-block; margin-top:20px; margin-right:20px",
            selectInput('choice1', 'Time Factor', 
                        choices =  c(`Month` = 'Month',
                                     `Day of Week`='Day_of_Week',
                                     `Time Group`='time_indicator',
                                     `Holiday`='Holiday',
                                     `Season`='Season'),
                        multiple = FALSE),
            ),
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top", valueBoxOutput("TripsCount")),
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top", valueBoxOutput("TripsSD")),
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top", valueBoxOutput("TotalDistance")),
            div(style="display: inline-block; margin-top:25px; vertical-align:top", valueBoxOutput("DistanceSD")),
          ),
              
          fluidRow(
            div(style="display: inline-block; margin-top:20px; margin-right:20px; width:500px", plotOutput("dynamic_plot", width="100%")),
            div(style="display: inline-block; margin-top:20px; width:500px", plotOutput("box_plot", width="100%"))
          ),
          
          fluidRow(
            selectInput('choice2', 'Secondary Factor',
                        choices =  c(`Month` = 'Month',
                                     `Day of Week`='Day_of_Week',
                                     `Time Group`='time_indicator',
                                     `Holiday`='Holiday',
                                     `Season`='Season'),
                        multiple = FALSE)
          ),
          
          fluidRow(
            div(style="display: inline-block; width:1000px", plotOutput("TwoFactorPlot"))
          ),
        )
      ),
      tabItem(
        tabName = "originDestination",
        div(
          div(style="margin-top:20px;",
            h4("This dashboard shows the various aspects of Chicago taxi trips in 2019 (origin to destination, across time etc). Choose the following to view the travel patterns."),
            selectInput(
              inputId = 'ODSelector',
              label = "View as:",
              c("From Origin to Destination" = "OToD",
                "Origin and Destination Across Time" = "OAndD"
              ),
              selected="OtoD"
            )
          ),
          br(),
          div(
            conditionalPanel(
              condition = "input.ODSelector == 'OToD'",
              div(style="display:inline-block;width:250px; margin-right:20px; vertical-align:top",
                 selectInput("pickup","Pickup Community Area",
                              choices = community$community)),
              div(style="display:inline-block;width:250px; margin-right:20px;vertical-align:top",
                 selectInput("cal", "Weekday/weekends",
                             choices = list('All','Weekdays', 'Weekend/Holidays') 
                             )),
              div(style="display:inline-block;width:250px; margin-right:20px;vertical-align:top",
                 selectInput("time","Time Period",
                             choices = list('All','AM Period', 'Lunch Period', 'PM Period', 'Night'))),
              div(style="display:inline-block;width:250px; margin-right:20px;vertical-align:top",
                 selectInput("ind","Travel Indicators",
                             choices = list("Average Trips","Average Time", "Average Fare"))
               ),
              div(style="display:inline-block;width:750px",
                     h4("If the destination is empty on the map, it could be due to no trips or extreme values of the travel indicators."),
                     div(style="display:inline-block;width:1100px",leafletOutput("map", width="900px", height = "600px"))
              )
          ),
            conditionalPanel(
              condition = "input.ODSelector == 'OAndD'",
              fluidRow(
                div(style="display:inline-block;width:350px; vertical-align:top",
                  sliderInput("hr2", "Start Hour:",
                              min = 4, max = 23,
                              value = 7)
                ),
                div(style="display:inline-block;width:350px; vertical-align:top",
                  selectInput("cal2", "Weekday/weekends",
                              choices = list('All','Weekdays', 'Weekend/Holidays') 
                  )
                ),
                div(style="display:inline-block;width:350px; vertical-align:top",
                  selectInput("ind2","Travel Indicators",
                              choices = list("Average Trips","Average Speed"))
                )
              ),
              fluidRow(
                div(style="display:inline-block; margin-top:20px",
                  column(
                    style="display:inline-block",
                    width=6,
                    h3("Origins"),
                    div(style="display:inline-block;width:500px",leafletOutput("map2", width="450px", height = "600px"))
                  ),
                  column(
                    style="display:inline-block",
                    width=6,
                    h3("Destinations"),
                    div(style="display:inline-block;width:500px",leafletOutput("map3", width="450px", height = "600px"))
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "operations",
        div(style="display:inline-block",
          selectInput(
            inputId = 'ops_time_factor', label= 'Time Factor', 
            choices =  c("Month" = 'month',
                         "Day of week" ='day',
                         "Time Group" ='time_bin',
                         "Holiday"='holiday',
                         "Season" ='season'),
            multiple = FALSE), 
        ),
        # div(style="display:inline-block",
        #   selectInput(
        #     'ops_company', 'Company',
        #     choices = c(`All` = 'all',
        #                 `Blue Ribbon Taxi Association Inc.` = 'Blue Ribbon Taxi Association Inc.',
        #                 `Chicago Carriage Cab Corp` = 'Chicago Carriage Cab Corp',
        #                 `Chicago Independents` = 'Chicago Independents',
        #                 `Choice Taxi Association` = 'Choice Taxi Association',
        #                 `City Service` = 'City Service',
        #                 `Flash Cab` = 'Flash Cab',
        #                 `Globe Taxi` = 'Globe Taxi',
        #                 `Medallion Leasin` = 'Medallion Leasin',
        #                 `Star North Management LLC` = 'Star North Management LLC',
        #                 `Sun Taxi` = 'Sun Taxi',
        #                 `Taxi Affiliation Service Yellow` = 'Taxi Affiliation Service Yellow',
        #                 `Taxi Affiliation Services` = 'Taxi Affiliation Services',
        #                 `Taxicab Insurance Agency, LLC` = 'Taxicab Insurance Agency, LLC',
        #                 `Top Cab Affiliation` = 'Top Cab Affiliation'
        #     )
        #   )    
        # ),
        div(style="display: inline-block; width:1200px", plotOutput("downtime_chart", width="100%")),
        div(style="display: inline-block; width:1200px", plotOutput("earnings_chart", width="100%")),
        div(style="display: inline-block; width:1200px", plotOutput("tpc_chart", width="100%"))
      ),
      tabItem(
        tabName = "comparison",
        div(style="margin-top:20px;",h3("ANOVA on Mean Daily Trips per Taxi by Company (Top 9)")),
        fluidRow(
          div(style="display: inline-block; margin-bottom:30px;margin-top:30px;margin-right:30px;height: 100px; ",
              list(img(src=anova_img,height='100px'))),
          div(style="display: inline-block; margin-bottom:30px;height: 200px; ",
              list(img(src=rank_img,height='200px')))
          ),
        fluidRow(
          div(style="margin-bottom:30px;",h3("Tukey Multiple pairwise Comparisons")),
          DT::dataTableOutput("tukey")
        )
      ),
      tabItem(
        tabName = "farePrediction",
        fluidRow(
          div(style="display:inline-block;width:250px; margin-right: 20px; vertical-align:top",
              selectInput("trip_day", "Weekday/weekends",
                          choices = list('Weekdays', 'Weekend/Holidays') 
              )),
          div(style="display:inline-block;width:250px; margin-right: 20px; vertical-align:top",
              selectInput("trip_time","Time Period",
                          choices = list('AM Period', 'Lunch Period', 'PM Period', 'Night'))),
          div(style="display:inline-block;width:250px; margin-right: 20px; vertical-align:top",
              selectInput("trip_pickup","Pickup Community Area",
                          choices = community$community)),
          div(style="display:inline-block;width:250px; vertical-align:top",
              selectInput("trip_dropoff","Dropoff Community Area",
                          choices = community$community)
          )
        ),
        fluidRow(
          column(width = 6,
           div(style="margin-bottom: 20px; width: 350px",
               valueBoxOutput("trip_duration")),
            div(style="margin-bottom: 20px; width: 350px",
              valueBoxOutput("trip_fare")),
            div(style="margin-bottom: 20px; width: 350px",
              sliderInput("trip_tip", "Tip Rate %",
                          min = 0, max = 100,
                          value = 10)),
            div(style="width: 350px",
              valueBoxOutput("trip_total"))
          ),
          column(width = 6,
            leafletOutput("trip_map", width="600px", height = "600px")
          )
        )
      )
    )
  ), theme = "cosmo"
)


