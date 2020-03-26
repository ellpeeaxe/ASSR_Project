# Set working directory before loading data
setwd('C:/Users/lta-spl2/Desktop/Wei Ji Work/R Stuff/For WJ')

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
usePackage("data.table")
usePackage("dplyr")

##data portion 
community <- fread("community area.csv")
taxi_od <- fread('trips for R split.csv') 
taxi_across <- fread('trips for R across time.csv')

# UI is working
ui <- fluidPage(
        titlePanel("Chicago Taxi Travel Patterns Dashboard"),
        sidebarPanel
              (
               tags$li("This dashboard shows the various aspects of Chicago taxi trips in 2019 (time, mileage etc) for an origin. Choose the following to view the travel patterns."),
                h2(),
               selectInput("pickup","Pickup Community Area",
                           choices = community$community),
               selectInput("cal", "Weekday/weekends",
                           choices = list('Weekdays', 'Weekend/Holidays') 
                             ),
               selectInput("time","Time Period",
                           choices = list('AM Period', 'Lunch Period', 'PM Period', 'Night')),
               
               selectInput("ind","Travel Indicators",
                              choices = list("Average Trips","Average Time", "Average Fare")),
                tags$li("If the destination is empty on the map, it could be due to no trips or extreme values of the travel indicators."),
               width = 2, height = 1),
  mainPanel(leafletOutput("map", width="900px", height = "600px"))
)


## server input for the filtering and mapping of the leaflet
server <- function(input, output) {
  #loading the shapefiles 
  comm_area <- readOGR("geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0.shp")
  centroids <- readOGR("Layer_Chicago.shp")
  comm_area <- spTransform(comm_area, CRS("+proj=longlat +ellps=GRS80"))
  centroids <-spTransform(centroids, CRS("+proj=longlat +ellps=GRS80"))
  centroids_test <- reactive({subset(centroids, community == input$pickup)})
  #basemap
  

  filter <- reactive({subset(taxi_od, cal_day_desc == input$cal & pickup_ca == input$pickup & time_indicator == input$time)})
  m <- reactive({merge(comm_area, filter(), by.x ='area_numbe', by.y = 'dropoff_community_area')})

   # time map
  pal <-
    colorBin(palette = rev(brewer.pal(9,"RdYlGn")),
             domain = c(0,1400),
             na.color = "#00000000",
             bins=c(0,15,30,45,60,75,90,120))
  # create the base map, default will be openstreetmap if not selected 
  # added centroids point as well
  leaflet_time <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal(m()$average_time),
                                                                    weight = 2,
                                                                    opacity = 1,
                                                                    color = "grey",
                                                                    dashArray = "3",
                                                                    fillOpacity = 0.8) %>% addLegend("topright", pal, values=(0:120), title = "Average Time Taken", labFormat = labelFormat(suffix = " mins", between = '-'))%>% addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                     lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                     labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  
  
  
  pal_fare <- colorBin(palette = rev(brewer.pal(6,"RdYlGn")),
                       domain = c(0,7000),
                       na.color = "#00000000",
                       bins=c(0,10,20,30,40,50))
  
  leaflet_fares <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal_fare(m()$average_fare),
                                                                     weight = 2,
                                                                     opacity = 1,
                                                                     color = "grey",
                                                                     dashArray = "3",
                                                                     fillOpacity = 0.8) %>% addLegend("topright", pal_fare, values = (0:50), title = "Average Amount Paid", labFormat = labelFormat(prefix = "$", between = '- $'))%>% addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                           lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                           labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  
  pal_trips <- colorBin(palette = rev(brewer.pal(15,"RdYlGn")),
                       domain = c(0,5000),
                       na.color = "#00000000",
                       bins=c(0,1,2,5,10,20,50,100,200,300,500,1000,2000,3000,4000,5000))
  
  leaflet_trips <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal_trips(m()$avg_trips),
                                                                               weight = 2,
                                                                               opacity = 1,
                                                                               color = "grey",
                                                                               dashArray = "3",
                                                                               fillOpacity = 0.8) %>% addLegend("topright", pal_trips, values = (0:5000),  title = "Average Trips Taken", labFormat = labelFormat(suffix = 'trips', between = '-'))%>% addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                                           lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                           labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  
  
  
  
  leaflet_selection <-reactive({
    if (input$ind == "Average Time") {
      leaflet_selection <- leaflet_time()
    }
    else if (input$ind == "Average Fare") {
      leaflet_selection <- leaflet_fares()
    }
    else {
      leaflet_selection <- leaflet_trips()
    }
    return(leaflet_selection)
  })
  
  
 
               
output$map <- renderLeaflet(leaflet_selection())
}


shinyApp(ui = ui, server = server)
 