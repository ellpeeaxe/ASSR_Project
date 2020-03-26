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
taxi_across <- fread('trips and speed across time r.csv')

# UI is working
ui <- fluidPage(
        titlePanel("Chicago Taxi Trip Trends Across Time in 2019"),
        sidebarPanel
              (
               tags$li("This dashboard shows the various aspects of Chicago taxi trips in 2019 across time. Select the following to view the trend across time."),
                h2(),
                sliderInput("hr", "Start Hour:",
                            min = 4, max = 23,
                            value = 7),
               selectInput("cal", "Weekday/weekends",
                           choices = list('Weekdays', 'Weekend/Holidays') 
                             ),
               selectInput("ind","Travel Indicators",
                              choices = list("Average Trips","Average Speed")),
                             width = 2, height = 1),
  mainPanel(leafletOutput("map", width="900px", height = "600px"))
)


## server input for the filtering and mapping of the leaflet
server <- function(input, output) {
  #loading the shapefiles 
  comm_area <- readOGR("geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0.shp")
  comm_area <- spTransform(comm_area, CRS("+proj=longlat +ellps=GRS80"))
  centroids <- readOGR("Layer_Chicago.shp")
  centroids <-spTransform(centroids, CRS("+proj=longlat +ellps=GRS80"))
  
  #basemap
  

  filter <- reactive({subset(taxi_across, cal_day_desc == input$cal & start_hour== input$hr)})
  m <- reactive({merge(comm_area, filter(), by.x ='area_numbe', by.y = 'dropoff_community_area')})

  # speed across time 
  pal <-
    colorBin(palette = rev(brewer.pal(7,"YlGnBu")),
             domain = c(0,120),
             na.color = "#00000000",
             bins=c(0,20,30,40,50,110))
  # create the base map, default will be openstreetmap if not selected 
  # added centroids point as well
  leaflet_speed <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal(m()$average_speed),
                                                                              weight = 2,
                                                                              opacity = 1,
                                                                              color = "grey",
                                                                              dashArray = "3",
                                                                              fillOpacity = 0.8) %>% addLegend("topright", pal, values=(0:120), title = "Average Speed", labFormat = labelFormat(suffix = " km/h", between = '-'))%>%addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                         lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                         labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))})
  #trips across time 
  pal_trips <- colorBin(palette = rev(brewer.pal(11,"RdYlGn")),
                        domain = c(0,5000),
                        na.color = "#00000000",
                        bins=c(0,1,2,5,10, 50,100,500,1000,2000,2500))
  
  leaflet_trips <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal_trips(m()$avg_trips),
                                                                               weight = 2,
                                                                               opacity = 1,
                                                                               color = "grey",
                                                                               dashArray = "3",
                                                                               fillOpacity = 0.8) %>% addLegend("topright", pal_trips, values = (0:2000),  title = "Average Trips Taken", labFormat = labelFormat(suffix = ' trips', between = '-'))%>%addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                                          lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))})
  
  leaflet_selection <-reactive({
    if (input$ind == "Average Speed") {
      leaflet_selection <- leaflet_speed()
    }
    else {
      leaflet_selection <- leaflet_trips()
    }
    return(leaflet_selection)
  })
  
  output$map <- renderLeaflet(leaflet_selection())             
}


shinyApp(ui = ui, server = server)
 