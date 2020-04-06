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
library(dplyr)
library(magrittr)
library(plotly)
library(viridis)
library(RColorBrewer)
library(leaflet)
library(sp)
library(data.table)
library(rgeos)
library(raster)
library(rgdal)
library(GISTools)
library(xts)

#Sean & Jayne's Stuff
taxi <-  fread("../data/taxi_descriptive.csv")
taxi<- taxi %>%  mutate (Season = fct_relevel(as.factor(Season),
                                              "Spring","Summer",
                                              "Autumn","Winter"))
taxi %<>% mutate ( time_indicator = fct_relevel(as.factor(time_indicator),
                                                "AM Period","Lunch Period",
                                                "PM Period","Night"))
taxi %<>% mutate (Day_of_Week = fct_relevel(as.factor(Day_of_Week),
                                            "Mon","Tue","Wed","Thu",
                                            "Fri","Sat","Sun"))
taxi %<>% mutate (Month= fct_relevel(as.factor(Month),
                                     "Jan","Feb","Mar","Apr",
                                     "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
taxi$Holiday <-  as.factor(taxi$Holiday)

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

#fare_data <- read.csv("data/fares.csv")
###########################################################################################
#                                                                                         #
#                                       SERVER CODES                                      #
#                                                                                         #
###########################################################################################
# Define server logic 
server <- function(input, output, session) {
  
  
  ###########################################################################################
  #                                                                                         #
  #                                 TRAVEL PATTERNS                                         #
  #                                                                                         #
  ###########################################################################################

  aggregated <- reactive({
    taxi %>%
      group_by_at(input$choice1) %>%
      dplyr::summarise(Trips_Count=sum(trips),
                       Total_Distance=sum(Distance))
  })
  
  output$TripsCount <-  renderValueBox(
    valueBox(
      value= formatC(mean(aggregated()$Trips_Count,na.rm=TRUE), format = "d", big.mark = ","),
      subtitle = "Mean Trips",
      icon("map marker alternate"),
      color="purple",
      size="tiny"
    )
  )
  
  output$TripsSD <-  renderValueBox(
    valueBox(
      value = trunc(sd(aggregated()$Trips_Count,na.rm=TRUE)),
      subtitle = "Trips SD",
      icon("map marker alternate"),
      color="purple",
      size="tiny"
    )
  )
  
  output$TotalDistance <-  renderValueBox(
    valueBox(
      value= formatC(mean(aggregated()$Total_Distance,na.rm=TRUE), format = "d", big.mark = ","),
      subtitle = "Mean Distance (km)",
      icon("road"),
      color="green",
      size="tiny"
    )
  )
  
  output$DistanceSD <-  renderValueBox(
    valueBox(
      value = trunc(sd(aggregated()$Total_Distance,na.rm=TRUE)),
      subtitle = "Distance SD (km)",
      icon("road"),
      color="green",
      size="tiny"
    )
  )
  
  aggriplot <- reactive({
    taxi %>% 
      group_by_at(input$choice1) %>% 
      group_by(trips,add=TRUE) %>% 
      dplyr::summarise(Trips=sum(trips))
  })
  
  output$dynamic_plot <- renderPlot({
    ggplot(aggriplot(), aes_string(x = input$choice1,fill = input$choice1))+geom_bar(aes(y=Trips),stat='identity')+
      ggtitle("Total Count") +
      scale_fill_manual(values = mycolors) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  boxdata <- reactive({
    taxi %>% 
      group_by_at(input$choice1) %>% 
      group_by(trip_date,trips,add=TRUE) %>% 
      dplyr::summarise(Trips=sum(trips))
  })
  
  
  
  output$box_plot <- renderPlot({
    ggplot(boxdata(), aes_string(x = input$choice1,fill=input$choice1))+ geom_boxplot(aes(y=Trips),alpha = 0.8) + 
      geom_jitter(aes(y=Trips),color="black",size=0.2,alpha=0.4) +
      theme (legend.position = "none") +
      scale_fill_manual(values = mycolors) +
      theme(plot.title = element_text(face = "bold"))+
      ggtitle ("Daily Variation")
  })
  
  
  multiplot <- reactive({
    taxi %>% 
      group_by_at(c(input$choice1,input$choice2)) %>% 
      group_by(trips,add=TRUE)%>% 
      dplyr::summarise(N=sum(trips))
  })
  
  
  output$TwoFactorPlot <- renderPlot({
    ggplot(multiplot(), aes_string(x=input$choice1,y=input$choice2))+geom_tile(aes(fill=N),color="white",na.rm=TRUE)+
      scale_fill_gradient(low = "#FFFFFF", high = "#0E7C7B")+
      guides(fill=guide_legend(title="Total Trips"))+
      theme_bw()+theme_minimal()+
      theme(plot.title = element_text(face = "bold", size = (15)))
  })
  
  
  ###########################################################################################
  #                                                                                         #
  #                                 Origin & Destination                                    #
  #                                                                                         #
  ###########################################################################################
  #Weiji's Stuff
  community <- fread("../data/community area.csv")
  taxi_od <- fread('../data/trips for R split.csv') 
  taxi_across <- fread('../data/trips across time origin.csv')
  taxi_across2 <- fread('../data/trips across time destination.csv')
  
  #loading the shapefiles 
  comm_area <- readOGR("../data/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0.shp")
  centroids <- readOGR("../data/Layer_Chicago/Layer_Chicago.shp")
  comm_area <- spTransform(comm_area, CRS("+proj=longlat +ellps=GRS80"))
  centroids <- spTransform(centroids, CRS("+proj=longlat +ellps=GRS80"))
  centroids_test <- reactive({subset(centroids, community == input$pickup)})
  
  # O to D
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
  leaflet_time <- reactive({leaflet(comm_area,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal(m()$average_time),
                                                                              weight = 2,
                                                                              opacity = 1,
                                                                              color = "grey",
                                                                              dashArray = "3",
                                                                              fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal, values=(0:120), title = "Average Time Taken", labFormat = labelFormat(suffix = " mins", between = '-'))%>% addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                               lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                               labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  pal_fare <- colorBin(palette = rev(brewer.pal(6,"RdYlGn")),
                       domain = c(0,7000),
                       na.color = "#00000000",
                       bins=c(0,10,20,30,40,50))
  
  leaflet_fares <- reactive({leaflet(comm_area,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal_fare(m()$average_fare),
                                                                               weight = 2,
                                                                               opacity = 1,
                                                                               color = "grey",
                                                                               dashArray = "3",
                                                                               fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal_fare, values = (0:50), title = "Average Amount Paid", labFormat = labelFormat(prefix = "$", between = '- $'))%>% addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                                              lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                              labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  
  pal_trips <- colorBin(palette = rev(brewer.pal(15,"RdYlGn")),
                        domain = c(0,5000),
                        na.color = "#00000000",
                        bins=c(0,1,2,5,10,20,50,100,200,300,500,1000,2000,3000,4000,5000)) 
  
  leaflet_trips <- reactive({leaflet(comm_area,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal_trips(m()$avg_trips),
                                                                               weight = 2,
                                                                               opacity = 1,
                                                                               color = "grey",
                                                                               dashArray = "3",
                                                                               fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal_trips, values = (0:5000),  title = "Average Trips Taken", labFormat = labelFormat(suffix = 'trips', between = '-'))%>% addLabelOnlyMarkers(data = centroids,
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
  
  # O AND D (O)
  comm_area2 <- readOGR("../data/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0.shp")
  centroids2 <- readOGR("../data/Layer_Chicago/Layer_Chicago.shp")
  comm_area2 <- spTransform(comm_area2, CRS("+proj=longlat +ellps=GRS80"))
  centroids2 <-spTransform(centroids2, CRS("+proj=longlat +ellps=GRS80"))
  #basemap
  filter2 <- reactive({subset(taxi_across, cal_day_desc == input$cal2 & start_hour== input$hr2)})
  m2 <- reactive({merge(comm_area2, filter2(), by.x ='area_numbe', by.y = 'pickup_community_area')})
  # speed across time 
  pal2 <-
    colorBin(palette = rev(brewer.pal(7,"YlGnBu")),
             domain = c(0,120),
             na.color = "#00000000",
             bins=c(0,20,30,40,50,110))

  leaflet_speed2 <- reactive({leaflet(comm_area2,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal2(m2()$average_speed),
                                                                                weight = 2,
                                                                                opacity = 1,
                                                                                color = "grey",
                                                                                dashArray = "3",
                                                                                fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal2, values=(0:120), title = "Average Speed", labFormat = labelFormat(suffix = " km/h", between = '-'))%>%addLabelOnlyMarkers(data = centroids2,
                                                                                                                                                                                                                                                                     lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                     labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))})
  #trips across time 
  pal_trips2 <- colorBin(palette = rev(brewer.pal(11,"RdYlGn")),
                         domain = c(0,5000),
                         na.color = "#00000000",
                         bins=c(0,1,2,5,10, 50,100,500,1000,2000,2500,3000,3500))
  
  leaflet_trips2 <- reactive({leaflet(comm_area2,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal_trips2(m2()$avg_trips),
                                                                                weight = 2,
                                                                                opacity = 1,
                                                                                color = "grey",
                                                                                dashArray = "3",
                                                                                fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal_trips2, values = (0:2000),  title = "Average Trips Taken", labFormat = labelFormat(suffix = ' trips', between = '-'))%>%addLabelOnlyMarkers(data = centroids2,
                                                                                                                                                                                                                                                                                      lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                                      labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))})
  
  leaflet_selection2 <-reactive({
    if (input$ind2 == "Average Speed") {
      leaflet_selection2 <- leaflet_speed2()
    }
    else {
      leaflet_selection2 <- leaflet_trips2()
    }
    return(leaflet_selection2)
  })
  
  output$map2 <- renderLeaflet(leaflet_selection2())  
  
  # O and D (D)
  #loading the shapefiles 
  comm_area3 <- readOGR("../data/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0.shp")
  comm_area3 <- spTransform(comm_area3, CRS("+proj=longlat +ellps=GRS80"))
  centroids3 <- readOGR("../data/Layer_Chicago/Layer_Chicago.shp")
  centroids3 <-spTransform(centroids3, CRS("+proj=longlat +ellps=GRS80"))
  
  #basemap
  filter3 <- reactive({subset(taxi_across2, cal_day_desc == input$cal2 & end_hour== input$hr2)})
  m3 <- reactive({merge(comm_area3, filter3(), by.x ='area_numbe', by.y = 'dropoff_community_area')})
  
  # speed across time 
  pal3 <-
    colorBin(palette = rev(brewer.pal(7,"YlGnBu")),
             domain = c(0,120),
             na.color = "#00000000",
             bins=c(0,20,30,40,50,110))
  # create the base map, default will be openstreetmap if not selected 
  # added centroids3 point as well
  leaflet_speed3 <- reactive({leaflet(comm_area3,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal3(m3()$average_speed),
                                                                                 weight = 2,
                                                                                 opacity = 1,
                                                                                 color = "grey",
                                                                                 dashArray = "3",
                                                                                 fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal3, values=(0:120), title = "Average Speed", labFormat = labelFormat(suffix = " km/h", between = '-'))%>%addLabelOnlyMarkers(data = centroids3,
                                                                                                                                                                                                                                                                      lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                      labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))})
  #trips across time 
  pal_trips3 <- colorBin(palette = rev(brewer.pal(11,"RdYlGn")),
                         domain = c(0,5000),
                         na.color = "#00000000",
                         bins=c(0,1,2,5,10, 50,100,500,1000,2000,2500))
  
  leaflet_trips3 <- reactive({leaflet(comm_area3,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = ~pal_trips3(m3()$avg_trips),
                                                                                 weight = 2,
                                                                                 opacity = 1,
                                                                                 color = "grey",
                                                                                 dashArray = "3",
                                                                                 fillOpacity = 0.8) %>% leaflet::addLegend("bottomleft", pal_trips3, values = (0:2000),  title = "Average Trips Taken", labFormat = labelFormat(suffix = ' trips', between = '-'))%>%addLabelOnlyMarkers(data = centroids3,
                                                                                                                                                                                                                                                                                       lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                                                                                                                                                                                       labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))})
  
  leaflet_selection3 <-reactive({
    if (input$ind2 == "Average Speed") {
      leaflet_selection3 <- leaflet_speed3()
    }
    else {
      leaflet_selection3 <- leaflet_trips3()
    }
    return(leaflet_selection3)
  })
  
  output$map3 <- renderLeaflet(leaflet_selection3())
  
  
  ###########################################################################################
  #                                                                                         #
  #                                     Operations                                          #
  #                                                                                         #
  ###########################################################################################
  ops_data <- fread('../data/ops_data.csv')
  ops_data$company <- as.factor(ops_data$company)
  ops_data$day <- as.factor(ops_data$day)
  ops_data$weekday <- as.factor(ops_data$weekday)
  ops_data$time_bin <- as.factor(ops_data$time_bin)
  ops_data$month <- as.factor(ops_data$month)
  ops_data$season <- as.factor(ops_data$season)
  ops_data$holiday <- as.factor(ops_data$holiday)
  
  downtime_data <- reactive({
    d <- ops_data %>%
      group_by_(input$ops_time_factor, 'company') %>%
      dplyr::summarise(downtime=median(median_downtime))
    return(d)
  })
  output$downtime_chart <- renderPlot({
    ggplot(downtime_data(), aes_string(x = input$ops_time_factor)) + 
      geom_bar(aes(y = downtime, fill = company), stat="identity", position = "dodge") +
      scale_fill_manual(values = mycolors) +
      ggtitle("Median Trip Downtime")
  })
  
  earnings_data <- reactive({
    d <- ops_data %>%
      group_by_(input$ops_time_factor, 'company') %>%
      dplyr::summarise(fare=mean(fare))
    return(d)
  })
  output$earnings_chart <- renderPlot({
    ggplot(earnings_data(), aes_string(x = input$ops_time_factor)) + 
      geom_bar(aes(y = fare, fill = company), stat="identity", position = "dodge") +
      scale_fill_manual(values = mycolors) +
      ggtitle("Mean Trip Fare")
  })
  
  tpc_data <- reactive({
    d <- ops_data %>%
      group_by_(input$ops_time_factor, 'company') %>%
      dplyr::summarise(trips=mean(trips_per_cab))
    return(d)
  })
  output$tpc_chart <- renderPlot({
    ggplot(tpc_data(), aes_string(x = input$ops_time_factor)) + 
      geom_bar(aes(y = trips, fill = company), stat="identity", position = "dodge") +
      scale_fill_manual(values = mycolors) +
      ggtitle("Mean Trips per Cab")
  })

  ###########################################################################################
  #                                                                                         #
  #                             Performance Comparison                                      #
  #                                                                                         #
  ###########################################################################################
  hypothesis_data <- fread("../data/hypothesis.csv")
  
  z <- reactive({
    company1 <- input$company1
    company2 <- input$company2
    metric <- input$comp_metric
    xbar <- paste(metric,"_mean",sep="")
    sd <- paste(metric,"_std",sep="")
    
    company1_data <- hypothesis_data %>% dplyr::filter(company == company1)
    company2_data <- hypothesis_data %>% dplyr::filter(company == company2)
    
    n1 <- company1_data$count
    n2 <- company2_data$count
    
    xbar1 <- company1_data[[xbar]]
    xbar2 <- company2_data[[xbar]]
    
    sd1 <- company1_data[[sd]] 
    sd2 <- company2_data[[sd]]
    
    z <- (xbar1-xbar2)/sqrt(sd1^2/n1+sd2^2/n2)
    z
  })
  
  output$Z_value <- renderValueBox(
    valueBox(
      value= round(z(),2),
      subtitle = "Z-Value",
      color="black",
      size="tiny"
    )
  )
  
  ###########################################################################################
  #                                                                                         #
  #                                 Fare Prediction                                         #
  #                                                                                         #
  ###########################################################################################
  model_data <- fread("../data/modelling.csv")
  
  prediction <- reactive({
    day <- if (input$trip_day == "Weekdays") {
              day <- 1
            }
            else {
              day <- 0
            }
    
    time <- if (input$trip_time == "AM Period") {
              time <- 1
            }
            else if (input$trip_time == "Lunch Period"){
              time <- 2
            }
            else if (input$trip_time == "PM Period"){
              time <- 3
            }
            else {
              time <- 4
            }
    
    pickup_data <- model_data[model_data$pickup_community_area == input$trip_pickup, ]
    dropoff_data <- pickup_data %>% dplyr::filter(dropoff_community_area == input$trip_dropoff, weekday == day, time_bin == time)
    if (nrow(dropoff_data) == 0){
      dropoff_data <- pickup_data %>% dplyr::filter(dropoff_community_area == input$trip_dropoff)
    } 
    
    if (nrow(dropoff_data) == 0){
      trip_duration <- mean(pickup_data[["duration"]])
      trip_distance <- mean(pickup_data[["distance_km"]])
    }
    
    if (nrow(dropoff_data) > 0){
      trip_duration <- mean(dropoff_data[["duration"]])
      trip_distance <- mean(dropoff_data[["distance_km"]])
    } 

    trip_data <- data.frame(time_bin=c(time),weekday=c(day),duration=c(trip_duration),distance_km=c(trip_distance))
    fit <- lm(fare ~ time_bin + weekday + duration + distance_km, data = pickup_data)
    prediction <- c(predict(fit,trip_data),trip_duration)
  })
  
  output$trip_duration <- renderValueBox(
    valueBox(
      value= round(prediction()[[2]]/60,0),
      subtitle = "Estimated Minutes",
      color="black",
      size="tiny"
    )
  )
  
  output$trip_fare <- renderValueBox(
    valueBox(
      value= paste("$",round(prediction()[[1]],2)),
      subtitle = "Estimated Fare",
      color="black",
      size="tiny"
    )
  )
  
  output$trip_total <- renderValueBox(
    valueBox(
      value= paste("$",round(prediction()[[1]]*(100+input$trip_tip)/100,2)),
      subtitle = "Total Fare with Tip",
      color="black",
      size="tiny"
    )
  )
  
  centroids_ptrip <- reactive({subset(centroids, community == input$trip_pickup)})
  centroids_dtrip <- reactive({subset(centroids, community == input$trip_dropoff)})
  leaflet_trip <- reactive({leaflet(comm_area,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addPolygons(fillColor = "#81B29A",
                                                                              weight = 2,
                                                                              opacity = 0.5,
                                                                              color = "grey",
                                                                              dashArray = "3",
                                                                              fillOpacity = 0.7) %>% addLabelOnlyMarkers(data = centroids,
                                                                                                                         lng = ~centroid_x, lat = ~centroid_y, label = ~area_num_1,
                                                                                                                         labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
                                                                                                 addMarkers(lng = c(centroids_ptrip()$centroid_x,centroids_dtrip()$centroid_x), lat=c(centroids_ptrip()$centroid_y,centroids_dtrip()$centroid_y))})
  
  output$trip_map <- renderLeaflet(leaflet_trip())
}