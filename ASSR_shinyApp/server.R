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
library(feather)
library(viridis)


stat_box_data <- function(x, upper_limit = 90000 ) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', 
                    format(sum(x$trips), big.mark = ",", decimal.mark = ".", scientific = FALSE), ###need help here
                    '\n',
                    'mean =', 
                    format(round(mean(x), 1), big.mark = ",", decimal.mark = ".", scientific = FALSE))
    )
  )
}


#Sean's Stuff
taxi <-  read_feather("C:/Users/Seant/Desktop/taxi")
taxi_summary <-  read_feather("C:/Users/Seant/Desktop/taxi_summary")
dayHour <-  taxi %>%  group_by(start_wday,start_hour) %>% dplyr::summarise(N=n())
dayHour$start_wday <-  factor(dayHour$start_wday, levels = rev(levels(dayHour$start_wday)))
dayMonth <-  taxi %>%  group_by(start_wday,start_month) %>% dplyr::summarise(N=n())
dayMonth$start_month <-  factor(dayMonth$start_month, levels = rev(levels(dayMonth$start_month)))
HourMonth <-  taxi %>%  group_by(start_hour,start_month) %>% dplyr::summarise(N=n())
HourMonth$start_month <-  factor(HourMonth$start_month, levels = rev(levels(HourMonth$start_month)))



#Jayne's Stuff
taxitrips <-read.csv("C:/Users/Seant/Desktop/Trips by days.csv")
taxitrips$trips <- as.numeric(taxitrips$trips)
taxitrips$trip_date <-dmy(taxitrips$trip_date)
taxitrips$Month <-as.factor(taxitrips$Month)

#Weiji's Stuff
community <- fread("C:/Users/Seant/Desktop/community area.csv")
taxi_od <- fread('C:/Users/Seant/Desktop/trips for R split.csv') 
taxi_across <- fread('C:/Users/Seant/Desktop/trips for R across time.csv')




#fare_data <- read.csv("data/fares.csv")
###########################################################################################
#                                                                                         #
#                                       SERVER CODES                                      #
#                                                                                         #
###########################################################################################
# Define server logic 
server <- function(input, output, session) {
  
  # Ridge Plot
#  companyridge_data <- reactive({
#    companyridge_data <- fare_data %>% 
#      filter(company %in% c(input$companyA,input$companyB)) %>%
#      select(fare,company) 
#  })
  
#  output$fare_ridgeplot <- renderPlot({
#    ggplot(companyridge_data(), aes(y=1,x=fare,fill=company)) +
#      geom_density_ridges(alpha=0.5) +
#      xlim(0,100) +
#      theme(axis.text=element_text(size=10,), axis.title.y = element_blank(), legend.position = "bottom") 
#  })
  
  
 ### For Travel Patterns 
  
  ##Sean

  aggregated <- reactive({
    taxi %>%
      group_by_at(input$choice1) %>%
      dplyr::summarise(Trips_Count=length(taxi_id),
                Taxi_Count = n_distinct(taxi_id),
                Total_Distance=sum(trip_miles))
  })

  output$TripsCount <-  renderValueBox(
    valueBox(
      value= paste0("No. of Trips: ",formatC(mean(aggregated()$Trips_Count,na.rm=TRUE), format = "d", big.mark = ",")),
      subtitle = paste0("SD: ",trunc(sd(aggregated()$Trips_Count,na.rm=TRUE))),
      icon("map marker alternate"),
      color="purple"
    )
  )
  
  output$TotalDistance <-  renderValueBox(
    valueBox(
      value= paste0("Distance Travelled (miles): ",formatC(mean(aggregated()$Total_Distance,na.rm=TRUE), format = "d", big.mark = ",")),
      subtitle = paste0("SD: ",trunc(sd(aggregated()$Total_Distance,na.rm=TRUE))),
      icon("road"),
      color="green"
    )
  )
  
  output$TaxiCount <-  renderValueBox(
    valueBox(
      value= paste0("No. of Taxis: ",formatC(mean(aggregated()$Taxi_Count,na.rm=TRUE), format = "d", big.mark = ",")),
      subtitle = paste0("SD: ",trunc(sd(aggregated()$Taxi_Count,na.rm=TRUE))),
      icon("taxi"),
      color="blue"
    )
  )

  output$TripsByTime <- renderPlot({
    ggplot(taxi_summary, aes(x=Time,y=Count,colour=Group))+geom_point() + 
      ggtitle("2019 Traffic Count") +
      theme(plot.title = element_text(face = "bold", size = (15)),axis.text.x=element_text(angle = 45,hjust =1))+
      scale_x_datetime(date_breaks = '1 hour',
                       date_labels ='%H:%M')
  })
  
  output$dayHour <- renderPlot({
    ggplot(dayHour, aes(start_hour,start_wday))+geom_tile(aes(fill=N),color="white",na.rm=TRUE)+
      scale_fill_gradient(low = "#d8e1cf", high = "#0E3386")+
      guides(fill=guide_legend(title="Total Trips"))+
      theme_bw()+theme_minimal()+
      labs(title="Taxi Trips Start by Day of Week and Hour", x= "Trips per Hour", y= "Day of Week")
  })
  
  output$dayMonth <-  renderPlot({
    ggplot(dayMonth, aes(start_wday,start_month))+geom_tile(aes(fill=N),color="white",na.rm=TRUE)+
      scale_fill_gradient(low = "#d8e1cf", high = "#FF0000")+
      guides(fill=guide_legend(title="Total Trips"))+
      theme_bw()+theme_minimal()+
      labs(title="Taxi Trips Start by Day of Month and Weekday", x= "Day of Week", y= "Month")
  })
  
  ##Jayne
  
  output$IntraDay <-  renderPlot({
    taxitrips %>%
      mutate (Day = fct_relevel(as.factor(taxitrips$time_indicator),
                                "AM Period","Lunch Period",
                                "PM Period", "Night")) %>%
      ggplot(aes(x=Day, y=trips, fill=time_indicator)) +
        geom_violin() +
        scale_fill_viridis(discrete=TRUE, alpha = 0.6, option = "A") +
        theme (legend.position = "none") +
        scale_fill_brewer(palette = "Greens") +
        xlab("Intraday") + ylab("# of Trips") +
        ggtitle ("Taxi trips across intraday")
  })


  
  output$TripsByDate <-  renderPlot({
    taxitrips %>%  group_by(trip_date) %>%  summarise(trips=sum(trips)) %>% ggplot(aes(x=trip_date, y=trips)) +
      geom_area (fill = "#69b3a2" , alpha = 0.7) +
      geom_line (colour = "#69b3a2", size = 1) +
      geom_smooth(se = FALSE) +
      geom_point(size = 0.5, color="#69b3a2")+
      ggtitle ("Taxi trips across 2019")+
      xlab("Months") + ylab("# of Trips")+
      scale_x_date(date_breaks = "1 month",date_labels = "%b") 
  })
  
  output$TripsByDay <-  renderPlot({
    taxitrips %>% group_by(trip_date, Day) %>%  summarise(trips=sum(trips)) %>% 
      mutate (Day = fct_relevel(as.factor(Day),
                                "MONDAY","TUESDAY",
                                "WEDNESDAY","THURSDAY",
                                "FRIDAY", "SATURDAY",
                                "SUNDAY")) %>%
      ggplot(aes(x=Day, y=trips, fill=Day)) +
      geom_boxplot(alpha = 0.8) + 
      geom_jitter(color="black",size=0.2,alpha=0.4) +
      theme (legend.position = "none") +
      scale_fill_brewer(palette = "Blues") +
      xlab("Weekday") + ylab("# of Trips") +
      ggtitle ("Taxi trips across the week")+
      stat_summary(
        fun.data = stat_box_data, 
        geom = "text", 
        hjust = 0.5,
        vjust = 0.9,
        size = 3
      )
  })
  
  output$TripsByMonth <-  renderPlot({
    taxitrips %>%  group_by(trip_date, Month) %>%  summarise(trips=sum(trips)) %>% ggplot(aes(x=Month, y=trips, fill = Month)) +
      geom_boxplot(alpha = 0.8) + 
      geom_jitter(color="black",size=0.2,alpha=0.4) +
      theme (legend.position = "none") +
      scale_fill_brewer(palette = "Greens") +
      xlab("Month") + ylab("# of Trips") +
      ggtitle ("Taxi trips across the Months")+
      stat_summary(
        fun.data = stat_box_data, 
        geom = "text", 
        hjust = 0.5,
        vjust = 0.9,
        size = 3
      )
  })
  
  output$TripsBySeason <-  renderPlot({
    taxitrips %>% group_by(trip_date, Season) %>%  summarise(trips=sum(trips)) %>% 
      mutate (Season = fct_relevel(as.factor(Season),
                                   "Spring","Summer",
                                   "Autumn","Winter")) %>%
      ggplot(aes(x=Season, y=trips, fill=Season)) +
      geom_boxplot(alpha = 0.8) + 
      geom_jitter(color="black",size=0.2,alpha=0.4) +
      theme (legend.position = "none") +
      scale_fill_brewer(palette = "Reds") +
      xlab("Seasons") + ylab("# of Trips") +
      ggtitle ("Taxi trips across the seasons of 2019")+
      stat_summary(
        fun.data = stat_box_data, 
        geom = "text", 
        hjust = 0.5,
        vjust = 0.9,
        size = 3
      )
  })
  
  #Weiji
  
  #loading the shapefiles 
#  comm_area <- readOGR("C:/Users/Seant/Desktop/geo_export_4f61d7c6-cb0a-4b29-947a-e8e34933b8e0.shp")
#  centroids <- readOGR("C:/Users/Seant/Desktop/Layer_Chicago.shp")
#  comm_area <- spTransform(comm_area, CRS("+proj=longlat +ellps=GRS80"))
#  centroids <-spTransform(centroids, CRS("+proj=longlat +ellps=GRS80"))
#  centroids_test <- reactive({subset(centroids, community == input$pickup)})
  #basemap
  
  
#  filter <- reactive({subset(taxi_od, cal_day_desc == input$cal & pickup_ca == input$pickup & time_indicator == input$time)})
#  m <- reactive({merge(comm_area, filter(), by.x ='area_numbe', by.y = 'dropoff_community_area')})
#  
  # time map
#  pal <-
#    colorBin(palette = rev(brewer.pal(9,"RdYlGn")),
##             domain = c(0,1400),
#             na.color = "#00000000",
#             bins=c(0,15,30,45,60,75,90,120))
  # create the base map, default will be openstreetmap if not selected 
  # added centroids point as well
#  leaflet_time <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal(m()$average_time),
#                                                                              weight = 2,
#                                                                              opacity = 1,
#                                                                              color = "grey",
#                                                                              dashArray = "3",
#                                                                              fillOpacity = 0.8) %>% addLegend("topright", pal, values=(0:120), title = "Average Time Taken", labFormat = labelFormat(suffix = " mins", between = '-'))%>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
#  
  
  
#  pal_fare <- colorBin(palette = rev(brewer.pal(6,"RdYlGn")),
#                       domain = c(0,7000),
#                       na.color = "#00000000",
#                       bins=c(0,10,20,30,40,50))
  
#  leaflet_fares <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal_fare(m()$average_fare),
 #                                                                              weight = 2,
#                                                                               opacity = 1,
 #                                                                              color = "grey",
#                                                                               dashArray = "3",
 #                                                                              fillOpacity = 0.8) %>% addLegend("topright", pal_fare, values = (0:50), title = "Average Amount Paid", labFormat = labelFormat(prefix = "$", between = '- $'))%>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  
#  pal_trips <- colorBin(palette = rev(brewer.pal(11,"RdYlGn")),
#                        domain = c(0,5000),
#                        na.color = "#00000000",
#                        bins=c(0,1,2,5,10,20,50,100,200,300,500,1000,2000,3000,4000,5000))
  
#  leaflet_trips <- reactive({leaflet(comm_area) %>% addTiles() %>% addPolygons(fillColor = ~pal_trips(m()$avg_trips),
#                                                                               weight = 2,
 #                                                                              opacity = 1,
#                                                                               color = "grey",
  #                                                                             dashArray = "3",
  #                                                                             fillOpacity = 0.8) %>% addLegend("topright", pal_trips, values = (0:5000),  title = "Average Trips Taken", labFormat = labelFormat(suffix = 'trips', between = '-'))%>% addMarkers(lng = centroids_test()$centroid_x, lat=centroids_test()$centroid_y)})
  
  
  
  
#  leaflet_selection <-reactive({
#    if (input$ind == "Average Time") {
#      leaflet_selection <- leaflet_time()
#    }
#    else if (input$ind == "Average Fare") {
#      leaflet_selection <- leaflet_fares()
#    }
#    else {
#      leaflet_selection <- leaflet_trips()
#    }
#    return(leaflet_selection)
#  })
  
  
  
  
#  output$map <- renderLeaflet(leaflet_selection())


  
}