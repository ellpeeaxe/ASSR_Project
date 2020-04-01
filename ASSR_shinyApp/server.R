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
library(RColorBrewer)


#Sean & Jayne's Stuff
taxi <-  read.csv("taxi_descriptive.csv")
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
  
  ##Sean & Jayne

  aggregated <- reactive({
    taxi %>%
      group_by_at(input$choice1) %>%
      dplyr::summarise(Trips_Count=sum(trips),
                       Total_Distance=sum(Distance))
  })
  
  output$TripsCount <-  renderValueBox(
    valueBox(
      value= paste0("Mean Trips: ",formatC(mean(aggregated()$Trips_Count,na.rm=TRUE), format = "d", big.mark = ",")),
      subtitle = paste0("SD: ",trunc(sd(aggregated()$Trips_Count,na.rm=TRUE))),
      icon("map marker alternate"),
      color="purple"
    )
  )
  
  output$TotalDistance <-  renderValueBox(
    valueBox(
      value= paste0("Mean Distance (km): ",formatC(mean(aggregated()$Total_Distance,na.rm=TRUE), format = "d", big.mark = ",")),
      subtitle = paste0("SD: ",trunc(sd(aggregated()$Total_Distance,na.rm=TRUE))),
      icon("road"),
      color="green"
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
      theme(plot.title = element_text(face = "bold", size = (15)))
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
      theme(plot.title = element_text(face = "bold", size = (15)))+
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
      scale_fill_gradient(low = "#d8e1cf", high = "#0E3386")+
      guides(fill=guide_legend(title="Total Trips"))+
      theme_bw()+theme_minimal()+
      theme(plot.title = element_text(face = "bold", size = (15)))
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