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



taxi <-  read_feather("C:/Users/Seant/Desktop/taxi")
taxi_summary <-  read_feather("C:/Users/Seant/Desktop/taxi_summary")

dayHour <-  taxi %>%  group_by(start_wday,start_hour) %>% dplyr::summarise(N=n())
dayHour$start_wday <-  factor(dayHour$start_wday, levels = rev(levels(dayHour$start_wday)))

dayMonth <-  taxi %>%  group_by(start_wday,start_month) %>% dplyr::summarise(N=n())
dayMonth$start_month <-  factor(dayMonth$start_month, levels = rev(levels(dayMonth$start_month)))

HourMonth <-  taxi %>%  group_by(start_hour,start_month) %>% dplyr::summarise(N=n())
HourMonth$start_month <-  factor(HourMonth$start_month, levels = rev(levels(HourMonth$start_month)))




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

  
  
  aggregated <- reactive({
    taxi %>%
      group_by_at(input$choice1) %>%
      dplyr::summarise(Trips_Count=length(taxi_id),
                Taxi_Count = n_distinct(taxi_id),
                Total_Distance=sum(trip_miles))
  })
  
  #Trip_Count <- reactive({  aggregated() %>% summarise(Trip_Count=mean(Trips_Count))})
  
  
  
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
  
  
#  output$TaxiCount <-  renderValueBox(
#    valueBox(
#      subtitle = 'No. of Taxis',
#      value=formatC(mean(aggregated()$Taxi_Count,na.rm=TRUE), format = "d", big.mark = ","),
#      icon("taxi"),
#      color="blue"
#    )
#  )
  
  
  
  
  output$distPlot <- renderPlot({
    ggplot(taxi_summary, aes(x=Time,y=Count,colour=Group))+geom_point() + 
      ggtitle("2019 Traffic Count") +
      theme(plot.title = element_text(face = "bold", size = (15)),axis.text.x=element_text(angle = 45,hjust =1))+
      scale_x_datetime(date_breaks = '1 hour',
                       date_labels ='%H:%M')
  })
  
  
  output$dayHour <- renderPlot({ggplot(dayHour, aes(start_hour,start_wday))+geom_tile(aes(fill=N),color="white",na.rm=TRUE)+
      scale_fill_gradient(low = "#d8e1cf", high = "#0E3386")+
      guides(fill=guide_legend(title="Total Trips"))+
      theme_bw()+theme_minimal()+
      labs(title="Taxi Trips Start by Day of Week and Hour", x= "Trips per Hour", y= "Day of Week")
  })
  
  output$dayMonth <-  renderPlot({ggplot(dayMonth, aes(start_wday,start_month))+geom_tile(aes(fill=N),color="white",na.rm=TRUE)+
      scale_fill_gradient(low = "#d8e1cf", high = "#FF0000")+
      guides(fill=guide_legend(title="Total Trips"))+
      theme_bw()+theme_minimal()+
      labs(title="Taxi Trips Start by Day of Month and Weekday", x= "Day of Week", y= "Month")
  })
  
  


}