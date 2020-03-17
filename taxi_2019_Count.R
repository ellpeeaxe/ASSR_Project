library(tidyverse)
library(lubridate)
library(magrittr)
library(plotly)

#Method 1: Count

#query = """
#    SELECT trip_miles,trip_start_timestamp, trip_end_timestamp, trip_seconds,  trip_miles/trip_seconds*3600 as miles_per_hr
#    FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`
#    WHERE EXTRACT (YEAR from trip_start_timestamp) = 2019 AND trip_miles  > 0 AND trip_seconds >0
#"""



taxi <-  read.csv("C:/Users/Seant/Desktop/taxi_2019_time.csv", head= TRUE)
taxi$trip_start_timestamp <- ymd_hms(taxi$trip_start_timestamp)
taxi$trip_end_timestamp <- ymd_hms(taxi$trip_end_timestamp)

taxi %<>% mutate(start_time = format(trip_start_timestamp,"%H:%M:%S"),
                  end_time=format(trip_end_timestamp,"%H:%M:%S"))

taxi %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",taxi$start_time)),ymd_hms(paste0("2019-01-01",taxi$end_time)))) %>% filter(!is.na(interval))
taxi_summary <- as.data.frame(seq(ymd_hms('2019-01-01 00:00:00'),ymd_hms('2019-01-01 23:45:00'), by = '15 mins'))
taxi_summary <- as.data.frame(taxi_summary[c(20:nrow(taxi_summary),1:19),])
names(taxi_summary)[1] <- "Time"


taxi_summary$Time <- ymd_hms(taxi_summary$Time)
taxi_summary$Count <-  NA
for(i in seq_along(taxi_summary$Time)){
  taxi_summary$Count[i] <- sum(taxi_summary$Time[i] %within% taxi$interval)
}

taxi_summary$Group <- cumsum(taxi_summary$Count) %/% (ceiling(sum(taxi_summary$Count) / 5)) + 1
plt <- ggplot(taxi_summary, aes(x=Time,y=Count,col=as.factor(Group)))+geom_line()+ggtitle("2019 Traffic Count") + theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)))
ggplotly(plt)
                              
                                                  


taxi_timings <- data.frame(Group=1:5,Start=format(taxi_summary[!duplicated(taxi_summary$Group, fromLast=FALSE), "Time"],"%H:%M:%S"),End=format(taxi_summary[!duplicated(taxi_summary$Group, fromLast=TRUE), "Time"],"%H:%M:%S"))



write.csv(taxi_timings,"taxi_timing.csv")

#Method 2 : Speed

#query = """
#    SELECT trip_miles,trip_start_timestamp, trip_end_timestamp, trip_seconds,  trip_miles/trip_seconds*3600 as miles_per_hr
#    FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`
#    WHERE EXTRACT (YEAR from trip_start_timestamp) = 2019 AND pickup_community_area = dropoff_community_area AND trip_miles  > 0 AND trip_seconds >0
#"""

timings <- read.csv("C:/Users/Seant/Desktop/taxi_timing.csv",head=TRUE)
timings %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",timings$Start)),ymd_hms(paste0("2019-01-01",timings$End))))

taxi_speed %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",taxi_speed$start_time)),ymd_hms(paste0("2019-01-01",taxi_speed$end_time)))) %>% filter(!is.na(interval)&miles_per_hr<70)


taxi_speed <-  read.csv("C:/Users/Seant/Desktop/taxi_2019_speed.csv", head = TRUE)
taxi_speed$trip_start_timestamp <- ymd_hms(taxi_speed$trip_start_timestamp)
taxi_speed$trip_end_timestamp <- ymd_hms(taxi_speed$trip_end_timestamp)


taxi_speed %<>% mutate(start_time = format(trip_start_timestamp,"%H:%M:%S"),
                 end_time=format(trip_end_timestamp,"%H:%M:%S"))

taxi_speed %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",taxi_speed$start_time)),ymd_hms(paste0("2019-01-01",taxi_speed$end_time)))) %>% filter(!is.na(interval)&miles_per_hr<70)
taxi_summary <- as.data.frame(seq(ymd_hms('2019-01-01 00:00:00'),ymd_hms('2019-01-01 23:45:00'), by = '15 mins'))
names(taxi_summary)[1] <- "Time"
taxi_summary$Time <- ymd_hms(taxi_summary$Time)
taxi_summary$Speed <-  NA
for(i in seq_along(taxi_summary$Time)){
  taxi_summary$Speed[i] <- taxi_speed[taxi_summary$Time[i] %within% taxi_speed$interval,] %>%  summarise(mean=mean(miles_per_hr))
}

ggplot(taxi_summary,aes(x=Time, y= as.numeric(Speed)))+geom_line()+ggtitle("Speed Across Time")


for(s in seq_along(taxi_speed$X)){
  taxi_speed$Group[s] <- timings$Group[ymd_hms(paste0("2019-01-01",taxi_speed$end_time[s])) %within% timings$interval] 
}

taxi_speed <- read.csv("C:/Users/Seant/Desktop/taxi_speed with Groups.csv", head = TRUE)

ggplot(taxi_speed,aes(x=as.factor(Group),y=miles_per_hr))+geom_boxplot()

res.aov <- aov(miles_per_hr~as.factor(Group),data=taxi_speed)           
summary(res.aov)
TukeyHSD(res.aov)


write.csv(taxi_speed,"taxi_speed with Groups.csv")


taxi_speed %<>% mutate(Group=recode(Group,
                                    '1' = '1',
                                    '2' = '2',
                                    '3' = '3',
                                    '4' = '3',
                                    '5' = '5'))

write.csv(taxi_speed,"C:/Users/Seant/Desktop/taxi_speed with Groups.csv")

