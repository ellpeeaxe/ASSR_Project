library(tidyverse)
library(lubridate)
library(magrittr)
library(plotly)
library(feather)

#Step 1: Bin by Trip Counts

#query = """
#    SELECT trip_miles,trip_start_timestamp, trip_end_timestamp, trip_seconds,  trip_miles/trip_seconds*3600 as miles_per_hr
#    FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`
#    WHERE EXTRACT (YEAR from trip_start_timestamp) = 2019 AND trip_miles  > 0 AND trip_seconds >0
#"""
taxi <- read_feather("C:/Users/Seant/Desktop/taxi")
taxi <-  read.csv("C:/Users/Seant/Desktop/taxi_2019_time.csv")


taxi$trip_start_timestamp <- ymd_hms(taxi$trip_start_timestamp)
taxi$trip_end_timestamp <- ymd_hms(taxi$trip_end_timestamp)


taxi %<>% mutate(start_time = format(trip_start_timestamp,"%H:%M:%S"),
                  end_time=format(trip_end_timestamp,"%H:%M:%S"))

taxi %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",taxi$start_time))
                                     ,ymd_hms(paste0("2019-01-01",taxi$end_time)))) %>% filter(!is.na(interval))


taxi_summary <- as.data.frame(seq(ymd_hms('2019-01-01 00:00:00'),ymd_hms('2019-01-01 23:45:00'), by = '15 mins'))
taxi_summary <- as.data.frame(taxi_summary[c(20:nrow(taxi_summary),1:19),])
names(taxi_summary)[1] <- "Time"
taxi_summary$Time <- ymd_hms(taxi_summary$Time)
taxi_summary$Count <-  NA
for(i in seq_along(taxi_summary$Time)){
  taxi_summary$Count[i] <- sum(taxi_summary$Time[i] %within% taxi$interval)
}

taxi_summary$Group <- as.factor(cumsum(taxi_summary$Count) %/% (ceiling(sum(taxi_summary$Count) / 5)) + 1)

ggplot(taxi_summary, aes(x=Time,y=Count,colour=Group))+geom_point() + 
  ggtitle("2019 Traffic Count") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),axis.text.x=element_text(angle = 45,hjust =1))+
  scale_x_datetime(date_breaks = '1 hour',
                   date_labels ='%H:%M')


taxi$start_month <- month(taxi$trip_start_timestamp,label=TRUE)
taxi$start_year <-year(taxi$trip_start_timestamp)
taxi$start_wday <-  wday(taxi$trip_start_timestamp,label=TRUE)
taxi$start_hour <-  hour(taxi$trip_start_timestamp)
taxi$start_day <-  as.factor(as.Date(taxi$trip_start_timestamp))
                                                  

taxi_timings <- data.frame(Group=1:6,Start=format(taxi_summary[!duplicated(taxi_summary$Group, fromLast=FALSE), "Time"],"%H:%M:%S"),
                           End=format(taxi_summary[!duplicated(taxi_summary$Group, fromLast=TRUE), "Time"],"%H:%M:%S"))


month <- taxi %>%  group_by(start_month) %>%  summarise(Taxi= n_distinct(taxi_id))
day <- taxi %>%  group_by(start_day) %>%  summarise(Taxi= n_distinct(taxi_id))


write.csv(taxi_timings,"taxi_timing.csv")

#Step 2 : ANOVA Using Speed

#query = """
#    SELECT trip_miles,trip_start_timestamp, trip_end_timestamp, trip_seconds,  trip_miles/trip_seconds*3600 as miles_per_hr
#    FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`
#    WHERE EXTRACT (YEAR from trip_start_timestamp) = 2019 AND pickup_community_area = dropoff_community_area AND trip_miles  > 0 AND trip_seconds >0
#"""


timings %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",timings$Start)),ymd_hms(paste0("2019-01-01",timings$End))))
names(timings)[1] <- "Group"

taxi_speed <-  read.csv("C:/Users/Seant/Desktop/taxi_speed.csv", head = TRUE)



taxi_speed$trip_start_timestamp <- ymd_hms(taxi_speed$trip_start_timestamp)
taxi_speed$trip_end_timestamp <- ymd_hms(taxi_speed$trip_end_timestamp)

taxi_speed %<>% mutate(start_time = format(trip_start_timestamp,"%H:%M:%S"),
                 end_time=format(trip_end_timestamp,"%H:%M:%S"))

taxi_speed %<>% mutate(interval = interval(ymd_hms(paste0("2019-01-01",taxi_speed$start_time)),
                                           ymd_hms(paste0("2019-01-01",taxi_speed$end_time)))) %>% filter(!is.na(interval)&miles_per_hr<70)

taxi_summary <- as.data.frame(seq(ymd_hms('2019-01-01 00:00:00'),ymd_hms('2019-01-01 23:45:00'), by = '15 mins'))
names(taxi_summary)[1] <- "Time"
taxi_summary$Time <- ymd_hms(taxi_summary$Time)
taxi_summary$Speed <-  NA
for(i in seq_along(taxi_summary$Time)){
  taxi_summary$Speed[i] <- taxi_speed[taxi_summary$Time[i] %within% taxi_speed$interval,] %>%  summarise(mean=mean(miles_per_hr))
}


for(s in seq_along(taxi_speed$X)){
  taxi_speed$Group[s] <- timings$Group[ymd_hms(paste0("2019-01-01",taxi_speed$end_time[s])) %within% timings$interval] 
}

ggplot(taxi_speed,aes(x=as.factor(Group),y=miles_per_hr))+geom_boxplot()+ggtitle("Speed by Groups")

res.aov <- aov(miles_per_hr~as.factor(Group),data=taxi_speed)           
summary(res.aov)
TukeyHSD(res.aov)
 

taxi_speed %<>% mutate(Group_Name=recode(Group,
                                    '1' = 'AM Period',
                                    '2' = 'Lunch Period',
                                    '3' = 'PM Period',
                                    '4' = 'PM Period',
                                    '5' = 'Night'))
