library(tidyverse) 
library(lubridate)  
library(hms) 
library(data.table) 

cyclistic_final <- read.csv("cyclistic_data.csv")

# Total number of riders
nrow(cyclistic_final)

# member type 
cyclistic_final %>%
  group_by(member_casual) %>%
  count(member_casual)

# Member types and total rides by bike type.
cyclistic_final %>% 
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# Total rides
cyclistic_final %>% 
  group_by(rideable_type) %>%
  count(rideable_type)

# Driving by the hours
# total rides by member type
cyclistic_final %>% 
  group_by(member_casual) %>%
  count(hour) %>%
  print(n=48)

# total rides 
cyclistic_final %>% 
  count(hour) %>%
  print() 


# Riding by Day
# morning
# total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

# Total Rides
cyclistic_final %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

# afternoon 
# total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

# Total Rides
cyclistic_final %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

# evening 
# total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

# Total Rides
cyclistic_final %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

# night
# total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

# Total Rides
cyclistic_final %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

# all times of day
# Total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  count(time_of_day)

# number of rides
cyclistic_final %>%
  group_by(time_of_day) %>%
  count(time_of_day)


# Day of The Week
# total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  count(day_of_week)

# total rides
cyclistic_final %>%
  count(day_of_week)

# Day of The Month
# Total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  count(day) %>%
  print(n = 62)

# total rides 
cyclistic_final %>%
  count(day) %>%
  print()

# MONTH
# total rides by member
cyclistic_final %>%
  group_by(member_casual) %>%
  count(month) %>%
  print(n=24)

cyclistic_final %>%
  count(month)

# SEASONS 

# Spring

# total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(season == "Spring") %>%
  count(season)

# Total rides 
cyclistic_final %>%
  filter(season == "Spring") %>%
  count(season)

# summer
# Total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(season == "Summer") %>%
  count(season)


# Total rides 
cyclistic_final %>%
  filter(season == "Summer") %>%
  count(season)

# Fall

# Total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(season == "Fall") %>%
  count(season)

cyclistic_final %>%
  filter(season == "Fall") %>%
  count(season)

# Winter

# Total rides by member type
cyclistic_final %>%
  group_by(member_casual) %>%
  filter(season == "Winter") %>%
  count(season)

cyclistic_final %>%
  filter(season == "Winter") %>%
  count(season)

# All reasons 
# Total rides by member type
cyclistic_final %>%
  group_by(season, member_casual) %>%
  count(season)

# total rides
cyclistic_final %>%
  group_by(season) %>%
  count(season)


# Average Ride Length
# average of ride_length
cyclistic_avgRide <- mean(cyclistic_final$ride_length)
print(cyclistic_avgRide)

# Member Type 
# average ride_length
cyclistic_final %>% group_by(member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Type of Bike
cyclistic_final %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length), list(time = mean))

cyclistic_final %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length), list(time = mean))


# HOUR
# average ride_length by member type
cyclistic_final %>% group_by(hour,member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=48)

# average ride_length
cyclistic_final %>% group_by(hour) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=24)

# Tıme Of Day 
# morning
# average ride length by member type
cyclistic_final %>% group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length
cyclistic_final %>% filter(time_of_day == "Morning") %>%
  summarise_at(vars(ride_length), list(time=mean))

# afternoon
# average ride length by member type
cyclistic_final %>% group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length
cyclistic_final %>% filter(time_of_day == "Afternoon") %>%
  summarise_at(vars(ride_length), list(time=mean))

# evening 
# average ride length by member type
cyclistic_final %>% group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length
cyclistic_final %>% filter(time_of_day == "Evening") %>%
  summarise_at(vars(ride_length), list(time=mean))


# Night
# average ride length by member type
cyclistic_final %>% group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length
cyclistic_final %>% filter(time_of_day == "Night") %>%
  summarise_at(vars(ride_length), list(time=mean))

# All times of day
# average ride length by member type
cyclistic_final %>% group_by(time_of_day,member_casual) %>% 
  summarise_at(vars(ride_length), list(time=mean))

# average ride length
cyclistic_final %>% group_by(time_of_day) %>% 
  summarise_at(vars(ride_length), list(time=mean))

# DAY OF THE WEEK 
# average ride_length by member type
cyclistic_final %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length), list(time=mean))

# average ride length
cyclistic_final %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length), list(time=mean))


# DAY OF THE MONTH
# average ride_length by member type
cyclistic_final %>% group_by(day,member_casual) %>% 
  summarise_at(vars(ride_length), list(time=mean)) %>%
  print(n=62)

# average ride_length
cyclistic_final %>% group_by(day) %>% 
  summarise_at(vars(ride_length), list(time=mean)) %>%
  print(n=31)

# MONTH
# average ride_length by member type
cyclistic_final %>% group_by(month,member_casual) %>% 
  summarise_at(vars(ride_length), list(time=mean)) %>%
  print(n=24)

# average ride_length
cyclistic_final %>% group_by(month) %>% 
  summarise_at(vars(ride_length), list(time=mean)) %>%
  print(n=12)  


# SEASON
# SPRİNG
# average ride_length by member type
cyclistic_final %>% group_by(member_casual) %>%
  filter(season == "Spring") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length 
cyclistic_final %>% 
  filter(season == "Spring") %>%
  summarise_at(vars(ride_length), list(time=mean))

# SUMMER 
# average ride_length by member type
cyclistic_final %>% group_by(member_casual) %>%
  filter(season == "Summer") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length 
cyclistic_final %>% 
  filter(season == "Winter") %>%
  summarise_at(vars(ride_length), list(time=mean))

# Fall
# average ride_length by member type
cyclistic_final %>% group_by(member_casual) %>%
  filter(season == "Fall") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length 
cyclistic_final %>% 
  filter(season == "Fall") %>%
  summarise_at(vars(ride_length), list(time=mean))

# WİNTER 
# average ride_length by member type
cyclistic_final %>% group_by(member_casual) %>%
  filter(season == "Winter") %>%
  summarise_at(vars(ride_length), list(time=mean))

# average ride length 
cyclistic_final %>% 
  filter(season == "Winter") %>%
  summarise_at(vars(ride_length), list(time=mean))


# ALL SEASONS 
# average ride_length by member type
cyclistic_final %>% group_by(season, member_casual) %>%
  summarise_at(vars(ride_length), list(time=mean))

cyclistic_final %>% group_by(season) %>%
  summarise_at(vars(ride_length), list(time=mean))