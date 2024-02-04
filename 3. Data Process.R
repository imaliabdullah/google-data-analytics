## Data Pre-processing and Cleaning
### Creating a single csv file with multiple data set

# first we load the libraries
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)

# now load the csv files from the directory
df_jan01 <- read.csv('202201-divvy-tripdata.csv')
df_feb02 <- read.csv('202202-divvy-tripdata.csv')
df_mar03 <- read.csv('202203-divvy-tripdata.csv')
df_apr04 <- read.csv('202204-divvy-tripdata.csv')
df_may05 <- read.csv('202205-divvy-tripdata.csv')
df_jun06 <- read.csv('202206-divvy-tripdata.csv')
df_jul07 <- read.csv('202207-divvy-tripdata.csv')
df_aug08 <- read.csv('202208-divvy-tripdata.csv')
df_sep09 <- read.csv('202209-divvy-publictripdata.csv')
df_oct10 <- read.csv('202210-divvy-tripdata.csv')
df_nov11 <- read.csv('202211-divvy-tripdata.csv')
df_dec12 <- read.csv('202212-divvy-tripdata.csv')

# merge all the data sets into one data frame
cyclistic_df <- rbind(df_jan01, df_feb02, df_mar03, df_apr04, df_may05, df_jun06,
                      df_jul07, df_aug08, df_sep09, df_oct10, df_nov11, df_dec12)

# now remove the single month data set from the environment
remove(df_jan01, df_feb02, df_mar03, df_apr04, df_may05, df_jun06,
      df_jul07, df_aug08, df_sep09, df_oct10, df_nov11, df_dec12)

# create a duplicate data frame
cyclistic_date <- cyclistic_df 

# calculate the ride length by subtracting ended_at time from started_at time 
# and convert it into minutes
cyclistic_date$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units='mins')
cyclistic_date$ride_length <- round(cyclistic_date$ride_length, digits = 1)

# now creating a column for day of week, month, day, year, time, hour
#default format is yyyy-mm-dd, use start date
cyclistic_date$date <- as.Date(cyclistic_date$started_at) 
#calculate the day of week
cyclistic_date$day_of_week <- wday(cyclistic_df$started_at) 
#create column for day of week
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A") 
#create column for month
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m") 
#create column for day
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d") 
#create column for year
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y") 
#format time as HH:MM:SS
cyclistic_date$time <- format(as.Date(cyclistic_date$started_at), "%H:%M:%S") 
# now we are using the POSIXct
cyclistic_date$time <- format(as.POSIXct(cyclistic_date$started_at), format = "%H:%M:%S")

# now we assign the time data to the time column
cyclistic_date$hour <- format(as.POSIXct(cyclistic_date$started_at), format = "%H")


#create column for different seasons: Spring, Summer, Fall, Winter
cyclistic_date <- cyclistic_date %>% mutate(season = 
                                              case_when(month == "12" ~ "Winter",
                                                        month == "01" ~ "Winter",
                                                        month == "02" ~ "Winter",
                                                        month == "03" ~ "Spring",
                                                        month == "04" ~ "Spring",
                                                        month == "05" ~ "Spring",
                                                        month == "06"  ~ "Summer",
                                                        month == "07"  ~ "Summer",
                                                        month == "08"  ~ "Summer",
                                                        month == "09" ~ "Fall",
                                                        month == "10" ~ "Fall",
                                                        month == "11" ~ "Fall")
                                            
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_date <- cyclistic_date %>% mutate(time_of_day = 
                                              case_when(hour == "00" ~ "Night",
                                                        hour == "01" ~ "Night",
                                                        hour == "02" ~ "Night",
                                                        hour == "03" ~ "Night",
                                                        hour == "04" ~ "Night",
                                                        hour == "05" ~ "Night",
                                                        hour == "06" ~ "Morning",
                                                        hour == "07" ~ "Morning",
                                                        hour == "08" ~ "Morning",
                                                        hour == "09" ~ "Morning",
                                                        hour == "10" ~ "Morning",
                                                        hour == "11" ~ "Morning",
                                                        hour == "12" ~ "Afternoon",
                                                        hour == "13" ~ "Afternoon",
                                                        hour == "14" ~ "Afternoon",
                                                        hour == "15" ~ "Afternoon",
                                                        hour == "16" ~ "Afternoon",
                                                        hour == "17" ~ "Afternoon",
                                                        hour == "18" ~ "Evening",
                                                        hour == "19" ~ "Evening",
                                                        hour == "20" ~ "Evening",
                                                        hour == "21" ~ "Evening",
                                                        hour == "22" ~ "Evening",
                                                        hour == "23" ~ "Evening")
)

#create a column for the month using the full month name
cyclistic_date <- cyclistic_date %>% mutate(month = 
                                              case_when(month == "01" ~ "January",
                                                        month == "02" ~ "February",
                                                        month == "03" ~ "March",
                                                        month == "04" ~ "April",
                                                        month == "05" ~ "May",
                                                        month == "06" ~ "June",
                                                        month == "07" ~ "July",
                                                        month == "08" ~ "August",
                                                        month == "09" ~ "September",
                                                        month == "10" ~ "October",
                                                        month == "11" ~ "November",
                                                        month == "12" ~ "December")
)

str(cyclistic_date)

#clean the data 
cyclistic_date <- na.omit(cyclistic_date) #remove rows with NA values 
cyclistic_date <- distinct(cyclistic_date) #remove duplicate rows
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_date <- cyclistic_date %>% #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng,end_lat,end_lng))


#view the final data 
View(cyclistic_date)

#created a new dataframe to use in tableau
cyclistic_final <- cyclistic_date

#clean the data 
cyclistic_final <- cyclistic_final %>% #remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

fwrite(cyclistic_final, 'cyclistic_data.csv')