library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(skimr)
library(conflicted)
library(ggplot2)
library(lubridate)
library(viridis)  # For better color scales in heatmaps
#bike_data_2 <- read_csv("C:/Users/LENOVO/Downloads/Google DA cert/Course 8/Case_Study_1/Divvy_Trips_2019_Q1.csv")
#bike_data <- na.omit(bike_data_2)
#View(bike_data)
#write.csv(bike_data, "Divvy_Trips_2019_Q1_Cleaned.csv", row.names = FALSE)
#getwd()

#Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# STEP 1: COLLECT DATA

q1_2019 <- read_csv("C:/Users/LENOVO/Downloads/Google DA cert/Course 8/Case_Study_1/Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("C:/Users/LENOVO/Downloads/Google DA cert/Course 8/Case_Study_1/Divvy_Trips_2020_Q1.csv")

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
# Compare columns

colnames(q1_2019)
colnames(q1_2020)

#Renaming columns  to make them consistent with q1_2020

(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype))

# Inspect the dataframes and look for inaccuracies
str(q1_2019)
str(q1_2020)

# Convert ride_id and rideable_type to char so they can stack properly
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))


# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS



# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are  data frame?
dim(all_trips)  #Dimensions of  data frame?
head(all_trips)  #See first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# In the "member_casual" column, replace 
#"Subscriber" with "member" and "Customer" with "casual"

table(all_trips$member_casual)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))

# Checking to make sure the proper assignments were made
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride.
# This will allow us to aggregate ride data for each month, day, or year.
# Before completing these operations we could only aggregate at the ride level.


all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks
#and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
write.csv(all_trips_v2, file = 'Divvy_Trips_2019-20_Cleaned.csv', row.names=FALSE)
# Filter data for casual users
casual_users <- all_trips_v2 %>%
  filter(member_casual == "casual")



# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS



mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# days of the week are out of order.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by user-type and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday)	# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Count the number of rides per start station and select the top 10
top_start_stations <- casual_users %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(desc(number_of_rides)) %>% 
  head(10)

# Count the number of rides per end station and select the top 10
top_end_stations <- casual_users %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(desc(number_of_rides)) %>% 
  head(10)


# Visualize the top 10 most used START stations by casual users
ggplot(top_start_stations, aes(x = reorder(start_station_name, -number_of_rides), y = number_of_rides)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Most Used Start Stations by Casual Users",
       x = "Start Station",
       y = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualize the top 10 most used END stations by casual users
ggplot(top_end_stations, aes(x = reorder(end_station_name, -number_of_rides), y = number_of_rides)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Top 10 Most Used End Stations by Casual Users",
       x = "End Station",
       y = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Extract hour from started_at and ended_at for time analysis
casual_users <- casual_users %>%
  mutate(start_hour = hour(started_at),
         end_hour = hour(ended_at))

# Count the number of rides per hour for start stations
start_hour_traffic <- casual_users %>%
  group_by(start_station_name, start_hour) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Count the number of rides per hour for end stations
end_hour_traffic <- casual_users %>%
  group_by(end_station_name, end_hour) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Filter for top 10 start and end stations to simplify visualizations
top_start_stations <- start_hour_traffic %>%
  group_by(start_station_name) %>%
  summarise(total_rides = sum(count)) %>%
  arrange(desc(total_rides)) %>%
  head(10)

top_end_stations <- end_hour_traffic %>%
  group_by(end_station_name) %>%
  summarise(total_rides = sum(count)) %>%
  arrange(desc(total_rides)) %>%
  head(10)

start_hour_traffic_top <- start_hour_traffic %>%
  filter(start_station_name %in% top_start_stations$start_station_name)

end_hour_traffic_top <- end_hour_traffic %>%
  filter(end_station_name %in% top_end_stations$end_station_name)

# Visualize start station traffic by hour using a LINE CHART
ggplot(start_hour_traffic_top, aes(x = start_hour, y = count, color = start_station_name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Start Station Traffic by Hour (Top 10 Stations)",
       x = "Hour of the Day",
       y = "Number of Rides",
       color = "Start Station") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Visualize end station traffic by hour using a LINE CHART
ggplot(end_hour_traffic_top, aes(x = end_hour, y = count, color = end_station_name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "End Station Traffic by Hour (Top 10 Stations)",
       x = "Hour of the Day",
       y = "Number of Rides",
       color = "End Station") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Visualize start station traffic by hour using a HEATMAP
ggplot(start_hour_traffic_top, aes(x = start_hour, y = start_station_name, fill = count)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma") +  # Use a colorblind-friendly palette
  labs(title = "Start Station Traffic by Hour (Heatmap)",
       x = "Hour of the Day",
       y = "Start Station",
       fill = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualize end station traffic by hour using a HEATMAP
ggplot(end_hour_traffic_top, aes(x = end_hour, y = end_station_name, fill = count)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma") +  # Use a colorblind-friendly palette
  labs(title = "End Station Traffic by Hour (Heatmap)",
       x = "Hour of the Day",
       y = "End Station",
       fill = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS


# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC
# Change the file location accordingly
# (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data.
# You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
