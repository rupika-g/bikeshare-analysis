library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
library(lubridate)
library(ggplot2)
# library(hms)

# Calculate ride duration and mean ride duration
# Return modified data frame with ride duration column
calculate_trip_duration <- function(data_frame, type_of_rider) {
  data_frame$trip_duration <- difftime(data_frame$ended_at, data_frame$started_at)
  data_frame <- arrange(data_frame, -trip_duration, started_at)
  data_frame <- clean_names(data_frame)
  mean_trip_duration <- mean(data_frame$trip_duration)
  total_no_of_riders <- length(data_frame$ride_id)
  print(total_no_of_riders)
  print(paste("Mean", type_of_rider, mean_trip_duration))
  return(list("month" = data_frame$started_at[1], "mean" = mean_trip_duration, "num_riders" = total_no_of_riders))
}

total_riders_per_station <- function(sub_df, df_name) {
  sub_df %>% group_by(rideable_type) %>% drop_na %>%
    summarize(total_rideable_type = length(rideable_type))
  
  # Most popular 10 routes
  new_sub_df <- head(sub_df %>% drop_na() %>% count(start_station_name, end_station_name, sort= TRUE), 10)
  
  #View(new_sub_df, df_name)
}

rideable_types <- function(sub_df, df_name) {
  types <- sub_df %>% group_by(rideable_type)%>% drop_na %>%
    summarize(total_rideable_type = length(rideable_type))
  print(paste(df_name, types))
}

process_csv <- function(file_name) {
  # Load data
  df <- read_csv(file_name)
  
  # Clean data
  df <- remove_empty(df, which=c("rows"))
  df <- remove_empty(df, which=c("cols"))
  #View(df, "Full")

  #glimpse(df)

  # Create data frames for member and casual riders
  casual_df <- df %>% filter(member_casual == "casual")
  member_df <- df %>% filter(member_casual == "member")
  
  # Analyze data
  
  casual_trip_details <- calculate_trip_duration(casual_df, "casual")
  member_trip_details <- calculate_trip_duration(member_df, "member")
  
  total_riders_per_station(casual_df, "Casual")
  total_riders_per_station(member_df, "Member")

  unique(casual_df$rideable_type)
  rideable_types(casual_df, "Casual")
  rideable_types(member_df, "Member")

  return(list("casual" = casual_trip_details, "member" = member_trip_details))
}

csv_file_list = list.files("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/", pattern="*-divvy-tripdata.csv", full.names=TRUE)

# Create list 
casual_trip_duration_list <- c()
casual_trip_month_list <- c()
member_trip_duration_list <- c()
member_trip_month_list <- c()

num_of_casual_riders <- c()
num_of_member_riders <- c()

for (csv_file in csv_file_list) {
  trip_details <- process_csv(csv_file)
  casual_trip_duration_list <- append(casual_trip_duration_list, trip_details$casual$mean)
  casual_trip_month_list <- append(casual_trip_month_list, trip_details$casual$month)
  num_of_casual_riders <- append(num_of_casual_riders, trip_details$casual$num_riders)
  
  member_trip_duration_list <- append(member_trip_duration_list, trip_details$member$mean)
  member_trip_month_list <- append(member_trip_month_list, trip_details$member$month)
  num_of_member_riders <- append(num_of_member_riders, trip_details$member$num_riders)
}

print(casual_trip_duration_list)

# Creating df for Mean
mean_df <- data.frame(Month=casual_trip_month_list, Mean_casual=casual_trip_duration_list, Mean_member=member_trip_duration_list)
#casual_mean_df <- mean_df %>% arrange(mdy(casual_mean_df$Month))
number_of_riders_df <- data.frame(Month=casual_trip_month_list, casual_riders=num_of_casual_riders, member_riders=num_of_member_riders)

# Visualization
ggplot(mean_df, aes(x= Month)) + 
  geom_line(aes(y= Mean_casual, color= "green"))+
  geom_line(aes(y= Mean_member, color= "red"))+
  theme(axis.text.x = element_text(angle = 45))

ggplot(number_of_riders_df, aes(x= Month)) + 
  geom_line(aes(y= casual_riders, color= "green"))+
  geom_line(aes(y= member_riders, color= "red"))+
  theme(axis.text.x = element_text(angle = 45))
