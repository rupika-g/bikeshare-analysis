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
  data_frame <- arrange(data_frame, -trip_duration)
  data_frame <- clean_names(data_frame)
  mean_trip_duration <- mean(data_frame$trip_duration)
  print(paste("Mean", type_of_rider, mean_trip_duration))
  year_and_month <- paste(month(data_frame$started_at[1]), "/", year(data_frame$started_at[1]))
  return(list("mean" = mean_trip_duration, "month" = year_and_month))
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

  return(casual_trip_details)
}

csv_file_list = list.files("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/", pattern="*-divvy-tripdata.csv", full.names=TRUE)

# Create list 
casual_trip_duration_list <- c()
casual_trip_month_list <- c()

for (csv_file in csv_file_list) {
 casual_mean_trip_details <- process_csv(csv_file)
 casual_trip_duration_list <- append(casual_trip_duration_list, casual_mean_trip_details$mean)
 casual_trip_month_list <- append(casual_trip_month_list, casual_mean_trip_details$month)
}

print(casual_trip_duration_list)

# Creating df for Mean
mean_df <- data.frame(Month=casual_trip_month_list, Mean_casual=casual_trip_duration_list)

ggplot(mean_df, aes(x= Mean_casual, y= Month, group=1)) + geom_line()+ geom_point()

#casual_mean_trip_details <- process_csv("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/202106-divvy-tripdata.csv")
#print(casual_mean_trip_details)