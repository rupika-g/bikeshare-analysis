library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
# library(hms)

# Calculate ride duration and mean ride duration
# Return modified data frame with ride duration column
calculate_trip_duration <- function(data_frame, type_of_rider) {
  data_frame$trip_duration <- difftime(data_frame$ended_at, data_frame$started_at)
  data_frame <- arrange(data_frame, -trip_duration)
  data_frame <- clean_names(data_frame)
  mean_trip_duration <- mean(data_frame$trip_duration)
  print(paste("Mean", type_of_rider, mean_trip_duration))
  return(data_frame)
}

total_riders_per_station <- function(sub_df, df_name) {
  sub_df %>% group_by(rideable_type) %>% drop_na %>%
    summarize(total_rideable_type = length(rideable_type))
  
  new_sub_df <- sub_df %>% drop_na() %>% count(start_station_name, end_station_name, sort= TRUE)
  
  View(new_sub_df, df_name)
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

  glimpse(df)

  # Create data frames for member and casual riders
  casual_df <- df %>% filter(member_casual == "casual")
  member_df <- df %>% filter(member_casual == "member")
  
  # Analyze data
  
  casual_df <- calculate_trip_duration(casual_df, "casual")
  member_df <- calculate_trip_duration(member_df, "member")
  
  total_riders_per_station(casual_df, "Casual")
  total_riders_per_station(member_df, "Member")

  unique(casual_df$rideable_type)
  rideable_types(casual_df, "Casual")
  rideable_types(member_df, "Member")
}

process_csv("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/202106-divvy-tripdata.csv")
