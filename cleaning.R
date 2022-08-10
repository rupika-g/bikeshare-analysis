library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
# library(hms)


process_data <- function(data_frame, type_of_rider) {
  data_frame$trip_duration <- difftime(data_frame$ended_at, data_frame$started_at)
  data_frame <- arrange(data_frame, -trip_duration)
  data_frame <- clean_names(data_frame)
  mean_trip_duration <- mean(data_frame$trip_duration)
  print(paste("Mean", type_of_rider, mean_trip_duration))
  return(data_frame)
}

process_csv <- function(file_name) {
  df <- read_csv(file_name)
  View(df, "Full")

  glimpse(df)
  
  member_df <- df %>% filter(member_casual == "member")
  casual_df <- df %>% filter(member_casual == "casual")
  
  process_events(member_df, "Member")
  process_events(casual_df, "Casual")
  
  # casual_df %>% drop_na %>%
  # summarize(total_rideable = count(casual_df,rideable_type))
}

process_events <- function(sub_df, df_name) {
  sub_df %>% group_by(rideable_type) %>% drop_na %>%
    summarize(total_rideable_type = length(rideable_type))
  #unique(sub_df$rideable_type) 
  
  
  #sub_df <- process_data(sub_df, "casual")
  #View(casual_df)
  
  #sub_df <- process_data(sub_df, "member")
  #View(sub_df)
  
  # sub_df %>% group_by(rideable_type)%>% drop_na %>%
  #   summarize(total_rideable_type = length(rideable_type))
  
  # sub_df %>% drop_na %>%
  #   summarize(total_rideable_type = count(sub_df,rideable_type))
  
  new_sub_df <- sub_df %>% drop_na() %>% count(start_station_name, end_station_name, sort= TRUE)
  
  View(new_sub_df, df_name)
}
  



process_csv("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/202106-divvy-tripdata.csv")