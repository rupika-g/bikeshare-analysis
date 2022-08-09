library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
# library(hms)

process_csv <- function(file_name) {
  df <- read_csv(file_name)
  
  glimpse(df)
  
  member_df <- df %>% filter(member_casual == "member")
  casual_df <- df %>% filter(member_casual == "casual")
  # casual_df %>% drop_na %>%
  # summarize(total_rideable = count(casual_df,rideable_type))
  casual_df %>% group_by(rideable_type) %>% drop_na %>%
    summarize(total_rideable_type = length(rideable_type))
  #unique(member_df$rideable_type) 
  

  process_data <- function(data_frame, type_of_rider) {
    data_frame$trip_duration <- difftime(data_frame$ended_at, data_frame$started_at)
    data_frame <- arrange(data_frame, -trip_duration)
    data_frame <- clean_names(data_frame)
    mean_trip_duration <- mean(data_frame$trip_duration)
    print(paste("Mean", type_of_rider, mean_trip_duration))
    return(data_frame)
  }

  #casual_df <- process_data(casual_df, "casual")
  #View(casual_df)

  #member_df <- process_data(member_df, "member")
  #View(member_df)
  
  member_df %>% group_by(rideable_type)%>% drop_na %>%
    summarize(total_rideable_type = length(rideable_type))
  
  member_df %>% drop_na %>%
    summarize(total_rideable_type = count(member_df,rideable_type))
}

process_csv("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/202106-divvy-tripdata.csv")
