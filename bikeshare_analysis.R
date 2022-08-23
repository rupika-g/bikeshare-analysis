library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
library(lubridate)
library(ggplot2)

# Calculate ride duration and mean ride duration
# Return modified data frame with ride duration column
calculate_trip_duration <- function(data_frame, type_of_rider) {
  data_frame$trip_duration <- difftime(data_frame$ended_at, data_frame$started_at)
  data_frame <- arrange(data_frame, -trip_duration, started_at)
  data_frame <- clean_names(data_frame)
  mean_trip_duration <- mean(data_frame$trip_duration)
  total_no_of_riders <- length(data_frame$ride_id)
  # print(paste("Mean", type_of_rider, mean_trip_duration))
  return(list(
    "month" = data_frame$started_at[1], "mean" = mean_trip_duration,
    "num_riders" = total_no_of_riders
  ))
}

total_riders_per_station <- function(sub_df, df_name) {
  sub_df %>%
    group_by(rideable_type) %>%
    summarize(total_rideable_type = length(rideable_type))

  # Most popular 10 routes
  new_sub_df <- sub_df %>%
    drop_na() %>%
    count(start_station_name, end_station_name, sort = TRUE) %>%
    filter(start_station_name == end_station_name)

  # View(new_sub_df, df_name)
  return(new_sub_df)
}
# rideable types
rideable_types <- function(sub_df, df_name) {
  types <- sub_df %>%
    group_by(rideable_type) %>%
    summarize(total_rideable_type = length(rideable_type))
  return(types)
}

# weekly riders and mean duration
day_of_week <- function(sub_df, df_name) {
  sub_df$trip_duration <- difftime(sub_df$ended_at, sub_df$started_at)
  number_of_rides <- sub_df %>%
    group_by(week = weekdays(started_at)) %>%
    summarize(number_of_rides = length(ride_id))
  mean_duration <- sub_df %>%
    group_by(week = weekdays(started_at)) %>%
    summarize(mean_ride_duration = mean(trip_duration))
  return(list("number_of_rides" = number_of_rides, "mean_duration" = mean_duration))
}

process_csv <- function(file_name) {
  # Load data
  df <- read_csv(file_name)

  # Clean data
  df <- remove_empty(df, which = c("rows"))
  df <- remove_empty(df, which = c("cols"))
  # View(df, "Full")

  # glimpse(df)

  # Create data frames for member and casual riders
  casual_df <- df %>% filter(member_casual == "casual")
  member_df <- df %>% filter(member_casual == "member")

  # Analyze data

  casual_trip_details <- calculate_trip_duration(casual_df, "casual")
  member_trip_details <- calculate_trip_duration(member_df, "member")

  unique(casual_df$rideable_type)
  number_of_casual_types <- rideable_types(casual_df, "Casual")
  number_of_member_types <- rideable_types(member_df, "Member")

  casual_day_of_week <- day_of_week(casual_df, "Casual")
  member_day_of_week <- day_of_week(member_df, "Member")

  casual_riders_returning_same_station <- total_riders_per_station(casual_df, "Casual")
  casual_riders_returning_same_station <- casual_riders_returning_same_station[c("start_station_name", "n")] # Select only start_station_name and number of riders from the dataframe
  member_riders_returning_same_station <- total_riders_per_station(member_df, "Member")
  member_riders_returning_same_station <- member_riders_returning_same_station[c("start_station_name", "n")]

  return(list(
    "casual" = casual_trip_details, "member" = member_trip_details,
    "casual_types" = number_of_casual_types, "member_types" = number_of_member_types,
    "casual_returning_same_station" = casual_riders_returning_same_station,
    "member_returning_same_station" = member_riders_returning_same_station,
    "casual_day_of_week" = casual_day_of_week,
    "member_day_of_week" = member_day_of_week
  ))
}

csv_file_list <- list.files("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/", pattern = "*-divvy-tripdata.csv", full.names = TRUE)
number_csv_files <- length(csv_file_list)

# Create list
casual_trip_duration_list <- c()
casual_trip_month_list <- c()
member_trip_duration_list <- c()
member_trip_month_list <- c()

num_of_casual_riders <- c()
num_of_member_riders <- c()

casual_rideable_types <- NULL
member_rideable_types <- NULL

casual_riders_returning_same_station <- NULL
member_riders_returning_same_station <- NULL

allday_casual_riders <- NULL
allday_member_riders <- NULL
allday_casual_mean_duration <- NULL
allday_member_mean_duration <- NULL


for (csv_file in csv_file_list) {
  trip_details <- process_csv(csv_file)

  casual_trip_duration_list <- append(casual_trip_duration_list, trip_details$casual$mean)
  casual_trip_month_list <- append(casual_trip_month_list, trip_details$casual$month)
  num_of_casual_riders <- append(num_of_casual_riders, trip_details$casual$num_riders)
  if (is.null(casual_rideable_types)) {
    casual_rideable_types <- trip_details$casual_types
  } else {
    casual_rideable_types <- merge(x = casual_rideable_types, y = trip_details$casual_types, by = "rideable_type", all = TRUE)
  }
  if (is.null(casual_riders_returning_same_station)) {
    casual_riders_returning_same_station <- trip_details$casual_returning_same_station
  } else {
    casual_riders_returning_same_station <- merge(
      x = casual_riders_returning_same_station,
      y = trip_details$casual_returning_same_station, by = "start_station_name", all = TRUE
    )
  }
  if (is.null(allday_casual_riders)) {
    allday_casual_riders <- trip_details$casual_day_of_week$number_of_rides
    allday_casual_mean_duration <- trip_details$casual_day_of_week$mean_duration
  } else {
    allday_casual_riders <- merge(x = allday_casual_riders, y = trip_details$casual_day_of_week$number_of_rides, by = "week", all = TRUE)
    allday_casual_mean_duration <- merge(x = allday_casual_mean_duration, y = trip_details$casual_day_of_week$mean_duration, by = "week", all = TRUE)
  }

  member_trip_duration_list <- append(member_trip_duration_list, trip_details$member$mean)
  member_trip_month_list <- append(member_trip_month_list, trip_details$member$month)
  num_of_member_riders <- append(num_of_member_riders, trip_details$member$num_riders)
  if (is.null(member_rideable_types)) {
    member_rideable_types <- trip_details$member_types
  } else {
    member_rideable_types <- merge(x = member_rideable_types, y = trip_details$member_types, by = "rideable_type", all = TRUE)
  }
  if (is.null(member_riders_returning_same_station)) {
    member_riders_returning_same_station <- trip_details$member_returning_same_station
  } else {
    member_riders_returning_same_station <- merge(
      x = member_riders_returning_same_station,
      y = trip_details$member_returning_same_station, by = "start_station_name", all = TRUE
    )
  }
  if (is.null(allday_member_riders)) {
    allday_member_riders <- trip_details$member_day_of_week$number_of_rides
    allday_member_mean_duration <- trip_details$member_day_of_week$mean_duration
  } else {
    allday_member_riders <- merge(x = allday_member_riders, y = trip_details$member_day_of_week$number_of_rides, by = "week", all = TRUE)
    allday_member_mean_duration <- merge(x = allday_member_mean_duration, y = trip_details$member_day_of_week$mean_duration, by = "week", all = TRUE)
  }
}

casual_rideable_types <- data.frame(rideable_type = casual_rideable_types$rideable_type, number_of_rides = rowSums(casual_rideable_types[, 2:number_csv_files + 1]))
casual_rideable_types$user_type <- "casual"
member_rideable_types <- data.frame(rideable_type = member_rideable_types$rideable_type, number_of_rides = rowSums(member_rideable_types[, 2:number_csv_files + 1]))
member_rideable_types$user_type <- "member"
all_rideable_type <- merge(x = casual_rideable_types, y = member_rideable_types, all = TRUE)
# all_rideable_type <- all_rideable_type %>% rename("Casual Rides" = number_of_rides.x, "Member Rides" = number_of_rides.y)

# View(all_rideable_type)

casual_riders_returning_same_station <- data.frame(
  start_station_name = casual_riders_returning_same_station$start_station_name,
  number_of_rides = rowSums(casual_riders_returning_same_station[, 2:number_csv_files + 1])
)

member_riders_returning_same_station <- data.frame(
  start_station_name = member_riders_returning_same_station$start_station_name,
  number_of_rides = rowSums(member_riders_returning_same_station[, 2:number_csv_files + 1])
)

# Creating df for Mean
mean_df <- data.frame(Month = casual_trip_month_list, Mean_casual = casual_trip_duration_list, Mean_member = member_trip_duration_list)
# casual_mean_df <- mean_df %>% arrange(mdy(casual_mean_df$Month))

# Creating df for total number of rides
number_of_riders_df <- data.frame(Month = casual_trip_month_list, casual_riders = num_of_casual_riders, member_riders = num_of_member_riders)

# Creating df for weekly riders
weekly_casual_rides <- data.frame(week = allday_casual_riders$week, number_of_rides = rowSums(allday_casual_riders[, 2:number_csv_files + 1]))
weekly_member_rides <- data.frame(week = allday_member_riders$week, number_of_rides = rowSums(allday_member_riders[, 2:number_csv_files + 1]))
weekly_rides <- data.frame(week = allday_casual_riders$week, casual = weekly_casual_rides$number_of_rides, member = weekly_member_rides$number_of_rides)

# Creating df for weekly rider's mean duration
weekly_casual_mean_ride_duration <- data.frame(week = allday_casual_mean_duration$week, mean_duration = rowSums(sapply(allday_casual_mean_duration[, 1:number_csv_files + 1], function(x) as.numeric(as.character(x)))))
weekly_member_mean_ride_duration <- data.frame(week = allday_member_mean_duration$week, mean_duration = rowSums(sapply(allday_member_mean_duration[, 1:number_csv_files + 1], function(x) as.numeric(as.character(x)))))
weekly_mean_ride_duration <- data.frame(week = allday_casual_mean_duration$week, casual = weekly_casual_mean_ride_duration$mean_duration, member = weekly_member_mean_ride_duration$mean_duration)


# Visualization
# Mean
ggplot(mean_df, aes(x = Month)) +
  geom_line(aes(y = Mean_casual, color = "casual")) +
  geom_line(aes(y = Mean_member, color = "member")) +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Duration of Casual Rides vs Members Rides", subtitle = "Mean Duration in seconds",
    x = "Month", y = "Mean duration(seconds)", colour = "Mean duration"
  )

ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/Total mean duration of casual and member riders.png", width = 2500, height = 1200, units = "px")

# Total number of riders
ggplot(number_of_riders_df, aes(x = Month)) +
  geom_line(aes(y = casual_riders, color = "casual")) +
  geom_line(aes(y = member_riders, color = "members")) +
  labs(
    title = "Number of Casual vs Member Riders", subtitle = "Total number of riders per Month",
    x = "Month", y = "Total number of rides", colour = "Riders"
  ) +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/visualization/Total number of riders.png", width = 2500, height = 1200, units = "px")

# Rideable types
ggplot(all_rideable_type, aes(fill = user_type, y = number_of_rides, x = rideable_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(y = "Number of rides", x = "rideable types", title = "Member vs Casual Rideable Types", fill = "Riders") +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/visualization/Rideable types.png", width = 2500, height = 900, units = "px")

# Returning to same station
casual_data <- head(casual_riders_returning_same_station %>% drop_na() %>% arrange(-number_of_rides), 25)
ggplot(casual_data, aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides, fill = number_of_rides)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Casual Riders Returning to Same Station", subtitle = "Top 20 stations",
    y = "Casual rides", x = "Station name", fill = "Number of rides"
  ) +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) # Without stat='identity' ggplot wants to aggregate your data into counts

ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/visualization/Casual_returning_station_numbers.png", width = 2500, height = 1200, units = "px")

# View(casual_riders_returning_same_station)

member_data <- head(member_riders_returning_same_station %>% drop_na() %>% arrange(-number_of_rides), 25)
ggplot(member_data, aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides, fill = number_of_rides)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Members Returning to Same Station", subtitle = "Top 20 stations",
    y = "Member rides", x = "Station name", fill = "Number of rides"
  ) +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/visualization/Members_returning_station_numbers.png", width = 2500, height = 1200, units = "px")

# View(member_riders_returning_same_station)

# Weekly riders
ggplot(weekly_rides, aes(x = factor(week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), group = 1)) +
  geom_line(aes(y = casual, color = "casual users")) +
  geom_line(aes(y = member, color = "members")) +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Number of Riders Week Wise",
    subtitle = "Total number of casual and member riders weekly", x = "Weekdays", y = "Number of rides", colour = "Rides"
  )
ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/visualization/Riders week wise.png", width = 2500, height = 1200, units = "px")

# Mean duration of rides weekly
ggplot(weekly_mean_ride_duration, aes(x = factor(week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), group = 1)) +
  geom_line(aes(y = casual, color = "casual users")) +
  geom_line(aes(y = member, color = "members")) +
  theme(
    axis.text.x = element_text(angle = 30), axis.text = element_text(size = 8), axis.title = element_text(size = 11),
    plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10), legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Weekdays vs Mean Ride Duration",
    subtitle = "Mean ride duration by casual and member users", x = "Weekdays", y = "Mean ride duration", color = "Ride duration"
  )
ggsave("C:/Users/rramachandran/Downloads/Data Analysis Capstone Project/visualization/Mean duration of rides weekly.png",
  width = 2500, height = 1200, units = "px"
)
