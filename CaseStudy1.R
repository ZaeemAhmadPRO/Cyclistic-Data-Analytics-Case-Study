install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library('readr')
library('readxl')
library('ggplot2')
library('patchwork')
library('stringr')
library('scales')

setwd("C:/Users/hp/Downloads/Case study 1 - Cyclistic_BikeShare")
getwd()

#----
#Pre-processing
#
file_list <- list.files(
  path = "C:/Users/hp/Downloads/Case study 1 - Cyclistic_BikeShare",
  pattern = "*.csv",
  full.names = TRUE
)

cyclistic_2025 <- file_list %>%
  map_dfr(read_csv)

glimpse(cyclistic_2025)
summary(cyclistic_2025$started_at)

cyclistic_2025 <- cyclistic_2025 %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  )


cyclistic_2025 <- cyclistic_2025 %>%
  mutate(
    day_of_week = wday(started_at, week_start = 7)
  )

table(cyclistic_2025$day_of_week)

# data cleaning step

cyclistic_2025 <- cyclistic_2025 %>%
  filter(
    ride_length > 0,
    !is.na(ride_length),
    !is.na(member_casual)
  )

summary(cyclistic_2025$ride_length)
table(cyclistic_2025$member_casual)
table(cyclistic_2025$rideable_type)
table(cyclistic_2025$day_of_week)

table(cyclistic_2025$member_casual)


cyclistic_2025 %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    ride_count = n()
  )




#----
#Analysis 


custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    panel.grid.minor = element_blank()
  )


brand_colors <- function(n_red, n_yellow, n_green, n_blue) {
  c(
    rep("#DB4437", n_red),
    rep("#F4B400", n_yellow),
    rep("#0F9D58", n_green),
    rep("#4285F4", n_blue)
  )
}

rider_colors <- c(
  "casual" = "#F4B400",
  "member" = "#DB4437"
)

exists("cyclistic_2025")
exists("custom_theme")
exists("rider_colors")

# Ride length distribution
print(
  cyclistic_2025 %>%
    filter(ride_length < 120) %>%
    ggplot(aes(x = ride_length, fill = member_casual)) +
    geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
    scale_fill_manual(values = rider_colors) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Distribution of Ride Length by Rider Type",
      x = "Ride Length (minutes)",
      y = "Number of Rides",
      fill = "Rider Type"
    ) +
    custom_theme
)

#number of riders by day of week
print(
  cyclistic_2025 %>%
    group_by(member_casual, day_of_week) %>%
    summarise(ride_count = n(), .groups = "drop") %>%
    ggplot(aes(x = factor(day_of_week), y = ride_count, fill = member_casual)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = rider_colors) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Number of Rides by Day of Week",
      x = "Day of Week (1 = Sunday, 7 = Saturday)",
      y = "Number of Rides",
      fill = "Rider Type"
    ) +
    custom_theme
)

# average ride length by day of week
print(
  cyclistic_2025 %>%
    group_by(member_casual, day_of_week) %>%
    summarise(avg_ride_length = mean(ride_length), .groups = "drop") %>%
    ggplot(aes(
      x = factor(day_of_week),
      y = avg_ride_length,
      color = member_casual,
      group = member_casual
    )) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = rider_colors) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Average Ride Length by Day of Week",
      x = "Day of Week",
      y = "Average Ride Length (minutes)",
      color = "Rider Type"
    ) +
    custom_theme
)

# bike type usage by rider type
print(
  cyclistic_2025 %>%
    group_by(member_casual, rideable_type) %>%
    summarise(ride_count = n(), .groups = "drop") %>%
    ggplot(aes(x = rideable_type, y = ride_count, fill = member_casual)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = rider_colors) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Bike Type Usage by Rider Type",
      x = "Bike Type",
      y = "Number of Rides",
      fill = "Rider Type"
    ) +
    custom_theme
)

# Monthly ride trends (2025)
cyclistic_2025 <- cyclistic_2025 %>%
  mutate(month = month(started_at, label = TRUE))
print(
  cyclistic_2025 %>%
    group_by(member_casual, month) %>%
    summarise(ride_count = n(), .groups = "drop") %>%
    ggplot(aes(x = month, y = ride_count, color = member_casual, group = member_casual)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = rider_colors) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Monthly Ride Trends by Rider Type",
      x = "Month",
      y = "Number of Rides",
      color = "Rider Type"
    ) +
    custom_theme
)

# Hourly rides pattern
cyclistic_2025 <- cyclistic_2025 %>%
  mutate(hour = hour(started_at))

print(
  cyclistic_2025 %>%
    group_by(member_casual, hour) %>%
    summarise(ride_count = n(), .groups = "drop") %>%
    ggplot(aes(x = hour, y = ride_count, color = member_casual)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = rider_colors) +
    scale_x_continuous(breaks = 0:23) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Hourly Ride Patterns by Rider Type",
      x = "Hour of Day",
      y = "Number of Rides",
      color = "Rider Type"
    ) +
    custom_theme
)

# percentage of total rides by rider type
rider_share <- cyclistic_2025 %>%
  group_by(member_casual) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  mutate(
    percentage = total_rides / sum(total_rides),
    label = percent(percentage, accuracy = 0.1)
  )

rider_share
print(
  ggplot(rider_share, aes(x = "", y = percentage, fill = member_casual)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = rider_colors) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 5,
      color = "black"
    ) +
    labs(
      title = "Percentage of Total Rides by Rider Type",
      fill = "Rider Type"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
)

# Five‑Number Summary of Ride Length
print(
  ggplot(
    cyclistic_2025,
    aes(x = member_casual, y = ride_length, fill = member_casual)
  ) +
    geom_boxplot(outlier.shape = NA, alpha = 1) +
    scale_fill_manual(values = rider_colors) +
    scale_y_continuous(
      name = "Ride Length (minutes)",
      breaks = seq(0, 45, 5),
      limits = c(0, 45)
    ) +
    labs(
      title = "Five-Number Summary of Ride Length by Rider Type",
      x = "Rider Type"
    ) +
    custom_theme +
    theme(legend.position = "none")
)

# Stations analysis

# Members – Start Stations
member_start <- cyclistic_2025 %>%
  filter(member_casual == "member", !is.na(start_station_name)) %>%
  count(start_station_name, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Members – End Stations
member_end <- cyclistic_2025 %>%
  filter(member_casual == "member", !is.na(end_station_name)) %>%
  count(end_station_name, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Casual – Start Stations
casual_start <- cyclistic_2025 %>%
  filter(member_casual == "casual", !is.na(start_station_name)) %>%
  count(start_station_name, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Casual – End Stations
casual_end <- cyclistic_2025 %>%
  filter(member_casual == "casual", !is.na(end_station_name)) %>%
  count(end_station_name, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)


# Members – Start
p1 <- ggplot(member_start,
             aes(x = count, y = reorder(start_station_name, count))) +
  geom_col(fill = "#DB4437") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Most Used Start Stations (Members)",
    x = "# of Trips",
    y = "Station Address"
  ) +
  custom_theme


# Members – End
p2 <- ggplot(member_end,
             aes(x = count, y = reorder(end_station_name, count))) +
  geom_col(fill = "#DB4437") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Most Used End Stations (Members)",
    x = "# of Trips",
    y = NULL
  ) +
  custom_theme


# Casual – Start
p3 <- ggplot(casual_start,
             aes(x = count, y = reorder(start_station_name, count))) +
  geom_col(fill = "#F4B400") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Most Used Start Stations (Casual Riders)",
    x = "# of Trips",
    y = "Station Address"
  ) +
  custom_theme


# Casual – End
p4 <- ggplot(casual_end,
             aes(x = count, y = reorder(end_station_name, count))) +
  geom_col(fill = "#F4B400") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Most Used End Stations (Casual Riders)",
    x = "# of Trips",
    y = NULL
  ) +
  custom_theme


# Combine into 2x2 layout
final_station_plot <- (p1 | p2) / (p3 | p4)
print(final_station_plot)

