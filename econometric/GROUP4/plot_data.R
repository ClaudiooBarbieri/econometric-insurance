rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# select the stock/index you want to print
df <- list_indeces$NIFTY50
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, tz = "Asia/Kolkata"))

# select day
day_start <- "2017-01-02"
day_end <- "2020-12-31"
start <- as.POSIXct(paste(day_start, "09:15:00"), tz = "Asia/Kolkata")
end <- as.POSIXct(paste(day_end, "15:29:00"), tz = "Asia/Kolkata")

# extract from data frame
df_plot <- df %>%
  filter(timestamp >= start & 
           timestamp <= end)

ggplot(df_plot, aes(x = timestamp, y = close)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = paste("Time Series Plot", paste(day_start, day_end, sep = " - "), sep = " : "),
       x = "Time",
       y = "Value") +
  theme_minimal()

# if plotting more days, and do not want to see gap for closed market
df_plot <- df_plot %>%
  arrange(timestamp) %>%
  mutate(time_index = row_number())

ggplot(df_plot, aes(x = time_index, y = close)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = paste("Time Series Plot", paste(day_start, day_end, sep = " - "), sep = " : "),
       x = "Time Index",
       y = "Close Price") +
  theme_minimal()

# define general function to plot time series, as series of point
plott <- function(df, xx, yy) {
  
  ggplot(df, aes(x = !!sym(xx), y = !!sym(yy))) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = paste("Time Series Plot: ", yy),
         x = xx,
         y = yy) +
    theme_minimal()
}



