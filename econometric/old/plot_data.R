rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# select the stock/index you want to print
df <- list_nifty50$ADANIPORTS
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, tz = "Asia/Kolkata"))

# select day
day <- "2017-01-01"
start <- as.POSIXct(day, tz = "Asia/Kolkata")
end <- as.POSIXct(paste(day, "23:59:59"), tz = "Asia/Kolkata")

# extract from data frame
df_daily <- df %>%
  filter(timestamp >= start & 
           timestamp <= end)


ggplot(df_daily, aes(x = timestamp, y = close)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = paste("Time Series Plot", day, sep = " : "),
       x = "Time",
       y = "Value") +
  theme_minimal()


