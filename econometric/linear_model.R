rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# keep only data from starting_date on
starting_date <- "2021-01-01"

filter_by_timestamp <- function(df) {
  df %>% 
    filter(timestamp >= starting_date) %>%
    select(timestamp, close)              
}

filtered_list <- lapply(list_indeces, filter_by_timestamp)

# collapse in a unique dataframe
indeces <- reduce(filtered_list, full_join, by = "timestamp")
names(indeces) <- c("timestamp",names(list_indeces))
indeces$NIFTY500 <- NULL

# compute logreturns
minute <- 1
returns <- indeces %>%
  filter(minute(timestamp) %% minute == 0) %>%
  mutate(across(
    -timestamp,
    ~ asinh(./lag(.)),
    .names = "log_ret_{.col}" 
  )) %>%
  select(timestamp, starts_with("log_ret_"))

# linear model log_ret_NIFTY50 = alpha + beta * log_ret_NIFTYMIDCAP100
model <- lm(log_ret_NIFTY50 ~ log_ret_NIFTYMIDCAP100, data = returns)
summary(model)

# inspection for hypotesis
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

plot(returns$log_ret_NIFTYMIDCAP100, returns$log_ret_NIFTY50)

# date of outliers, remove them (covid possible explanation)
outliers_idx <- which(abs(returns$log_ret_NIFTYMIDCAP100)>0.02)
returns[outliers_idx,]$timestamp

returns <- returns %>%
  slice(-outliers_idx)

# linear model log_ret_NIFTY50 = alpha + beta * log_ret_NIFTYMIDCAP100
model <- lm(log_ret_NIFTY50 ~ log_ret_NIFTYMIDCAP100, data = returns)
summary(model)

# inspection for hypotesis
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))



