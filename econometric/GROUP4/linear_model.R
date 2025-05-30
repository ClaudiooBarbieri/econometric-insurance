rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(patchwork)
library(GGally)
library(quantmod) 
library(TTR)
library(xts)
library(IDPmisc)
library(DescTools)
library(MASS)
library(lmtest)
library(skedastic)
library(sandwich)

# file with info
nifty <- read.csv("Nifty50.csv")

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
plott(nifty, "X" ,"close")

# compute return and log returns on closing price
nifty <- nifty %>%
  mutate(log_return = log(close/lag(close))) %>%
  mutate(return = (close - lag(close))/lag(close))

# aggregate at daily level to reduce data noise
nifty_daily <- nifty %>%
  group_by(Date) %>%
  summarise(
    open = first(open, na.rm = T),
    high = max(high, na.rm = T),
    low = min(low, na.rm = T),
    close = last(close, na.rm = T),
    volume = sum(volume, na.rm = T),
    .groups = 'drop'
  )

nifty_daily <- nifty_daily %>%
  mutate(log_return = log(close/lag(close))) 
plot(ts(nifty_daily$log_return))
