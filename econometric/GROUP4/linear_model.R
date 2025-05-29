rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)

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


