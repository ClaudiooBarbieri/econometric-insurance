rm(list = ls())

library(forecast)
library(tseries)
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
library(tidyr)

# file with info, now consider data (daily) for which we have SPY also
nifty <- read.csv("nifty_lagged_indicator.csv")

# compute log return
nifty <- nifty %>%
  mutate(log_return = log(close/lag(close))) 

# plot the log return
plot(ts(nifty_daily$log_return))

# check for stationarity
adf.test(nifty_daily$log_return)

# exami the correlation function
par(mfrow=c(2,1))
acf(nifty_daily$log_return)
pacf(nifty_daily$log_return)
par(mfrow=c(1,1))


