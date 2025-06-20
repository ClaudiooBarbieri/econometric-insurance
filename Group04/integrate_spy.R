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

#APIKEY 6NIRJSA6V2A7SNKM

# read SPY
spy <- read.csv("daily_SPY.csv")

# consider the time span used for nifty
spy <- spy %>%
  mutate(Date = as.Date(timestamp))
spy_daily <- spy %>%
  filter(Date >= "2017-01-01" & Date <= "2020-01-01") %>%
  map_df(rev)

# save as csv
write.csv(spy_daily, file= "daily_SPY_filtered.csv")

# function to computed the lagged technical indicators
technical_indicators <- function(df){
  
  # fill NAs by keep constant value
  df <- df %>% 
    fill(c(open,high,low,close), .direction = "down")
  
  # fill for 0 in volume
  df <- df %>% 
    mutate(across(volume, ~ifelse(. == 0, NA, .))) %>% 
    fill(everything(), .direction = "down")
  
  # build the lagged (O)HLC object
  df_ohlcv <- df %>%
    mutate(Open = lag(open), High = lag(high), Low = lag(low), Close = lag(close), Volume = lag(volume)) %>%
    na.omit()
  
  df_ohlc <- df_ohlcv %>%
    dplyr::select(Open, High, Low, Close)
  
  df_hlc <- df_ohlc %>%
    dplyr::select(High, Low, Close)
  # compute indicators
  df_tech <- df_ohlcv %>%
    mutate(macd = MACD(df_hlc$Close)[,1]) %>%
    mutate(rsi = RSI(df_hlc$Close)) %>%
    mutate(ulti = ultimateOscillator(df_hlc)) %>%
    mutate(volatility = volatility(df_ohlc)) %>%
    mutate(roc = ROC(df_hlc$Close)) %>%
    mutate(dema = DEMA(df_hlc$Close)) %>%
    mutate(atr = ATR(df_hlc)[,1]) %>%
    mutate(cci = CCI(df_hlc)) %>%
    mutate(obv = OBV(df_hlc$Close, df_ohlcv$Volume)) %>%
    mutate(wr = WPR(df_hlc)) %>%
    mutate(across(everything(), unname)) %>%
    `attr<-`("na.action", NULL)
  
}

# compute technical indicators
spy_technical <- technical_indicators(spy_daily)

# save as csv
write.csv(spy_technical, file= "daily_SPY_filtered_lag_tech_ind.csv")

# if you want to have consistency with both time series days
nifty_technical <- read.csv("nifty_lagged_indicator.csv")

# consider only market matching days
common_dates <- as.Date(intersect(as.Date(spy_technical$Date), as.Date(nifty_technical$Date)))

spy_common <- spy_technical %>% filter(Date %in% common_dates)
nifty_common <- nifty_technical %>% filter(Date %in% common_dates)

write.csv(spy_common, file= "common_daily_SPY_filtered_lag_tech_ind.csv")
write.csv(nifty_common, file= "common_daily_nifty_filtered_lag_tech_ind.csv")


