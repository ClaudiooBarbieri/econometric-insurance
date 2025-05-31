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
library(tidyr)

# file with info
nifty <- read.csv("Nifty50.csv")

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
  df_tech <- df_hlc %>%
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
    dplyr::select(-High, -Low, -Close) %>%
    mutate(across(everything(), unname)) %>%
    `attr<-`("na.action", NULL)
 
}

nifty <- nifty %>%
  mutate(log_return = log(close/lag(close)))

lagged_technical_indicators <- technical_indicators(nifty)
lagged_technical_indicators$target <- nifty[-1,]$log_return

train <- lagged_technical_indicators[1:200000,]
test <- lagged_technical_indicators[200001:276374,]

model <- lm(target ~ -1 + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
dwstes(model)
acf(model$residuals)
pacf(model$residuals)
plot(ts(model$residuals))
prediction <- predict(model, test)
plot(ts(prediction-test$target))
lines(ts(test$target), col = 'red')
