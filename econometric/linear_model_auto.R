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

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# Create a list of all auto stocks data frames belonging to NIFTY50
auto_data <- list(
  NIFTY50 = list_indeces[["NIFTY50"]],
  NIFTYAUTO = list_indeces[["NIFTYAUTO"]],
  BAJAJAUTO = list_nifty50[["BAJAJ-AUTO"]],
  EICHERMOT = list_nifty50[["EICHERMOT"]],
  HEROMOTOCO = list_nifty50[["HEROMOTOCO"]],
  MM = list_nifty50[["M&M"]],
  MARUTI = list_nifty50[["MARUTI"]],
  TATAMOTORS = list_nifty50[["TATAMOTORS"]]
)

rm(list_indeces, list_nifty50, list_nifty100_mc)

name <- names(auto_data)

# competitor MARUTI vs MM vs TATAMOTRS
# BAJAJA vs EICHERMOT vs HEROMOTOCO

# select only 2018
start <- "2018-01-01"
end <- "2020-01-01"
select_period <- function(df, start, end){
  
  df$Datetime <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata")
  
  # Extract Date and Time
  df$Date <- as.Date(df$Datetime, tz="Asia/Kolkata")
  df$Time <- format(df$Datetime, format="%H:%M:%S")
  df$hour <- format(df$Datetime, format="%H")
  
  # Filter Only Market Hours (9:15 AM - 3:30 PM)
  df <- df %>%
    filter(Time >= "09:15:00" & Time <= "15:30:00") %>%
    filter(Date >= start & Date < end)
}

#auto_data_filtered <- lapply(auto_data, select_period, start = start, end = end)

selection <- select_period(auto_data[["NIFTY50"]],start,end)

group_by_day <- function(df){
  df <- df %>%
    group_by(Date) %>%
    summarise(
      open = first(open, na.rm = T),
      high = max(high, na.rm = T),
      low = min(low, na.rm = T),
      close = last(close, na.rm = T),
      volume = sum(volume, na.rm = T),
      .groups = 'drop'
    )
}

nifty50 <- group_by_day(selection)

# function to manipulate data and compute variables of interest
make_x <- function(df){
  
  # resample to five minutes
  # minute = 5
  # df <- df %>%
  #  (minute(timestamp) %% minute == 0)
  
  # dummy for time/period
  #df <- df %>%
   # mutate(NewDay = factor(ifelse(Time == "09:15:00", 1, 0))) %>%       # add dummy for new day
    #mutate(opening = factor(ifelse(Time >= "09:15:00" & Time <= "10:30:00" , 1, 0))) %>% # add dummy for opening market hours
    #mutate(middle = factor(ifelse(Time > "10:30:00" & Time <= "14:15:00" , 1, 0))) %>% # add dummy for middle market hours
    #mutate(closing = factor(ifelse(Time > "14:15:00" & Time <= "15:30:00" , 1, 0)))# add dummy for closing market hours
  
  # collapse all period dummy in one
  #df <- df %>%
  #  mutate(opening_middle_closing = factor(ifelse(df$Time >= "09:15:00" & df$Time <= "10:30:00" , 1, ifelse(df$Time > "10:30:00" & df$Time <= "14:15:00" , 2, 3))))
  
  # returns
  df <- df %>%
    mutate(log_return_1 = log(close) - log(lag(close))) %>%     # compute 1 log return
    mutate(log_return_5 = log(close) - log(lag(close, 5))) # compute 5 log return
  
  # returns on open
  df <- df %>%
    mutate(log_return_1_o = log(close) - log(open)) %>%     # compute 1min log return
    mutate(log_return_5_o = log(close) - log(lag(open, 5))) # compute 5min log return
  
  # trend following indicators
  df <- df %>%
    mutate(sma = SMA(lag(close), 10)) %>%                       # compute simple moving average 10min
    mutate(ema = EMA(lag(close), 10)) %>%                   # compute exponential moving average 10min
    mutate(macd_line = MACD(lag(close))[,1], macd_signal =  MACD(lag(close))[,2]) %>%  # compute moving average convergence divergence
    mutate(sar = SAR(cbind(
     high = lag(high),
     low = lag(low)))[,1])%>% # compute stop and reverse
    mutate(macd = factor(ifelse(macd_line>macd_signal,1,0))) #
  
  # momentum indicators
  df <- df %>% 
    mutate(rsi = RSI(lag(close))) %>% # compute relative strength index 
    mutate(williams = WPR(cbind(
      high = lag(high),
      low = lag(low),
      close = lag(close))))%>% # compute relative strength index 
    mutate(stoch = ST(cbind(
      high = lag(high),
      low = lag(low),
      close = lag(close)))) # compute William's %R
  
  # dummy for overbought (>70) or oversold (<30)
  df <- df %>%
    mutate(overbought = factor(ifelse(rsi > 70, 1, 0))) %>%       # add dummy for overbouught
    mutate(oversold = factor(ifelse(rsi < 30, 1, 0))) # add dummy for oversold
  
  # collapse overbought or oversold
  df <- df %>%
    mutate(over_bought_sold = factor(ifelse(rsi > 70, 1, ifelse(rsi < 30, 2, 3))))
  
  # volatility indicators
  df <- df %>% 
    mutate(atr = ATR(cbind( 
      high = lag(high),
      low = lag(low),
      close = lag(close)))[,2]) # compute average true range
    
  # volume based indicators
  df <- df %>% 
    mutate(obv = lag(OBV(close, volume))) %>% # compute on balance volume
    mutate(vwap = VWAP(lag(close), lag(volume))) %>% # compute volume weighted average price
    mutate(cmf = CMF(cbind( 
      high = lag(high),
      low = lag(low),
      close = lag(close)), lag(volume))) # compute Chiakin money flow
  
}

# apply computation of X
data <- make_x(nifty50)

# choose a data frame to use
df <- data
df <- df %>% dplyr::select(-c("obv","vwap","cmf"))
df <- na.omit(df) # remove NA

# plot values of log return to predict
y <- df$close
y_ts <- ts(y)
ts.plot(y_ts)


# fit model
model <- lm(log(close) ~ ema+rsi+williams+atr, data = df)
summary(model)
shapiro.test(model$residuals)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
df <- df[-c(155,305,389),]
model <- lm(log(close) ~ ema+rsi+williams+atr, data = df)
summary(model)
shapiro.test(model$residuals)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
df <- df[-c(154,158,387),]
model <- lm(log(close) ~ williams+ema+rsi, data = df)
summary(model)
shapiro.test(model$residuals)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# prediction / overfitting
library(caret)
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
cv_model <- train(log(close) ~ williams+ema+rsi, data = df, method = "lm", trControl = train_control)
print(cv_model)
