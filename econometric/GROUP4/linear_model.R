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
    mutate(across(everything(), unname)) %>%
    dplyr::select(-High, -Low) %>%
    `attr<-`("na.action", NULL)
 
}

# construct a dataset for supervised problem of predicting 1,3,5,7 day ahead price
lagged_technical_indicators_daily <- technical_indicators(nifty_daily)
target <- nifty_daily[-1,]$close
lagged_technical_indicators_daily$target1 <- target
lagged_technical_indicators_daily$target3 <- lead(target, 2)
lagged_technical_indicators_daily$target5 <- lead(target, 4)
lagged_technical_indicators_daily$target7 <- lead(target, 6)
lagged_technical_indicators_daily <- na.omit(lagged_technical_indicators_daily)

# split in train and test set 2/3 - 1/3
train <- lagged_technical_indicators_daily[1:468,]
test <- lagged_technical_indicators_daily[469:702,]

# model training 1-step
model1 <- lm(target1 ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + macd + rsi + ulti + roc + dema + atr + cci + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + macd + rsi + ulti + dema + atr + cci + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + macd + rsi + ulti + dema + atr + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + macd + ulti + dema + atr + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + macd + ulti + atr + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + ulti + atr + obv + wr, data = train)
summary(model1)
model1 <- lm(target1 ~ -1 + Close + ulti + obv + wr, data = train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
dwtest(model1)
bptest(model1)
acf(model1$residuals)
pacf(model1$residuals)
shapiro.test(model1$residuals)

par(mfrow=c(3,1))
plot(ts(train$target1))
plot(ts(model1$fitted.values), col = 'blue')
plot(ts(model1$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target1))
lines(ts(model1$fitted.values), col = 'blue')

prediction1 <- predict(model1, test)

plot(ts(test$target1))
lines(ts(prediction1), col = 'blue')

true_prices <- test$target1
predicted_prices <- prediction1

# Calculate directional changes
true_changes <- sign(diff(true_prices))  # +1 for up, -1 for down, 0 for no change
pred_changes <- sign(diff(predicted_prices))

# Directional accuracy (excluding cases with no change)
direction_correct <- true_changes == pred_changes
directional_accuracy <- mean(direction_correct[true_changes != 0], na.rm = TRUE)

print(paste("Directional Accuracy:", round(directional_accuracy*100, 2), "%"))
print(paste("RMSE:", RMSE(predicted_prices, true_prices)))

# model training 3-step
model3 <- lm(target3 ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model3)
model3 <- lm(target3 ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + obv + wr, data = train)
summary(model3)
model3 <- lm(target3 ~ -1 + Close + macd + rsi + ulti + roc + dema + atr + obv + wr, data = train)
summary(model3)
model3 <- lm(target3 ~ -1 + Close + macd + rsi + ulti + roc + dema + atr + wr, data = train)
summary(model3)
model3 <- lm(target3 ~ -1 + Close + macd + rsi + ulti + roc + dema + atr, data = train)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
par(mfrow=c(1,1))
dwtest(model3)
bptest(model3)
acf(model3$residuals)
pacf(model3$residuals)
shapiro.test(model3$residuals)

par(mfrow=c(3,1))
plot(ts(train$target3))
plot(ts(model3$fitted.values), col = 'blue')
plot(ts(model3$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target3))
lines(ts(model3$fitted.values), col = 'blue')

prediction3 <- predict(model3, test)

plot(ts(test$target3))
lines(ts(prediction3), col = 'blue')

true_prices <- test$target3
predicted_prices <- prediction3

# Calculate directional changes
true_changes <- sign(diff(true_prices))  # +1 for up, -1 for down, 0 for no change
pred_changes <- sign(diff(predicted_prices))

# Directional accuracy (excluding cases with no change)
direction_correct <- true_changes == pred_changes
directional_accuracy <- mean(direction_correct[true_changes != 0], na.rm = TRUE)

print(paste("Directional Accuracy:", round(directional_accuracy*100, 2), "%"))
print(paste("RMSE:", RMSE(predicted_prices, true_prices)))

# model training 5-step
model5 <- lm(target5 ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model5)
model5 <- lm(target5 ~ -1 + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model5)
model5 <- lm(target5 ~ -1 + macd + rsi + ulti + volatility + roc + dema + atr + obv + wr, data = train)
summary(model5)
model5 <- lm(target5 ~ -1 + macd + rsi + ulti + volatility + roc + dema + atr + obv, data = train)
summary(model5)
model5 <- lm(target5 ~ -1 + macd + rsi + volatility + roc + dema + atr + obv, data = train)
summary(model5)
model5 <- lm(target5 ~ -1 + macd + rsi + roc + dema + atr + obv, data = train)
summary(model5)
par(mfrow=c(2,2))
plot(model5)
par(mfrow=c(1,1))
dwtest(model5)
bptest(model5)
acf(model5$residuals)
pacf(model5$residuals)
shapiro.test(model5$residuals)

par(mfrow=c(3,1))
plot(ts(train$target5))
plot(ts(model5$fitted.values), col = 'blue')
plot(ts(model5$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target5))
lines(ts(model5$fitted.values), col = 'blue')

prediction5 <- predict(model5, test)

plot(ts(test$target5))
lines(ts(prediction5), col = 'blue')

true_prices <- test$target5
predicted_prices <- prediction5

# Calculate directional changes
true_changes <- sign(diff(true_prices))  # +1 for up, -1 for down, 0 for no change
pred_changes <- sign(diff(predicted_prices))

# Directional accuracy (excluding cases with no change)
direction_correct <- true_changes == pred_changes
directional_accuracy <- mean(direction_correct[true_changes != 0], na.rm = TRUE)

print(paste("Directional Accuracy:", round(directional_accuracy*100, 2), "%"))
print(paste("RMSE:", RMSE(predicted_prices, true_prices)))

# model training 7-step
model7 <- lm(target7 ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model7)
model7 <- lm(target7 ~ -1 + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
summary(model7)
model7 <- lm(target7 ~ -1 + macd + ulti + volatility + roc + dema + atr + +cci + obv + wr, data = train)
summary(model7)
model7 <- lm(target7 ~ -1 + macd + volatility + roc + dema + atr + cci + obv, data = train)
summary(model7)
par(mfrow=c(2,2))
plot(model7)
par(mfrow=c(1,1))
dwtest(model7)
bptest(model7)
acf(model7$residuals)
pacf(model7$residuals)
shapiro.test(model7$residuals)

par(mfrow=c(3,1))
plot(ts(train$target7))
plot(ts(model7$fitted.values), col = 'blue')
plot(ts(model7$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target7))
lines(ts(model7$fitted.values), col = 'blue')

prediction7 <- predict(model7, test)

plot(ts(test$target7))
lines(ts(prediction7), col = 'blue')

true_prices <- test$target7
predicted_prices <- prediction7

# Calculate directional changes
true_changes <- sign(diff(true_prices))  # +1 for up, -1 for down, 0 for no change
pred_changes <- sign(diff(predicted_prices))

# Directional accuracy (excluding cases with no change)
direction_correct <- true_changes == pred_changes
directional_accuracy <- mean(direction_correct[true_changes != 0], na.rm = TRUE)

print(paste("Directional Accuracy:", round(directional_accuracy*100, 2), "%"))
print(paste("RMSE:", RMSE(predicted_prices, true_prices)))