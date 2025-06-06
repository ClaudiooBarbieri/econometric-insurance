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
library(olsrr)

# files with info
nifty_daily <- read.csv("Nifty50_daily.csv")

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

# construct a dataset with the technical indicator
lagged_technical_indicators_nifty <- technical_indicators(nifty_daily)

# save for future possible uses
write.csv(lagged_technical_indicators_nifty, file= "nifty_lagged_indicator.csv")

# data for the supervised problem of predicting 1,3,5,7 day ahead price
target <- nifty_daily[-1,]$close
lagged_technical_indicators_nifty$target1 <- target
lagged_technical_indicators_nifty$target3 <- lead(target, 2)
lagged_technical_indicators_nifty$target5 <- lead(target, 4)
lagged_technical_indicators_nifty$target7 <- lead(target, 6)
lagged_technical_indicators_nifty <- na.omit(lagged_technical_indicators_nifty)

# split in train and test set 2/3 - 1/3
# nobs <- nrow(lagged_technical_indicators_nifty)
# stop <- floor(2*nobs/3)
# train <- lagged_technical_indicators_nifty[1:stop,]
# test <- lagged_technical_indicators_nifty[(stop+1):nobs,]

nobs <- nrow(lagged_technical_indicators_nifty)
first2019 <- which(format(as.Date(lagged_technical_indicators_nifty$Date), "%Y") == 2019)[1]
train <- lagged_technical_indicators_nifty[1:(first2019-1),]
test <- lagged_technical_indicators_nifty[first2019:nobs,]

# model training 1-step
model1 <- lm(target1 ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
model1_fw <- ols_step_forward_p(model1)
model1_bw <- ols_step_backward_p(model1)
# Compare AIC and assign the better model
if (min(model1_fw$metrics$aic) < min(model1_bw$metrics$aic)) {
  model1 <- model1_fw$model
} else {
  model1 <- model1_bw$model
}
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
model3_fw <- ols_step_forward_p(model3)
model3_bw <- ols_step_backward_p(model3)
# Compare AIC and assign the better model
if (min(model3_fw$metrics$aic) < min(model3_bw$metrics$aic)) {
  model3 <- model3_fw$model
} else {
  model3 <- model3_bw$model
}
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
model5_fw <- ols_step_forward_p(model5)
model5_bw <- ols_step_backward_p(model5)
# Compare AIC and assign the better model
if (min(model5_fw$metrics$aic) < min(model5_bw$metrics$aic)) {
  model5 <- model5_fw$model
} else {
  model5 <- model5_bw$model
}
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
model7_fw <- ols_step_forward_p(model7)
model7_bw <- ols_step_backward_p(model7)
# Compare AIC and assign the better model
if (min(model7_fw$metrics$aic) < min(model7_bw$metrics$aic)) {
  model7 <- model7_fw$model
} else {
  model7 <- model7_bw$model
}
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

