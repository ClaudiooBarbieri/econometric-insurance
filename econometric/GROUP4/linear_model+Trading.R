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
library(moments)
library(tsoutliers)
library(urca)
library(vars)
library(tseries)

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
par(mfrow=c(1,2))
acf(model1$residuals, main = "ACF")
pacf(model1$residuals, main = "PACF")
par(mfrow=c(1,1))
shapiro.test(model1$residuals)
jarque.test(model1$residuals)

par(mfrow=c(3,1))
plot(ts(train$target1))
plot(ts(model1$fitted.values), col = 'blue')
plot(ts(model1$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))


plot(ts(train$target1))
lines(ts(model1$fitted.values), col = 'blue')

prediction1 <- predict(model1, test)

plot(ts(test$target1), xlab = "Day", ylab = "Price")
lines(ts(prediction1), col = 'blue')

legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8)                          

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
par(mfrow=c(1,2))
acf(model3$residuals, main = "ACF")
pacf(model3$residuals, main = "PACF")
par(mfrow=c(1,1))
shapiro.test(model3$residuals)
jarque.test(model1$residuals)

par(mfrow=c(3,1))
plot(ts(train$target3))
plot(ts(model3$fitted.values), col = 'blue')
plot(ts(model3$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target3))
lines(ts(model3$fitted.values), col = 'blue')

prediction3 <- predict(model3, test)

plot(ts(test$target3), xlab = "Day", ylab = "Price")
lines(ts(prediction3), col = 'blue')

legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8)   

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
par(mfrow=c(1,2))
acf(model5$residuals, main = "ACF")
pacf(model5$residuals, main = "PACF")
par(mfrow=c(1,1))
shapiro.test(model5$residuals)
jarque.test(model5$residuals)

par(mfrow=c(3,1))
plot(ts(train$target5))
plot(ts(model5$fitted.values), col = 'blue')
plot(ts(model5$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target5))
lines(ts(model5$fitted.values), col = 'blue')

prediction5 <- predict(model5, test)

plot(ts(test$target5), xlab = "Day", ylab = "Price")
lines(ts(prediction5), col = 'blue')

legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8)   

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
par(mfrow=c(1,2))
acf(model7$residuals, main = "ACF")
pacf(model7$residuals, main = "PACF")
par(mfrow=c(1,1))
shapiro.test(model7$residuals)
jarque.test(model7$residuals)

par(mfrow=c(3,1))
plot(ts(train$target7))
plot(ts(model7$fitted.values), col = 'blue')
plot(ts(model7$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))

plot(ts(train$target7))
lines(ts(model7$fitted.values), col = 'blue')

prediction7 <- predict(model7, test)

plot(ts(test$target7), xlab = "Day", ylab = "Price")
lines(ts(prediction7), col = 'blue')

legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8)   

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

############## what if on log returns (log(close(t+1)/close(t))) #################################
target <- log(nifty_daily$close /  lag(nifty_daily$close))[-1]
lagged_technical_indicators_nifty <- technical_indicators(nifty_daily)
lagged_technical_indicators_nifty$return <- target
adf.test(target)
acf(target)
pacf(target)

nobs <- nrow(lagged_technical_indicators_nifty)
first2019 <- which(format(as.Date(lagged_technical_indicators_nifty$Date), "%Y") == 2019)[1]
train <- lagged_technical_indicators_nifty[1:(first2019-1),]
test <- lagged_technical_indicators_nifty[first2019:nobs,]

# model 
model <- lm(return ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train)
model_fw <- ols_step_forward_p(model)
model_bw <- ols_step_backward_p(model)
# Compare AIC and assign the better model
if (min(model_fw$metrics$aic) < min(model_bw$metrics$aic)) {
  model <- model_fw$model
} else {
  model <- model_bw$model
}
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
dwtest(model)
bptest(model)
par(mfrow=c(1,2))
acf(model$residuals, main = "ACF")
pacf(model$residuals, main = "PACF")
par(mfrow=c(1,1))
shapiro.test(model$residuals)
jarque.test(model$residuals)

# remove outliers
train[c(271,435,436),]$Date
train_wo <- train[-c(271,435,436),]
# and repeat
model <- lm(return ~ -1 + Close + macd + rsi + ulti + volatility + roc + dema + atr + cci + obv + wr, data = train_wo)
model_fw <- ols_step_forward_p(model)
model_bw <- ols_step_backward_p(model)
# Compare AIC and assign the better model
if (min(model_fw$metrics$aic) < min(model_bw$metrics$aic)) {
  model <- model_fw$model
} else {
  model <- model_bw$model
}
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
dwtest(model)
bptest(model)
par(mfrow=c(1,2))
acf(model$residuals, main = "ACF")
pacf(model$residuals, main = "PACF")
par(mfrow=c(1,1))
shapiro.test(model$residuals)
jarque.test(model$residuals)

par(mfrow=c(3,1))
plot(ts(train$return))
plot(ts(model$fitted.values), col = 'blue')
plot(ts(model$residuals), col = 'red')
abline(h=0)
par(mfrow=c(1,1))


plot(ts(train$return))
lines(ts(model$fitted.values), col = 'blue')

prediction <- predict(model, test)

plot(ts(test$return), xlab = "Day", ylab = "log return", main = "Test: Actual vs Predicted")
lines(ts(prediction), col = 'red')


legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "red"),           
       lty = 1,                            
       cex = 0.8)                          

true_prices <- test$return
predicted_prices <- prediction

# Calculate directional changes
true_changes <- sign(diff(true_prices))  # +1 for up, -1 for down, 0 for no change
pred_changes <- sign(diff(predicted_prices))

# Directional accuracy (excluding cases with no change)
direction_correct <- true_changes == pred_changes
directional_accuracy <- mean(direction_correct[true_changes != 0], na.rm = TRUE)

print(paste("Directional Accuracy:", round(directional_accuracy*100, 2), "%"))
print(paste("RMSE:", RMSE(predicted_prices, true_prices)))


#### what if on log(close/open) ####

# Target: log(close/open)
return_co <- log(nifty_daily$close / nifty_daily$open)

# Build features + new target
lagged_tech_co <- technical_indicators(nifty_daily)
lagged_tech_co$return_co <- return_co[-1]

# Train/test split at start of 2019
n_obs      <- nrow(lagged_tech_co)
start2019 <- which(format(as.Date(lagged_tech_co$Date), "%Y") == "2019")[1]
train_co  <- lagged_tech_co[1:(start2019 - 1), ]
test_co   <- lagged_tech_co[start2019:n_obs, ]

# Stationarity + ACF/PACF on target (OK!)
adf.test(train_co$return_co)
Acf(train_co$return_co, main = "ACF of return_co")
Pacf(train_co$return_co, main = "PACF of return_co")

# Outliers
# train_co[c(271,435,436),]$Date
# train_co <- train[-c(271,435,436),]

# Fit initial model (no intercept), then stepwise
mod_co <- lm(return_co ~ -1 +
               Close + macd + rsi + ulti + volatility +
               roc + dema + atr + cci + obv + wr,
             data = train_co)

fw_co <- ols_step_forward_p(mod_co)
bw_co <- ols_step_backward_p(mod_co)
mod_co <- if (min(fw_co$metrics$aic) < min(bw_co$metrics$aic)) fw_co$model else bw_co$model

# Diagnostic tests on selected model
summary(mod_co)
par(mfrow = c(2, 2)); plot(mod_co); par(mfrow = c(1, 1))
dwtest(mod_co)      # Durbin–Watson
bptest(mod_co)      # Breusch–Pagan
par(mfrow = c(1, 2))
Acf(residuals(mod_co), main = "ACF Residuals")
Pacf(residuals(mod_co), main = "PACF Residuals")
par(mfrow = c(1, 1))
shapiro.test(residuals(mod_co))
JarqueTest(residuals(mod_co))  # from DescTools

# Residual vs fitted plots
par(mfrow = c(3, 1))
plot(ts(train_co$return_co),          main = "Train return_co")
plot(ts(fitted(mod_co)), col = 'blue',main = "Fitted values")
plot(ts(residuals(mod_co)), col = 'red', main = "Residuals")
abline(h = 0)
par(mfrow = c(1, 1))

plot(ts(train_co$return_co), main = "Actual vs Fitted")
lines(ts(fitted(mod_co)), col = 'blue')
legend("topleft", legend = c("Actual", "Fitted"),
       col = c("black", "blue"), lty = 1)

# Predict on test + plot
pred_co   <- predict(mod_co, newdata = test_co)
actual_co <- test_co$return_co

plot(ts(actual_co), xlab = "Day", ylab = "Log(C→O) Return",
     main = "Test: Actual vs Predicted")
lines(ts(pred_co), col = 'red')
legend("topleft", legend = c("Actual", "Predicted"),
       col = c("black", "red"), lty = 1)

# Directional accuracy and RMSE
true_dir_co <- sign(diff(actual_co))
pred_dir_co <- sign(diff(pred_co))
dir_acc_co  <- mean(true_dir_co == pred_dir_co & true_dir_co != 0, na.rm = TRUE)
rmse_co     <- RMSE(pred_co, actual_co)

cat(sprintf("Directional Accuracy (C→O): %.2f%%\n", dir_acc_co * 100))
cat(sprintf("RMSE (C→O): %.6f\n", rmse_co))

# ------------------------------------------------------------------------------
# Trading strategy

# Metrics 
strategy_metrics <- function(returns, periods_per_year = 252, risk_free_rate = 0.0) {
  r <- returns
  T <- length(r)
  
  # Annualized Return 
  cum_growth <- prod(1 + r)
  ann_return <- cum_growth^(periods_per_year / T) - 1
  
  # Annualized Sharpe Ratio
  excess <- r - risk_free_rate
  sharpe <- (mean(excess) / sd(excess)) * sqrt(periods_per_year)
  
  # Maximum Drawdown
  wealth_index <- cumprod(1 + r)
  peak         <- cummax(wealth_index)
  drawdowns    <- (peak - wealth_index) / peak
  max_dd       <- max(drawdowns, na.rm = TRUE)
  
  # Sortino Ratio
  neg_ret     <- r[r < 0]
  downside_sd <- if (length(neg_ret) > 0) sd(neg_ret) else NA
  sortino     <- (mean(excess) / downside_sd) * sqrt(periods_per_year)
  
  return(list(
    Annualized_Return = ann_return,
    Sharpe_Ratio      = sharpe,
    Max_Drawdown      = max_dd,
    Sortino_Ratio     = sortino
  ))
}

# function for comparison plotting

plot_fun <- function(arr1, label1, arr2, label2, labelx, labely, title,
                     ylims = c(0.7, 1.3)) {

  
  plot_df <- data.frame(
    Time    = seq_along(arr1),
    Series1 = arr1,
    Series2 = arr2
  )
  names(plot_df)[2:3] <- c(label1, label2)
  
  plot_long <- pivot_longer(
    plot_df,
    cols      = c(label1, label2),
    names_to  = "Series",
    values_to = "Value"
  )
  
  ggplot(plot_long, aes(x = Time, y = Value, color = Series)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = setNames(c("blue", "orange"), c(label1, label2))
    ) +
    labs(
      title = title,
      x     = labelx,
      y     = labely,
      color = NULL
    ) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = ylims) +                      # <<< set Y‑axis limits
    theme_light(base_size = 14) +
    theme(
      plot.title           = element_text(hjust = 0.5, face = "bold"),
      legend.position      = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background    = element_rect(fill = alpha("white", 0.8), color = NA)
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray")  # baseline at 1
}

# Select the predicted and test
# log(close/open)
true_ret <- actual_co
pred_ret <- pred_co 

# log(close/close)
#true_ret <- true_prices
#pred_ret <- predicted_prices


plot_fun(true_ret , "actual returns", pred_ret , "predicted returns", "time", "returns", "Pred VS True returns")

trading_strategy <- function(pred_ret, true_ret, transaction_cost = 0.0005, threshold = 0.0001,
                             type = c("close_close", "close_open")) {
  # Calculate positions based on prediction and threshold
  positions <- ifelse(pred_ret > threshold, 1,
                      ifelse(pred_ret < -threshold, -1, 0))
  
  # Computation of costs
  n <- length(positions)
  costs <- numeric(n)  
  prev_pos_nz <- positions[1:(n-1)]!= 0
  curr_pos_nz <- positions[2:n]!= 0
  
  if (type == "close_close") {
    # Consider only when change position
    pos_change <- abs(diff(c(0, positions)))
    costs      <- transaction_cost * pos_change
  } else {
    # Consider all overnight costs
    prev_nz <- positions[1:(n-1)] != 0
    curr_nz <- positions[2:n]   != 0
    costs[2:n] <- transaction_cost * (as.integer(prev_nz) + as.integer(curr_nz))
    costs[1]   <- transaction_cost * as.integer(positions[1] != 0)
  }
  
  # Calculate strategy returns after costs
  strategy_return <- (positions * true_ret) - costs
  
  # Calculate cumulative returns
  cumulative_return <- cumsum(strategy_return)
  
  
  # Return data frame 
  data.frame(
    Position = positions,
    Strategy_Return = strategy_return,
    Cumulative_Return = cumulative_return,
    Costs = costs
  )
}

my_strategy <- trading_strategy(pred_ret, true_ret, 0.0005, 0.0001, "close_open")
#my_strategy <- trading_strategy(pred_ret, true_ret, 0.0005, 0.0001, "close_close")

# Plot Cumulative log-return
ggplot(my_strategy, aes(x = seq_along(Cumulative_Return), y = Cumulative_Return)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "PnL",
    x = "Time",
    y = "Cumulative Log Return"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

# Compare trading strategy vs index performance 
# Convert cumulative log returns to cumulative returns (in percentage)
cum_ret_exp <- exp(my_strategy$Cumulative_Return)

# Normalize strategy return to start at 1
cum_ret_norm <- cum_ret_exp / cum_ret_exp[1]

# Normalize NIFTY50 close price
close_price_norm  <- test_co$close / test_co$close[1]
plot_fun(cum_ret_norm, "Strategy", close_price_norm, "NIFTY50", "time", "cumulative returns", "Cumulative Returns: Strategy VS NIFTY50",
         ylims = c(0.6, 1.25))

# Convert net log‐returns to simple returns
simple_returns <- exp(my_strategy$Strategy_Return) - 1

metrics <- strategy_metrics(simple_returns,
                            periods_per_year = 252,
                            risk_free_rate   = 0.0)

cat(sprintf("Annualized Return: %6.2f%%\n", metrics$Annualized_Return * 100))
cat(sprintf("Sharpe Ratio:      %6.2f\n",    metrics$Sharpe_Ratio))
cat(sprintf("Max Drawdown:      %6.2f%%\n", metrics$Max_Drawdown * 100))
cat(sprintf("Sortino Ratio:     %6.2f\n",    metrics$Sortino_Ratio))



