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
library(urca)
library(vars)

# read data
nifty <- read.csv("Nifty50_daily.csv")

nifty <- nifty %>%
  dplyr::select(-X)

# need to transform to satisfy the constraints

transform_data <- function(open, high, low, close){
  set.seed(42)
  
  # Small epsilon and small random vector to deal with invalid
  eps <- 1e-6
  rand_eps1 <- runif(length(open), 0.001, 0.003)
  rand_eps2 <- runif(length(close), 0.001, 0.003)
  
  # lambdao
  lambdao <- ifelse(
    abs(open - low) < eps,
    rand_eps1,
    ifelse(
      abs(open - high) < eps,
      (open - rand_eps1 - low) / (high - low),
      (open - low) / (high - low)
    )
  )
  
  # lambdac
  lambdac <- ifelse(
    abs(close - low) < eps,
    rand_eps2,
    ifelse(
      abs(close - high) < eps,
      (close - rand_eps2 - low) / (high - low),
      (close - low) / (high - low)
    )
  )
  
  
  
  # unconstrained variable data
  y1 <- log( low)
  y2 <- log( high -  low)
  y3 <- log(lambdao / (1- lambdao))
  y4 <- log(lambdac / (1- lambdac))
  
  y <- data.frame(cbind(y1, y2, y3, y4))
  
}

y <- transform_data(nifty$open, nifty$high, nifty$low, nifty$close)

# function to go back to raw OHLC
backtransform <- function(y1, y2, y3, y4){
  
  lambdao <- exp( y3) / (1 + exp( y3))
  lambdac <- exp( y4) / (1 + exp( y4))
  
  open <- (lambdao * (exp( y1) + exp( y2))) + ((1 - lambdao) * exp( y1))
  high <- exp( y1) + exp( y2)
  low <- exp( y1)
  close <- (lambdac * (exp( y1) + exp( y2))) + ((1 - lambdac) * exp( y1))
  
  ohlc <- data.frame(cbind(open, high, low, close))
  
}

# X <- backtransform(y$y1, y$y2, y$y3, y$y4) used to test implementation

# test stationarity on the transformed data
adf.test(y$y1)
par(mfrow=c(1,2))
acf(y$y1, main = "ACF")
pacf(y$y1, main = "PACF")
adf.test(y$y2)
acf(y$y2, main = "ACF")
pacf(y$y2, main = "PACF")
adf.test(y$y3)
acf(y$y3, main = "ACF")
pacf(y$y3, main = "PACF")
adf.test(yq$y4)
acf(y$y4, main = "ACF")
pacf(y$y4, main = "PACF")
par(mfrow=c(1,1))

# VAR on it, windows q, m ahead
q <- 90
m <- 1 #3
n <- nrow(y)
all_true <- list()
all_pred <- list()
if(m == 1){
  predictions <- data.frame(
    open = numeric(n - q - m + 1),
    high = numeric(n - q - m + 1),
    low = numeric(n - q - m + 1),
    close = numeric(n - q - m + 1)
  )
} else {
  predictions_multi <- vector("list", length = n - q - m + 1)
}

for (i in seq(q, nrow(y) - m)) {
  y_train <- y[(i - q + 1):i, , drop = FALSE]
  
  # Apply ADF test to each column and stop if p-value > 0.05
  cols_to_difference <- c()
  for (colname in colnames(y_train)) {
    pval <- adf.test(y_train[[colname]])$p.value
    if (pval > 0.05) {
      warning(paste("ADF test failed: variable", colname, 
                 "is non-stationary at iteration", i, 
                 "with p-value =", round(pval, 4)))
      cols_to_difference <- c(cols_to_difference, colname)
    }
  }
  
  # differenciate when needed
  if (length(cols_to_difference) > 0) {
    y_train <- y_train %>%
      mutate(across(all_of(cols_to_difference), ~ . - lag(.))) %>%
      na.omit()
  }
  
  # Step 1: Select optimal lag
  lag_selection <- VARselect(y_train, lag.max = 10, type = "const")
  p <- lag_selection$selection["AIC(n)"]
  
  var_model <- VAR(y_train, p = p, type = "const")
  forecast <- predict(var_model, n.ahead = m)
  
  # Initialize a data frame to hold transformed forecasts
  forecast_df <- data.frame(matrix(NA, nrow = m, ncol = ncol(y)))
  colnames(forecast_df) <- colnames(y)
  
  for (col in colnames(y)) {
    if (col %in% cols_to_difference) {
      # Apply inverse differencing
      forecast_df[[col]] <- cumsum(forecast$fcst[[col]][,"fcst"]) + y[[col]][i]
    } else {
      # No differencing applied — keep forecast as-is
      forecast_df[[col]] <- forecast$fcst[[col]][,"fcst"]
    }
  }
  
  X_pred <- backtransform(forecast_df$y1, forecast_df$y2, forecast_df$y3, forecast_df$y4)
  
  if(m == 1){
    predictions[i-q+1, "open"] <- X_pred$open
    predictions[i-q+1, "high"] <- X_pred$high
    predictions[i-q+1, "low"] <- X_pred$low
    predictions[i-q+1, "close"] <- X_pred$close
  } else {
    predictions_multi[[i - q + 1]] <- X_pred
  }
  
  # Store predictions and true values
  all_true[[length(all_true) + 1]] <- nifty[(i + 1):(i + m), ] %>%
    dplyr::select(open, high, low, close)
  all_pred[[length(all_pred) + 1]] <- X_pred
}

true_df <- do.call(rbind, all_true)
pred_df <- do.call(rbind, all_pred)
print(paste("RMSE  open:", RMSE(true_df$open, pred_df$open)))
print(paste("RMSE  high:", RMSE(true_df$high, pred_df$high)))
print(paste("RMSE  low:", RMSE(true_df$low, pred_df$low)))
print(paste("RMSE  close:", RMSE(true_df$close, pred_df$close)))

# for one step ahead can also plot
actual <- nifty[(q+1):nrow(nifty),]

par(mfrow=c(2,2))
plot(ts(actual$open), xlab = "Day", ylab = "Price", main = "Open")
lines(ts(predictions$open), col = 'blue')
legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8) 
plot(ts(actual$high), xlab = "Day", ylab = "Price", main = "High")
lines(ts(predictions$high), col = 'blue')
legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8) 
plot(ts(actual$low), xlab = "Day", ylab = "Price", main = "Low")
lines(ts(predictions$low), col = 'blue')
legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8) 
plot(ts(actual$close), xlab = "Day", ylab = "Price", main = "Close")
lines(ts(predictions$close), col = 'blue')
legend("topleft",                   
       legend = c("Actual", "Predicted"),  
       col = c("black", "blue"),           
       lty = 1,                            
       cex = 0.8) 
par(mfrow=c(1,1))

# compute the error
print(paste("RMSE  open:", RMSE(actual$open, predictions$open)))
print(paste("RMSE  high:", RMSE(actual$high, predictions$high)))
print(paste("RMSE  low:", RMSE(actual$low, predictions$low)))
print(paste("RMSE  close:", RMSE(actual$close, predictions$close)))

# candlestick plot
ggplot(nifty, aes(x = as.Date(Date))) +
  geom_segment(aes(y = low, yend = high, xend = as.Date(Date)), color = "black") +
  geom_rect(aes(ymin = pmin(open, close), ymax = pmax(open, close),
                xmin = as.Date(Date) - 0.3, xmax = as.Date(Date) + 0.3,
                fill = close > open)) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red")) +
  theme_minimal() +
  labs(title = "OHLC Chart", y = "Price", x = "Date")


#-------------------------------------------------------------------------------
# Trading Strategy

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

#my_strategy <- trading_strategy(pred_ret_2019, true_ret_2019, 0.0005, 0.0001, "close_open")
my_strategy <- trading_strategy(pred_ret_2019, true_ret_2019, 0.0005, 0.0001, "close_close")

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
close_price_norm  <- nifty$close[(q+1):nrow(nifty)][idx_2019] / nifty$close[(q+1):nrow(nifty)][idx_2019][1]

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

