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
adf.test(y$y4)
acf(y$y4, main = "ACF")
pacf(y$y4, main = "PACF")
par(mfrow=c(1,1))

# VAR on it, windows q, m ahead
q <- 90
m <- 7
n <- nrow(y)
all_true <- list()
all_pred <- list()
serial_pvals <- c()
arch_pvals <- c()
normality_pvals <- c()
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
  
  # Serial correlation test
  s_pval <- serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")$serial$p.value
  serial_pvals <- c(serial_pvals, s_pval)
  
  # ARCH test
  a_pval <- arch.test(var_model, lags.multi = 5, multivariate.only = TRUE)$arch.mul$p.value
  arch_pvals <- c(arch_pvals, a_pval)
  
  # Normality test
  n_pval <- normality.test(var_model)$jb.mul$JB$p.value[1,1]
  normality_pvals <- c(normality_pvals, n_pval)
  
  
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
# check assumption
print(paste("Autocorrelated residual:", sum(serial_pvals<0.05)))
print(paste("Heteroskedastick residual:", sum(arch_pvals<0.05)))
print(paste("NON normal residual:", sum(normality_pvals<0.05)))

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
                xmin = as.Date(Date) - 0.5, xmax = as.Date(Date) + 0.5,
                fill = close > open)) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red"), guide="none") +
  theme_minimal() +
  labs(title = "Nifty50 daily OHLC", y = "Price", x = "Date")
