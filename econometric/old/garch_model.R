rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tseries)
library(FinTS)
library(rugarch)
library(urca)
library(tibble)
library(strucchange)


# Load the data
load("lists_of_datasets.RData") 

# Starting date 
starting_date <- "2020-12-30"

# Filter dataset
filter_by_timestamp <- function(df) {
  df %>% 
    filter(timestamp >= starting_date) %>%
    select(timestamp, close)
}

# Select NIFTY50 data
nifty50_data <- filter_by_timestamp(list_indeces[["NIFTY50"]])


# Compute log returns from the closing price
nifty50_data <- nifty50_data %>%
  mutate(log_ret = log(close / lag(close))) %>%
  drop_na() 

# Split in train and test data
train_start <- "2020-01-01"
train_end <- max(nifty50_data$timestamp[nifty50_data$timestamp < "2021-01-01"])
test_start <- min(nifty50_data$timestamp[nifty50_data$timestamp >= "2021-01-01"])

train_data <- nifty50_data %>%
  filter(timestamp <= train_end)

test_data <- nifty50_data %>%
  filter(timestamp >= test_start)

y <- train_data$log_ret


##### Perform some tests on the time series #####

### Structural breaks ###
# Structural break test in the mean
fs.y <- Fstats(y ~ 1)
plot(fs.y, main = "F-Statistics for Structural Break in Mean (log_ret)")
struct_test <- sctest(fs.y)
print(struct_test)
# No structural breaks in mean

# Structural breaks in trend 
train_data$trend <- seq_along(train_data$log_ret)

fs.y.2 <- Fstats(log_ret ~ trend, data = train_data)
plot(fs.y.2, main = "F-Statistics for Structural Break in Trend (log_ret ~ trend)")
sctest(fs.y.2)
# No structural breaks in trend

### Autocorrelation and Partial Autocorrelation ###

acf(y, lag.max = 12, main = "ACF of log_ret")
acf(y, lag.max = 12, type = "partial", main = "Partial ACF of log_ret")

# ACF is geometrically decaying,PACF is not significant 

### Stationarity tests ###

# Assuming no constant, no trend
ur_none <- ur.df(y, type = "none", lags = 10)
cat("ADF Test with no constant and no trend:\n")
summary(ur_none)

# Assuming with constnat drift only
ur_drift <- ur.df(y, type = "drift", lags = 9)
cat("\nADF Test with constant (drift) and no trend:\n")
summary(ur_drift)

# Assuming with both constant and trend
ur_trend <- ur.df(y, type = "trend", lags = 9)
cat("\nADF Test with constant and trend:\n")
summary(ur_trend)

# Time series is stationary

##### GARCH(p,q) model and generalizations #####


models <- c("sGARCH", "eGARCH", "gjrGARCH")

# Model order selection 
max_p <- 3
max_q <- 3

# Store the results here
results <- tibble(model = character(), p = integer(), q = integer(), AIC = double(), BIC = double())

# Store best fitted models
best_fits <- list()

for (model_type in models) {
  for (p in 0:max_p) {
    for (q in 0:max_q) {
      
      # Skip the case where both p and q are zero
      if (p == 0 & q == 0) next
      
      # Define model specification
      spec <- ugarchspec(
        variance.model = list(model = model_type, garchOrder = c(p, q)),
        mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      
      # Try to fit the model. If it fails, catch the error and continue.
      fit <- tryCatch(
        ugarchfit(spec, data = y, solver = "hybrid"),
        error = function(e) return(NULL),
        warning = function(w) return(NULL)
      )
      
      if (is.null(fit)) {
        cat("Model", model_type, "(p=", p, ", q=", q, ") failed to converge\n", sep = " ")
        next
      }
      
      current_AIC <- infocriteria(fit)["Akaike", ]
      current_BIC <- infocriteria(fit)["Bayes", ]
      
      # Ensure values are not NaN before storing
      if (!is.na(current_AIC) & !is.na(current_BIC)) {
        results <- results %>%
          add_row(model = model_type, p = p, q = q, AIC = current_AIC, BIC = current_BIC)
        
        # Store the fitted model
        best_fits[[paste0(model_type, "_", p, "_", q)]] <- fit
        
        cat("Fitted model", model_type, "(p=", p, ", q=", q, ") - AIC:", current_AIC, " BIC:", current_BIC, "\n", sep = " ")
      }
    }
  }
}

# Print all results
print(results)

if (nrow(results) > 0) {
  
  # Find the best model for each type based on BIC
  best_models <- results %>%
    group_by(model) %>%
    filter(BIC == min(BIC)) %>%
    ungroup()
  
  cat("\nBest models based on BIC for each model type:\n")
  print(best_models)
  
  # Extract best parameters
  garch_best_params <- best_models %>% filter(model == "sGARCH")
  egarch_best_params <- best_models %>% filter(model == "eGARCH")
  gjrgarch_best_params <- best_models %>% filter(model == "gjrGARCH")
  
  # Retrieve best models
  garch_model <- best_fits[[paste0("sGARCH_", garch_best_params$p, "_", garch_best_params$q)]]
  egarch_model <- best_fits[[paste0("eGARCH_", egarch_best_params$p, "_", egarch_best_params$q)]]
  gjrgarch_model <- best_fits[[paste0("gjrGARCH_", gjrgarch_best_params$p, "_", gjrgarch_best_params$q)]]
  
  cat("\nSelected best models:\n")
  print(garch_model)
  print(egarch_model)
  print(gjrgarch_model)
}

### Forecasting ### (DEVO ANCORA FARLO FUNZIONARE)

# We'll adopt a rolling approach, with a 1-step ahead aproach

n_ahead <- 10
n_test <- nrow(test_data) 

return_forecast_sGARCH <- numeric(n_test)
return_forecast_eGARCH <- numeric(n_test)
return_forecast_gjr <- numeric(n_test)

sigma_sGARCH <- numeric(n_test)
sigma_eGARCH <- numeric(n_test)
sigma_gjr <- numeric(n_test)

realized_return <- test_data$log_ret  # Actual returns for comparison
realized_var <- realized_return^2  # Realized variance for volatility comparison

# Rolling-window forecasting
for (i in 1:n_test:n_ahead) {
  
  # Update training set dynamically
  current_train <- nifty50_data %>%
    filter(timestamp <= test_data$timestamp[i])
  
  y_train <- current_train$log_ret
  
  # Re-fit models
  fit_sGARCH <- ugarchfit(
    spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(garch_best_params$p, garch_best_params$q)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "norm"),
    data = y_train, solver = "hybrid"
  )
  
  fit_eGARCH <- ugarchfit(
    spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(egarch_best_params$p, egarch_best_params$q)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "norm"),
    data = y_train, solver = "hybrid"
  )
  
  fit_gjr <- ugarchfit(
    spec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(gjrgarch_best_params$p, gjrgarch_best_params$q)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "norm"),
    data = y_train, solver = "hybrid"
  )
  
  # Forecast 1 step ahead
  fore_sGARCH <- ugarchforecast(fit_sGARCH, n.ahead = n_ahead)
  fore_eGARCH <- ugarchforecast(fit_eGARCH, n.ahead = n_ahead)
  fore_gjr <- ugarchforecast(fit_gjr, n.ahead = n_ahead)
  
  # Store return forecasts (mean forecast)
  return_forecast_sGARCH[i] <- fitted(fore_sGARCH)[n_ahead]
  return_forecast_eGARCH[i] <- fitted(fore_eGARCH)[n_ahead]
  return_forecast_gjr[i] <- fitted(fore_gjr)[n_ahead]
  
  # Store forecasted conditional standard deviations (volatility forecasts)
  sigma_sGARCH[i] <- sigma(fore_sGARCH)[n_ahead]
  sigma_eGARCH[i] <- sigma(fore_eGARCH)[n_ahead]
  sigma_gjr[i] <- sigma(fore_gjr)[n_ahead]
  
  # Realized variance (squared return) for evaluation
  realized_var[i] <- test_data$log_ret[i]^2
}


# Returns Forecast Performance
mse_sGARCH_return <- mean((return_forecast_sGARCH - realized_return)^2)
mse_eGARCH_return <- mean((return_forecast_eGARCH - realized_return)^2)
mse_gjr_return <- mean((return_forecast_gjr - realized_return)^2)

# Volatility Forecast Performance
mse_sGARCH_vol <- mean((sigma_sGARCH^2 - realized_var)^2)
mse_eGARCH_vol <- mean((sigma_eGARCH^2 - realized_var)^2)
mse_gjr_vol <- mean((sigma_gjr^2 - realized_var)^2)

# Print Results
results_fore <- tibble(
  Model = c("sGARCH", "eGARCH", "GJR-GARCH"),
  MSE_Returns = c(mse_sGARCH_return, mse_eGARCH_return, mse_gjr_return),
  MSE_Volatility = c(mse_sGARCH_vol, mse_eGARCH_vol, mse_gjr_vol)
)
print(results_fore)

### Plot Forecasted rETURNS vs. Realized Returns ###

df_returns <- tibble(
  Time = test_data$timestamp,
  RealizedReturn = realized_return,
  sGARCH = return_forecast_sGARCH,
  eGARCH = return_forecast_eGARCH,
  GJR = return_forecast_gjr
)

df_melt_returns <- melt(df_returns, id.vars = "Time", variable.name = "Model", value.name = "Return")

ggplot(df_melt_returns, aes(x = Time, y = Return, color = Model)) +
  geom_line() +
  labs(title = "Rolling Forecasted Returns vs Realized Returns",
       y = "Log Returns")

### Plot Forecasted vARIANCE vs. Realized Variance ###

df_vol <- tibble(
  Time = test_data$timestamp,
  RealizedVar = realized_var,
  sGARCH = sigma_sGARCH^2,
  eGARCH = sigma_eGARCH^2,
  GJR = sigma_gjr^2
)

df_melt_vol <- melt(df_vol, id.vars = "Time", variable.name = "Model", value.name = "Variance")

ggplot(df_melt_vol, aes(x = Time, y = Variance, color = Model)) +
  geom_line() +
  labs(title = "Rolling Forecasted Variance vs Realized Variance",
       y = "Variance")


