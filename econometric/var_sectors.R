 
rm(list = ls())

library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(vars)
library(tidyr)
library(tseries)
library(dynlm)


### Load the data ###
load("lists_of_datasets.RData")


starting_training_date <- "2017-01-01"
end_training_date <- "2017-02-01"

# Filter & select data by timestamp between the 2 dates (all data included)
filter_by_timestamp <- function(df) {
  df %>% 
    filter(timestamp >= starting_training_date,
           timestamp <= end_training_date)
}


filtered_nifty50 <- lapply(list_nifty50, filter_by_timestamp)

# Select the stocks of the automotiv sector and compute the log-returns (log(close of the day/open of the day))
nifty_auto <- c("BAJAJ-AUTO", "EICHERMOT", "HEROMOTOCO", 
                "M&M", "MARUTI", "TATAMOTORS")

list_auto <- filtered_nifty50[names(filtered_nifty50) %in% nifty_auto]

compute_log_return <- function(df) {
  df %>% 
    mutate(log_ret = log(close / open)) %>% 
    select(timestamp, log_ret)
}

list_auto_log <- lapply(list_auto, compute_log_return)

# Merge all data frames by "timestamp" using a full join
auto_log_returns <- reduce(list_auto_log, full_join, by = "timestamp")
names(auto_log_returns) <- c("timestamp", paste0("log_ret_", names(list_auto)))


### Tests for breaks and stationarity ###

sector_vars_log_ret <- c("log_ret_BAJAJ-AUTO", "log_ret_EICHERMOT", "log_ret_HEROMOTOCO", 
                         "log_ret_M&M", "log_ret_MARUTI", "log_ret_TATAMOTORS")

# Perform tests for each time series 

for (varname in sector_vars_log_ret) {
  
  cat("===========================================================\n")
  cat("Analysis for:", varname, "\n")
  cat("===========================================================\n\n")
  
  # Extract the time series
  y <- auto_log_returns[[varname]]
  
  
  # Testing for a break in the level
  fs.y <- Fstats(y ~ 1)
  struct_test <- sctest(fs.y)
  cat("Structural break test (mean) for", varname, ":\n")
  print(struct_test)
  cat("\n")
  
  # Testing for a break in the trend
  train_data <- data.frame(log_ret = y, trend = seq_along(y))
  fs.y.trend <- Fstats(log_ret ~ trend, data = train_data)
  struct_test_trend <- sctest(fs.y.trend)
  cat("Structural break test (trend) for", varname, ":\n")
  print(struct_test_trend)
  cat("\n")
  
  
  ## Stationarity Tests ##
  # ADF test assuming no constant and no trend
  ur_none <- ur.df(y, type = "none", lags = 10)
  cat("ADF Test (no constant, no trend) for", varname, ":\n")
  print(summary(ur_none))
  cat("\n")
  
  # ADF test assuming a constant (drift) only
  ur_drift <- ur.df(y, type = "drift", lags = 10)
  cat("ADF Test (constant/drift, no trend) for", varname, ":\n")
  print(summary(ur_drift))
  cat("\n")
  
  # ADF test assuming both constant and trend
  ur_trend <- ur.df(y, type = "trend", lags = 10)
  cat("ADF Test (constant and trend) for", varname, ":\n")
  print(summary(ur_trend))
  cat("\n\n")
}

# Overall, we can consider each time series stationary.

### VAR model ###
sector_data_log_ret_numeric <- auto_log_returns %>% select(-timestamp)

### Select the optimal number of lags for the VAR ###
lag_selection <- VARselect(sector_data_log_ret_numeric, lag.max = 20, type = "const")
print(lag_selection$selection)

p_bic <- lag_selection$selection["SC(n)"]

# Estimate the VAR model using the selected lag order
var_model_log_ret <- VAR(sector_data_log_ret_numeric, p = p_bic, type = "const")
summary(var_model_log_ret)


# Comments:
# - roots of characteristic polynomial less than 1 in absolute value
# - Statistically significant Granger-causality between stocks in many ways.

# Weakness:
# - Time series considered is quite short compared to the data available, but memory 
#   problems otherwise


### Granger Causality Tests ###

sector_vars_log_ret <- gsub("-", ".", sector_vars_log_ret)
sector_vars_log_ret <- gsub("&", ".", sector_vars_log_ret)

granger_results <- list()
for (cause_var in sector_vars_log_ret) {
  granger_results[[cause_var]] <- causality(var_model_log_ret, cause = cause_var)
}
print(granger_results)

# Results:
# Apart form log_ret_M.M do not Granger-cause the other vars, the other do


### IRF analysis ###

irf_results <- list()
for (impulse in sector_vars_log_ret) {
  for (response in sector_vars_log_ret) {
    if (impulse != response) {
      key <- paste(impulse, "->", response)
      cat("Computing IRF for:", key, "\n")
      irf_results[[key]] <- irf(var_model_log_ret,
                                impulse = impulse,
                                response = response,
                                n.ahead = 20,    # forecast horizon of 20 periods
                                ortho = TRUE,    # orthogonalized impulse responses
                                runs = 1000)     # number of bootstrap runs for CIs
    }
  }
}


### Function that plots the impulse-responses (fatto da GPT :) ###

plot_irf_custom <- function(irf_obj, main_title = "") {
  
  response <- irf_obj$irf[[1]]
  lower    <- irf_obj$Lower[[1]]
  upper    <- irf_obj$Upper[[1]]
  
  horizon <- 0:(length(response) - 1)
  
  ylim_range <- range(c(response, lower, upper), na.rm = TRUE)
  
  # Plot the impulse response
  plot(horizon, response, type = "l", col = "blue",
       ylim = ylim_range, main = main_title,
       xlab = "Horizon", ylab = "Response")
  
  # Add the confidence intervals
  lines(horizon, lower, col = "red", lty = 2)
  lines(horizon, upper, col = "red", lty = 2)
  
  abline(h = 0, lty = 3)
}

if (.Platform$OS.type == "windows") {
  open_graphics_window <- function() windows(width = 15, height = 8)
} else if (Sys.info()["sysname"] == "Darwin") {  # macOS
  open_graphics_window <- function() quartz(width = 15, height = 8)
} else {  
  open_graphics_window <- function() x11(width = 15, height = 8)
}


plot_keys <- names(irf_results)
n_plots <- length(plot_keys)
plots_per_window <- 10
n_windows <- ceiling(n_plots / plots_per_window)

for (w in 1:n_windows) {
  open_graphics_window()  
  
  par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
  
  start_index <- (w - 1) * plots_per_window + 1
  end_index   <- min(w * plots_per_window, n_plots)
  
  # Plot each IRF for the current window using the custom plotting function.
  for (i in start_index:end_index) {
    key <- plot_keys[i]
    cat("Plotting IRF for:", key, "\n")
    plot_irf_custom(irf_results[[key]], main_title = paste("SVAR IRF:", key))
  }
}

par(mfrow = c(1, 1))

### Inspection of the residuals ###

# Ljung–Box test
serial_test <- serial.test(var_model_log_ret, lags.pt = 16, type = "PT.adjusted")
print(serial_test)
#  strong evidence of serial correlation in the residuals

arch_test <- arch.test(var_model_log_ret, lags.multi = 5)
print(arch_test)
# There is significant evidence of heteroscedasticity. conditional variance is not constant
# volatility clustering is present

# Jarque-Brera test
normality_test <- normality.test(var_model_log_ret)
print(normality_test)
# NO normal distribution of the residuals

# Residuals of each forecast
resids <- resid(var_model_log_ret)

# Plot time series of residuals for each variable
par(mfrow = c(2, 3))
for (i in 1:ncol(resids)) {
  plot.ts(resids[, i], main = paste("Residuals for", colnames(resids)[i]),
          ylab = "Residual", xlab = "Time")
}
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))
# Create QQ-plots for each variable
for (i in 1:ncol(resids)) {
  var_name <- colnames(resids)[i]
  
  qqnorm(resids[, i], main = paste("QQ-Plot for", var_name))
  qqline(resids[, i])
 
}

par(mfrow = c(2, 3))
# Create histograms for each variable
for (i in 1:ncol(resids)) {
  var_name <- colnames(resids)[i]
  
  hist(resids[, i], main = paste("Histogram of", var_name),
       xlab = "Residual", col = "lightblue", breaks = 20)
}

### Prediction ###

forecast_results <- predict(var_model_log_ret, n.ahead = 10)
print(forecast_results)


# Extract the test set (first 10 observations with timestamp > end_training_date)
filter_test <- function(df) {
  df %>% 
    filter(timestamp > end_training_date) %>%
    head(10)
}


test_set_tot <- lapply(list_nifty50, filter_test )

test_set <- test_set[names(test_set) %in% nifty_auto]

test_set_log <- lapply(test_set, compute_log_return)

# Merge all data frames by "timestamp" using a full join
test_set_log <- reduce(test_set_log, full_join, by = "timestamp")
names(test_set_log) <- c("timestamp", paste0("log_ret_", names(test_set)))

# Initialize a forecast data frame with the timestamps from the test set
forecast_df <- data.frame(timestamp = test_set_log$timestamp)

names(test_set_log) <- gsub("-", ".", names(test_set_log))
names(test_set_log) <- gsub("&", ".", names(test_set_log))


# Loop through each variable
for (var in names(test_set_log)[-1]) {
  print(var)
  # Extract point forecasts for this variable
  print(forecast_results[[var]])
  fcst <- forecast_results$fcst[[var]][, "fcst"]
  print(fcst)
  forecast_df[[var]] <- fcst
}

# View the forecast data frame
print(forecast_df)

# Initialize a list to store metrics for each variable
metrics <- lapply(names(test_set_log)[-1], function(var) {
  actual <- test_set_log[[var]]
  forecasted <- forecast_df[[var]]
  mse <- mean((actual - forecasted)^2, na.rm = TRUE)
  mae <- mean(abs(actual - forecasted), na.rm = TRUE)
  rmse <- sqrt(mse)
  
  data.frame(variable = var, MSE = mse, MAE = mae, RMSE = rmse)
})

# Combine the metrics for all variables into a single data frame
metrics_df <- do.call(rbind, metrics)
print(metrics_df)

# The model predicts MARUTI’s log returns most accurately.
# The weaker performance is for EICHERMOT and M&M


# Plot of the forecast#

timestamp <- as.POSIXct(test_set_log$timestamp)

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

for (var in names(test_set_log)[-1]) {
  actual <- test_set_log[[var]]
  forecasted <- forecast_df[[var]]
  
  y_range <- range(c(actual, forecasted), na.rm = TRUE)
  
  plot(timestamp, actual, type = "o", col = "blue", lwd = 2, pch = 16,
       ylim = y_range, xlab = "Timestamp", ylab = "Log Return", main = var)
  
  lines(timestamp, forecasted, type = "o", col = "red", lwd = 2, pch = 17)
  
  legend("topright", legend = c("Actual", "Prediction"),
         col = c("blue", "red"), lty = 1, pch = c(16, 17), cex = 0.8)
}

par(mfrow = c(1, 1))

# Very poor performance #


##################################################################################################
##### Let us try to work with a VARX, including the volume as an exogenous external variable #####
##################################################################################################

# We choose volume (LAGGED) as it reflects trading intensity and liquidity, while the other
# external variables introduce redundancy and multicollinearity

# Build the volume dataset
volume_auto <- filtered_nifty50[names(filtered_nifty50) %in% nifty_auto]

select_volume <- function(df) {
  df %>% select(timestamp, volume)
}

list_volume_sel <- lapply(volume_auto, select_volume)

# Merge all volume data frames by "timestamp" using a full join
volume_data <- reduce(list_volume_sel, full_join, by = "timestamp")
names(volume_data) <- c("timestamp", paste0("vol_", names(volume_auto)))

# We need to work with the volume of the previous observation, since
# at forecasting time this is not available
lag_volume <- function(vec) {
    c(NA, vec[-length(vec)])
}
volume_data_lagged <- volume_data
for(i in 2:ncol(volume_data_lagged)){
  volume_data_lagged[[i]] <- lag_volume(volume_data_lagged[[i]])
}

varx_data <- auto_log_returns %>% inner_join(volume_data_lagged, by = "timestamp")

# Remove the first row (lag_volume introduced NA)
varx_data <- varx_data %>% filter(complete.cases(.))

# Endogenous matrix (log returns)
endog_vars <- varx_data %>% select(starts_with("log_ret_"))

# Exogenous matrix (lagged volume)
exogen_vars <- varx_data %>% select(starts_with("vol_")) %>% as.matrix()

#### VARX model ####
varx_model <- VAR(endog_vars, p = p_bic, type = "const", exogen = exogen_vars)
summary(varx_model)

### Residual analysis ###

# Serial correlation test
serial_test <- serial.test(varx_model, lags.pt = 16, type = "PT.adjusted")
print(serial_test)

# ARCH test for heteroscedasticity
arch_test <- arch.test(varx_model, lags.multi = 5)
print(arch_test)

# Normality test
normality_test <- normality.test(varx_model)
print(normality_test)

# Still, the statistics highlight autocorrelation, heteroscheasticity
# and absence of normality

# Plot residuals
resids_varx <- resid(varx_model)
par(mfrow = c(2, 3))
for (i in 1:ncol(resids)) {
  plot.ts(resids_varx[, i], main = paste("Residuals for", colnames(resids)[i]),
          ylab = "Residual", xlab = "Time")
}
par(mfrow = c(1, 1))


par(mfrow = c(2, 3))
# Create QQ-plots for each variable
for (i in 1:ncol(resids_varx)) {
  var_name <- colnames(resids_varx)[i]
  
  qqnorm(resids_varx[, i], main = paste("QQ-Plot for", var_name))
  qqline(resids_varx[, i])
  
}

par(mfrow = c(2, 3))
# Create histograms for each variable
for (i in 1:ncol(resids_varx)) {
  var_name <- colnames(resids_varx)[i]
  
  hist(resids_varx[, i], main = paste("Histogram of", var_name),
       xlab = "Residual", col = "lightblue", breaks = 20)
}

##### Forecasting ######

### Build the exogenous matrix for the test volume
# Test set for volume. Prepend the last volume in the training set
last_train_volume <- volume_data %>% 
  filter(timestamp <= end_training_date) %>% 
  tail(1)

test_volume_auto <- lapply(test_set, select_volume)
test_volume_auto <- reduce(test_volume_auto, full_join, by = "timestamp")

names(test_volume_auto) <- c("timestamp", 
                             paste0("vol_", names(volume_auto)[names(volume_auto) %in% nifty_auto]))
test_volume_auto <- bind_rows(last_train_volume, test_volume_auto)

# Apply lag to the test volume data (like in the training set)
for(i in 2:ncol(test_volume_auto)){
  test_volume_auto[[i]] <- lag_volume(test_volume_auto[[i]])
}

# Remove rows with NA introduced by lagging
test_volume_auto <- test_volume_auto %>% filter(complete.cases(.))

# Build the exogenous test matrix
exogen_test_matrix <- test_volume_auto %>% select(-timestamp) %>% as.matrix()
colnames(exogen_test_matrix) <- gsub("-", ".", colnames(exogen_test_matrix))
colnames(exogen_test_matrix) <- gsub("&", ".", colnames(exogen_test_matrix))

### Forecasting ###

# Now forecast with the exogenous test matrix 
forecast_results_varx <- predict(varx_model, n.ahead = 10,
                                 dumvar = exogen_test_matrix)
print(forecast_results_varx)

# Forecast data frame 
forecast_df_varx <- data.frame(timestamp = test_set_log$timestamp)

for (var in names(test_set_log)[-1]) {
  fcst <- forecast_results_varx$fcst[[var]][, "fcst"]
  forecast_df_varx[[var]] <- fcst
}

print(forecast_df_varx)

# Compute error metrics
metrics_varx <- lapply(names(test_set_log)[-1], function(var) {
  actual <- test_set_log[[var]]
  forecasted <- forecast_df_varx[[var]]
  mse <- mean((actual - forecasted)^2, na.rm = TRUE)
  mae <- mean(abs(actual - forecasted), na.rm = TRUE)
  rmse <- sqrt(mse)
  data.frame(variable = var, MSE = mse, MAE = mae, RMSE = rmse)
})
metrics_df_varx <- do.call(rbind, metrics_varx)
print(metrics_df_varx)

# Compare the metrics with the previous model
metrics_df <- metrics_df %>% 
  mutate(Model = "VAR")
metrics_df_varx <- metrics_df_varx %>% 
  mutate(Model = "VARX")

comparison_df <- rbind(metrics_df, metrics_df_varx)
print("Comparison of Forecasting Metrics (Long Format):")
print(comparison_df)

# Compute overall metrics
overall_metrics <- comparison_df %>%
  group_by(Model) %>%
  summarize(
    Overall_MSE = mean(MSE, na.rm = TRUE),
    Overall_MAE = mean(MAE, na.rm = TRUE),
    Overall_RMSE = mean(RMSE, na.rm = TRUE)
  )

overall_metrics_df <- as.data.frame(overall_metrics)
print("Overall Average Metrics by Model (with more decimals):")
print(format(overall_metrics_df, digits = 10))

# VAR seems to be slightly better!


# Plot actual vs forecasted log returns for each variable in the VARX model
timestamp <- as.POSIXct(test_set_log$timestamp)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
for (var in names(test_set_log)[-1]) {
  actual <- test_set_log[[var]]
  forecasted <- forecast_df_varx[[var]]
  y_range <- range(c(actual, forecasted), na.rm = TRUE)
  
  plot(timestamp, actual, type = "o", col = "blue", lwd = 2, pch = 16,
       ylim = y_range, xlab = "Timestamp", ylab = "Log Return", main = var)
  lines(timestamp, forecasted, type = "o", col = "red", lwd = 2, pch = 17)
  legend("topright", legend = c("Actual", "Prediction"),
         col = c("blue", "red"), lty = 1, pch = c(16, 17), cex = 0.8)
}
par(mfrow = c(1, 1))










#################### EXTRA PIECES OF CODE FOR FURTHER ANALYSIS #####################

# NIFTY AUTO
nifty_auto <- c("BAJAJ-AUTO", "EICHERMOT", "HEROMOTOCO", 
                "M&M", "MARUTI", "TATAMOTORS")

# NIFTY BANK
nifty_bank <- c("AXISBANK", "HDFCBANK", "ICICIBANK", 
                "INDUSINDBK", "KOTAKBANK", "SBIN")

# NIFTY COMMODITIES (includes metals, cement, etc.)
nifty_commodities <- c("TATASTEEL", "JSWSTEEL", "HINDALCO", 
                       "COALINDIA", "GRASIM", "ULTRACEMCO", "UPL")

# NIFTY FIN SERVICE
nifty_fin_service <- c("HDFC", "BAJFINANCE", "BAJAJFINSV", "HDFCLIFE")

# NIFTY ENERGY
nifty_energy <- c("RELIANCE", "ONGC", "NTPC", 
                  "POWERGRID", "BPCL", "IOC")

# Compute log-returns
log_ret_df_sectors <- indeces %>%
  mutate(across(
    -timestamp,
    ~ log(. / lag(.)),  # Log-return transformation
    .names = "log_ret_{.col}"
  )) %>%
  drop_na()  # Remove the first row with NA values

# Select the data related to sectors
sector_vars_log_ret <- c("log_ret_NIFTYAUTO",
                         "log_ret_NIFTYBANK",
                         "log_ret_NIFTYCOMMODITIES",
                         "log_ret_NIFTYFINSERVICE",
                         "log_ret_NIFTYENERGY")

sector_data_log_ret <- log_ret_df_sectors %>%
  select(timestamp, all_of(sector_vars_log_ret))

sector_data_log_ret_numeric <- sector_data_log_ret %>%
  select(-timestamp)

### Check for stationarity ###
for (varname in sector_vars_log_ret ) {
  cat("ADF test for:", varname, "\n")
  print(adf.test(sector_data_log_ret_numeric[[varname]]))
  cat("\n------------------------------------\n\n")
}


### Select the optimal number of lags for the VAR (done using log-returns) ###

# Analysis by dropping timestamp
lag_selection <- VARselect(sector_data_log_ret_numeric, lag.max = 20, type = "const")
lag_selection$selection

# They are all consistent. 1 lag is the best choice
p_bic <- lag_selection$selection["SC(n)"]

# Var model
var_model_log_ret <- VAR(sector_data_log_ret_numeric, p = p_bic, type = "const")
summary(var_model_log_ret)

# Comments:
# - roots of characteristic polynomial less than 1 in absolute value
# - No statistically significant Granger-causality between sectors.
# - Very poor model
# - High residual correlation between Bank and Financial Services and Commodities and 
#   Enerrgy residual

# Weakness:
# - Time series considered is quite short compared to the data available, but memory 
#   problems otherwise

# Let us verify with a Granger causality test 
granger_results <- list()

for (cause_var in sector_vars_log_ret) {
  granger_results[[cause_var]] <- causality(var_model_log_ret, cause = cause_var)
}
print(granger_results)

# Evidence to assume instant causality, but not "lagged" causality
# --> The residual covariance matrix is probably capturing the strong contemporaneous correlation

# Impulse-response function
irf_results <- list()

for (impulse in sector_vars_log_ret) {
  for (response in sector_vars_log_ret) {
    if (impulse != response) {
      key <- paste(impulse, "->", response)
      cat("Computing IRF for:", key, "\n")
      irf_results[[key]] <- irf(var_model_log_ret,
                                impulse = impulse,
                                response = response,
                                n.ahead = 20,   
                                ortho = TRUE,   
                                runs = 1000)        }
  }
}

# Plot the impulse-response functions
n_plots <- length(irf_results) 
n_cols <- ceiling(sqrt(n_plots))
n_rows <- ceiling(n_plots / n_cols)


par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

for (key in names(irf_results)) {
  cat("Plotting IRF for:", key, "\n")
  plot(irf_results[[key]], main = paste("SVAR IRF:", key))
}
par(mfrow = c(1, 1))





### Cointegration analysis ###
# Let us see if a cointegration relationship exists in the secotr indices

# In this setting we reset the starting date (in order to have more lags)
starting_date <- "2020-01-01"

# Filter & select 'timestamp' and 'close'
filter_by_timestamp <- function(df) {
  df %>% 
    filter(timestamp >= starting_date) %>%
    select(timestamp, close)              
}

filtered_list <- lapply(list_indeces, filter_by_timestamp)

# Merge into one dataframe by timestamp
indeces <- reduce(filtered_list, full_join, by = "timestamp")
names(indeces) <- c("timestamp", names(list_indeces))

indeces$NIFTY500 <- NULL

sector_vars <- c("NIFTYAUTO",
                 "NIFTYBANK",
                 "NIFTYCOMMODITIES",
                 "NIFTYFINSERVICE",
                 "NIFTYENERGY")

# Create a data frame for these sectors 
df_sectors <- indeces %>%
  select(timestamp, all_of(sector_vars)) %>%
  drop_na()  

### Check for stationarity ###
for (varname in sector_vars) {
  cat("ADF test for:", varname, "\n")
  print(adf.test(df_sectors[[varname]]))
  cat("\n------------------------------------\n\n")
}

# Under all specifications and testing up to 22 lags, we have significance to accept
# H0 --> all the time series are non-stationary

# Perform same analysis on the differences

diff_df_sectors <- df_sectors %>%
  mutate(across(
    -timestamp,
    ~ c(NA, diff(.)),  
    .names = "diff_{.col}"
  )) %>%
  drop_na()  # remove initial NA row


sector_vars_diff <- c("diff_NIFTYAUTO",
                 "diff_NIFTYBANK",
                 "diff_NIFTYCOMMODITIES",
                 "diff_NIFTYFINSERVICE",
                 "diff_NIFTYENERGY")

for (varname in sector_vars_diff) {
  cat("ADF test for:", varname, "\n")
  print(adf.test(diff_df_sectors[[varname]]))
  cat("\n------------------------------------\n\n")
}

# Under all specifications and testing up to 22 lags, we have significance to reject
# H0 --> all the time series are stationary

sector_data_numeric <- df_sectors %>% select(-timestamp)

# Select the number of lags
lag_selection <- VARselect(sector_data_numeric, lag.max = 20, type = "const")
lag_selection$selection

# Select the lag
p_bic <- lag_selection$selection["SC(n)"]

# FAIL if considering a constant term. We proceed by being parsimonious and neglecting
# this term. FAIL if num_lags < 2
coint_test <- ca.jo(sector_data_numeric, type = "trace", ecdet = "none", K = 2)
summary(coint_test)

# Comments
# - With confidence 95% we can state there is exactly one cointegrarion
#   relationship
# - The cointegration relationship: 
#   NIFTYAUTO.l2+0.1163NIFTYBANK.l2−4.2464NIFTYCOMMODITIES.l2+0.0199NIFTYFINSERVICE.l2+0.0443NIFTYENERGY.l2=0
# - Negative speed of  adjustment in the corresponding loading matrix: the variables
#   adjust to restore the long-run equilibrium

beta <- coint_test@V[, 1]
beta_normalized <- beta / beta[1] # not nec
cat("Normalized cointegrating vector:\n")
print(beta_normalized)

ECT <- as.matrix(sector_data_numeric) %*% beta_normalized
plot.new()
par(mfrow = c(1,1))
plot(ts(ECT), type = 'l', xlab = "Time", ylab = "ECT", 
     main = "Error Correction Term (Cointegration Relationship)")
abline(h = 0, col = "red", lty = 2)

# Non funzia :(
vec_model_auto <- dynlm(diff(NIFTYAUTO) ~ lag(ECT, 1), data = df_sectors)
summary(vec_model_auto)

# Provare sto error-correction per una qualche trading strategy?



