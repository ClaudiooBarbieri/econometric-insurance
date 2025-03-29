 
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

# IF go further back, memory pbs with VAR
starting_date <- "2021-01-01"

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

dev.new() 
par(mfrow = c(n_rows, n_cols))

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



