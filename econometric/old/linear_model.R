rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(patchwork)
library(GGally)

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# keep only data from starting_date on
starting_date <- "2021-01-01"

filter_by_timestamp <- function(df) {
  df %>% 
    filter(timestamp >= starting_date) %>%
    select(timestamp, close)              
}

filtered_list <- lapply(list_indeces, filter_by_timestamp)

# collapse in a unique dataframe
indeces <- reduce(filtered_list, full_join, by = "timestamp")
names(indeces) <- c("timestamp",names(list_indeces))
indeces$NIFTY500 <- NULL

# compute logreturns
minute <- 1
returns <- indeces %>%
  filter(minute(timestamp) %% minute == 0) %>%
  mutate(across(
    -timestamp,
    ~ asinh(./lag(.)),
    .names = "log_ret_{.col}" 
  )) %>%
  select(timestamp, starts_with("log_ret_"))

# linear model log_ret_NIFTY50 = alpha + beta * log_ret_NIFTYMIDCAP100
model <- lm(log_ret_NIFTY50 ~ log_ret_NIFTYMIDCAP100, data = returns)
summary(model)

# inspection for hypotesis
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

plot(returns$log_ret_NIFTYMIDCAP100, returns$log_ret_NIFTY50)

# date of outliers, remove them (covid possible explanation)
outliers_idx <- which(abs(returns$log_ret_NIFTYMIDCAP100)>0.02)
returns[outliers_idx,]$timestamp

returns <- returns %>%
  slice(-outliers_idx)

# linear model log_ret_NIFTY50 = alpha + beta * log_ret_NIFTYMIDCAP100
model <- lm(log_ret_NIFTY50 ~ log_ret_NIFTYMIDCAP100, data = returns)
summary(model)

# inspection for hypotesis
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

# fin services
financial_services <- list_indeces[["NIFTYFINSERVICE"]]
financial_services <- financial_services %>%
  filter(timestamp < "2017-01-31") %>%
  mutate(log_return = log(close) - log(open))

gf <- ggplot(financial_services, aes(x = timestamp, y = log_return)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = "LogReturn Fin",
       x = "Time",
       y = "Value") +
  theme_minimal()

# nifty50
nifty50 <- list_indeces[["NIFTY50"]]
nifty50 <- nifty50 %>%
  filter(timestamp < "2017-01-31") %>%
  mutate(log_return = log(close) - log(open))

gn <- ggplot(nifty50, aes(x = timestamp, y = log_return)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = "LogReturn N50",
       x = "Time",
       y = "Value") +
  theme_minimal()

# banks
banks <- list_indeces[["NIFTYBANK"]]
banks <- banks %>%
  filter(timestamp < "2017-01-31") %>%
  mutate(log_return = log(close) - log(open))

gb <- ggplot(banks, aes(x = timestamp, y = log_return)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = "LogReturn Bank",
       x = "Time",
       y = "Value") +
  theme_minimal()

(gn | gf | gb)

df_panel <- data.frame(n50 = nifty50$log_return, fin = financial_services$log_return, bank = banks$log_return)

ggpairs(df_panel)

model0 <- lm(n50 ~ fin + bank, data = df_panel)
summary(model0)
model1 <- lm(n50 ~ bank, data = df_panel)
summary(model1)
model2 <- lm(n50 ~ fin, data = df_panel)
summary(model2)

# bank stock
hdfc <- list_nifty50[["HDFC"]]
max_vol <- mean(hdfc$volume, na.rm = T)
hdfc <- hdfc %>%
  filter(timestamp < "2017-01-31") %>%
  mutate(log_return = log(close) - log(open), std_vol = volume )
  

ggplot(hdfc, aes(x = timestamp, y = log_return)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = "LogReturn HDFC",
       x = "Time",
       y = "Value") +
  theme_minimal()

df_model <- data.frame(n50 = nifty50$log_return, 
                       fin = financial_services$log_return, 
                       bank = banks$log_return, 
                       hdfc_vol = hdfc$std_vol, 
                       hdfc = hdfc$log_return,
                       open = hdfc$open)

model <- lm(hdfc ~ hdfc_vol, data = df_model)
summary(model)

shapiro.test(hdfc$log_return[1:5000])
