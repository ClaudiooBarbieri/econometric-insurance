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

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# Banks under NIFTY:
# Axis Bank Ltd.
# Bank of Baroda XXX
# Canara Bank 
# Federal Bank Ltd. 
# HDFC Bank Ltd. 
# ICICI Bank Ltd. 
# IDFC Bank Ltd. 
# IndusInd Bank Ltd. 
# Kotak Mahindra Bank Ltd. 
# Punjab National Bank XXX
# State Bank of India 
# Yes Bank Ltd.

# Create a list of all banking dataframes
bank_data <- list(
  NIFTYBANK = list_indeces[["NIFTYBANK"]],
  AXISBANK = list_nifty50[["AXISBANK"]],
  CANBK = list_nifty100_mc[["CANBK"]],
  FEDERALBNK = list_nifty100_mc[["FEDERALBNK"]],
  HDFCBANK = list_nifty50[["HDFCBANK"]],
  ICICIBANK = list_nifty50[["ICICIBANK"]],
  IDFCFIRSTB = list_nifty100_mc[["IDFCFIRSTB"]],
  INDUSINDBK = list_nifty50[["INDUSINDBK"]],
  KOTAKBANK = list_nifty50[["KOTAKBANK"]],
  SBIN = list_nifty50[["SBIN"]],
  YESBANK = list_nifty100_mc[["YESBANK"]]
)

df <- bank_data[["YESBANK"]]

# Convert Datetime column to POSIXct
df$Datetime <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata")

# Extract Date and Time
df$Date <- as.Date(df$Datetime, tz="Asia/Kolkata")
df$Time <- format(df$Datetime, format="%H:%M:%S")

# Filter Only Market Hours (9:15 AM - 3:30 PM)
df <- df %>%
  filter(Time >= "09:15:00" & Time <= "15:30:00") %>%
  filter(Date < "2018-01-01")

df <- df %>%
  arrange(Datetime) %>%
  mutate(LogReturn = c(NA, diff(log(close))))

df <- na.omit(df)
df <- inf.omit(df)

df <- df %>%
  mutate(NewDay = ifelse(Time == "09:15:00", 1, 0))

df$SMA_5 <- SMA(df$close, n=5)   # 5-minute moving average
df$EMA_5 <- EMA(df$close, n=5)   # 5-minute exponential moving average
df$Volatility_10 <- runSD(df$close, n=10)  # Rolling standard deviation (volatility)
df$RSI_14 <- RSI(df$close, n=14)  # Relative Strength Index
df$Volume_Change <- df$volume / lag(df$volume, 1) - 1

df <- NaRV.omit(df)

model <- lm(LogReturn ~ Volatility_10 + RSI_14 + Volume_Change, data=df)
summary(model)
plot(model)
ggpairs(data)
#end
























































# Filter each dataframe to include only data before end_date
end_date <- "2018-01-01"
#common_timestamp <- intersect(nifty_bank$timestamp, axis$timestamp)

bank_filtered_data <- lapply(bank_data, function(x) {
  x %>% filter(timestamp < end_date) #%>% filter(timestamp %in% common_timestamp)
})

ggplot(bank_filtered_data[['AXISBANK']], aes(x = timestamp, y = close)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + # points
  labs(title = "Close price",
       x = "Time",
       y = "Value") +
  theme_minimal()

df <- bank_filtered_data[['AXISBANK']]
df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata")
ts <- xts(df$close, order.by = df$timestamp)

plot(ts)

# Convert the filtered data back to individual variables
list2env(filtered_data, .GlobalEnv)

# Create a combined dataframe with one column from each bank
combined_df <- bind_cols(
  NIFTYBANK %>% select(close),
  AXISBANK %>% select(close),
  CANBK %>% select(close),
  FEDERALBNK %>% select(close),
  HDFCBANK %>% select(close),
  ICICIBANK %>% select(close),
  IDFCFIRSTB %>% select(close),
  INDUSINDBK %>% select(close),
  KOTAKBANK %>% select(close),
  SBIN %>% select(close),
  YESBANK %>% select(close)
)

ggpairs(combined_df)