rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)

# load image with the lists of dataframes
load("lists_of_datasets.RData")

# selected time series for the analysis
nifty50 <- list_indeces$NIFTY50

# drop data we are not going to use
rm(list_nifty100_mc, list_indeces)

# before dropping underlying stock, keeps aggregated volume, we need to handle the case of date-time for which we do not have date
# we consider as zero volume

combined <- bind_rows(list_nifty50, .id = "source")

# Get all unique dates
all_dates <- data.frame(timestamp = unique(combined$timestamp))

# Left join each data frame to all dates and sum
nifty50_volume <- list_nifty50 %>%
  map(~ right_join(., all_dates, by = "timestamp")) %>%  # Ensure all dates are present
  map(~ replace_na(., list(volume = 0))) %>%        # Replace NA with 0
  map(~ select(., timestamp, volume)) %>%                # Keep only needed columns
  reduce(full_join, by = "timestamp") %>%               # Combine all by date
  mutate(volume = rowSums(select(., starts_with("volume")), na.rm = TRUE)) %>%
  select(timestamp, volume)

rm(list_nifty50, combined, all_dates)

# 2017-07-10 a major technical glitch crippled the National Stock Exchange (NSE) in India, in the morning
setdiff(nifty50$timestamp, nifty50_volume$timestamp)

nifty50 <- nifty50 %>%
  select(-volume) %>%
  left_join(., nifty50_volume, by = "timestamp")

rm(nifty50_volume)

# consider only pre-covid data (2017-22018-2019)and ensure to keep only values related to market hours

select_period <- function(df, start, end){
  
  df$Datetime <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata")
  
  # Extract Date and Time
  df$Date <- as.Date(df$Datetime, tz="Asia/Kolkata")
  df$Time <- format(df$Datetime, format="%H:%M:%S")
  
  # Filter Only Market Hours (9:15 AM - 3:30 PM)
  df <- df %>%
    filter(Time >= "09:15:00" & Time <= "15:30:00") %>%
    filter(Date >= start & Date <= end) %>%
    mutate(NewDay = factor(ifelse(Time == "09:15:00", 1, 0))) %>%       # add dummy for new day
    select(-timestamp) %>%
    select(Datetime, Date, Time, NewDay, everything() ) #reordering
}

nifty50 <- select_period(nifty50, "2017-01-01", "2020-01-01")

# candlestick plot
ggplot(nifty50, aes(x = as.POSIXct(Datetime))) +
  geom_segment(aes(y = low, yend = high, xend = as.POSIXct(Datetime)), color = "black", size = 0.5) +
  geom_rect(aes(ymin = pmin(open, close), ymax = pmax(open, close),
                xmin = as.POSIXct(Date) - 15,  # 15 sec
                xmax = as.POSIXct(Date) + 15,  # 15 sec
                fill = close > open), 
            color = "black") +  # black border to emphasize
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red")) +
  theme_minimal() +
  labs(title = "Intraday Candlestick Chart", y = "Price", x = "Time")

library(plotly)

fig <- plot_ly(data = nifty50, type = "candlestick",
               x = ~Datetime,
               open = ~open,
               high = ~high,
               low = ~low,
               close = ~close) %>%
  layout(title = "Minute-level Candlestick Chart",
         xaxis = list(rangeslider = list(visible = F)),
         yaxis = list(title = "Price"))

fig


# save as csv
write.csv(nifty50, file= "Nifty50.csv")

nifty50 <- nifty50 %>%
  select(-NewDay, everything())

# aggregate at daily level to reduce data noise
nifty_daily <- nifty50 %>%
  group_by(Date) %>%
  summarise(
    open = first(open, na_rm = T),
    high = max(high, na.rm = T),
    low = min(low, na.rm = T),
    close = last(close, na_rm = T),
    volume = sum(volume, na_rm = T),
    .groups = 'drop'
  )

# save as csv
write.csv(nifty_daily, file= "Nifty50_daily.csv")

