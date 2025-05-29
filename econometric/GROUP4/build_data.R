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

# save as csv
write.csv(nifty50, file= "Nifty50.csv")



