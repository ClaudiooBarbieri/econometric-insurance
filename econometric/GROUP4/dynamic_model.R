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

# add differenced values for y1 showing non stationarity

ydiff <- y %>%
  mutate(y1 = y1 - lag(y1)) %>%
  na.omit()

# VAR on it, windows q, m ahead
q <- 90
m <- 1
n <- nrow(ydiff)
predictions <- data.frame(
  open = numeric(n - q - m + 1),
  high = numeric(n - q - m + 1),
  low = numeric(n - q - m + 1),
  close = numeric(n - q - m + 1)
)

for (i in seq(q, nrow(y) - m)) {
  y_train <- ydiff[(i - q + 1):i, , drop = FALSE]
  
  # Step 1: Select optimal lag
  lag_selection <- VARselect(y_train, lag.max = 10, type = "const")
  p <- lag_selection$selection["AIC(n)"]
  
  var_model <- VAR(y_train, p = p, type = "const")
  forecast <- predict(var_model, n.ahead = m)
  
  X_pred <- backtransform(y$y1[i] + forecast$fcst$y1[,"fcst"], forecast$fcst$y2[,"fcst"], forecast$fcst$y3[,"fcst"], forecast$fcst$y4[,"fcst"])
    
  predictions[i-q+1, "open"] <- X_pred$open
  predictions[i-q+1, "high"] <- X_pred$high
  predictions[i-q+1, "low"] <- X_pred$low
  predictions[i-q+1, "close"] <- X_pred$close
}

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













# select q time period
q <- 90
yq <- y[1:q,]
actual <- nifty[(q+1):(q+7),]



# signal of non stationarity for y1 -> differenciate it
base <- yq$y1[q]
yq$y1 <- yq$y1 - lag(yq$y1)
yq <- yq[-1,]
adf.test(yq$y1)
par(mfrow=c(1,2))
acf(yq$y1, main = "ACF")
pacf(yq$y1, main = "PACF")
par(mfrow=c(1,1))

# fit the VAR on the stationary time series
varsel <- VARselect(yq, lag.max = 10, type = "none")
k <- varsel$selection["AIC(n)"]
var_model <- VAR(yq, p = k, type = "none")

# predict differences
prediction <- predict(var_model, n.ahead = 7)

# compute back non differenced value
yyy <- c(base, prediction$fcst$y1[,"fcst"])
yyycumsum <- cumsum(yyy)[-1]












ggplot(nifty, aes(x = as.Date(Date))) +
  geom_segment(aes(y = low, yend = high, xend = as.Date(Date)), color = "black") +
  geom_rect(aes(ymin = pmin(open, close), ymax = pmax(open, close),
                xmin = as.Date(Date) - 0.3, xmax = as.Date(Date) + 0.3,
                fill = close > open)) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red")) +
  theme_minimal() +
  labs(title = "OHLC Chart", y = "Price", x = "Date")

# find cointegration relationship with Johansen approach
johansen_result <- ca.jo(yq, type = "trace", ecdet = "const", K = 4)
summary(johansen_result)
# if cointegration relationships exist
cr <- 3
vecm_model <- vec2var(johansen_result, r = cr)
summary(vecm_model)
# otherwise