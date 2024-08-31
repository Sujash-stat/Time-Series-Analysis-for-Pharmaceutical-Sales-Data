df <- read.csv("C:/Users/SUJASH/OneDrive/Desktop/Time Series/data/salesweekly.csv")
head(df_1)
length(te_drug)

# library required for forecasting  
library(forecast) 
library(tidyverse)
library(dplyr)
#install.packages("tseries")
library(tseries)

df_1 <- df[,1:2]
df_1 <- ts(df_1)

head(df_1)
plot(drug, type = 'line')
drug <- df_1$M01AB
# Load necessary library
library(zoo)

# Calculate 7-day moving average
moving_avg <- rollmean(drug, k = 7, fill = NA, align = "right")

# Print the moving average
print(moving_avg)

# Plot the original data and the moving average
plot(drug, type = "l", col = "blue", lwd = 2, ylab = "Value", main = "7-Day Moving Average")
lines(moving_avg, col = "red", lwd = 2)
legend("topright", legend = c("Original Data", "7-Day Moving Avg"), col = c("blue", "red"), lwd = 2)
###############################
tr_drug <- moving_avg[1:211]
te_drug <- data.frame(moving_avg[212:302])
tsdata <- ts(tr_drug,start=1,frequency=211)
model <- auto.arima(tsdata,seasonal=F)
forecast_values <- forecast(model, h = 91)
d <- data.frame(forecast_values$mean)
mse <- sum(abs(te_drug$moving_avg.212.302. - d$forecast_values.mean ))
mse/91
df <- data.frame(te_drug$moving_avg.212.302.,d$forecast_values.mean )
adf <- adf.test()

plot(forecast_values,type="l")
lines(moving_avg,type="l",col = "blue")

################################
tr_drug <- drug[1:211]
te_drug <- data.frame(drug[212:302])
tsdata <- ts(tr_drug,start=1,frequency=211)
model <- auto.arima(tsdata,seasonal=F)
forecast_values <- forecast(model, h = 91)
d <- data.frame(forecast_values$mean)
mse <- sum((te_drug$drug.212.302. - d$forecast_values.mean ))^2
mse/91
adf <- adf.test(tsdata)
