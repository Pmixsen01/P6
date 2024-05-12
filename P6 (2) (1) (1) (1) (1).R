library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)
library(forecast)
library(vars)
library(ggfortify)
library(tseries)
library(urca)
library(ggpubr)
#library(fpp2)

# Fetch data for elctiricity prices
elecprices <- GET("https://api.energidataservice.dk/dataset/Elspotprices?limit=1000000&start=2015-01-01T00:00&end=2021-01-01T00:00&filter={\"PriceArea\":\"DK1\"}")
elpriccontent <- content(elecprices, as = "text")

# Parse JSON content
strjson <- fromJSON(elpriccontent)

# Convert to data frame
Elpriser <- strjson$records

# Adjusting the hours to represent readable dates and hours.
Elpriser$HourDK <- ymd_hms(Elpriser$HourDK)

# Add a column that indicates the daily number
Elpriser$day <- floor_date(Elpriser$HourDK, "day")

# Group by days and calculate the daily mean
daily_price_means <- Elpriser %>%
  group_by(day) %>%
  summarise(MeanPrice = mean(SpotPriceDKK, na.rm = TRUE))

# Splitting spotprices into training and testing sets
training_data_price <- subset(daily_price_means, day < as.Date("2020-01-01"))
testing_data_price <- subset(daily_price_means, day >= as.Date("2020-01-01"))


#We can now do the same for consumption, where we will end up with a dataframe with both data
#Fetching consumtion datasheet
consump <- GET("https://api.energidataservice.dk/dataset/ProductionConsumptionSettlement?limit=1000000&start=2015-01-01T00:00&end=2021-01-01T00:00&filter={\"PriceArea\":\"DK1\"}")

consumpcontent <- content(consump, as = "text")

constrjson <- fromJSON(consumpcontent)
Consumption <- constrjson$records

Consumption$HourDK <- ymd_hms(Consumption$HourDK)
Consumption$day <- floor_date(Consumption$HourDK, "day")

daily_cons_means <- Consumption %>%
  group_by(day) %>%
  summarise(DailyConsumption = mean(GrossConsumptionMWh, na.rm = TRUE))

# Splitting consumption into training and testing sets
training_data_consumption <- subset(daily_cons_means, day < as.Date("2020-01-01"))
testing_data_consumption <- subset(daily_cons_means, day >= as.Date("2020-01-01"))

#########################################################################
# Now, daily_means is our new dataframe with the mean price for each week
#We can now create an initial time series to check for how we want to proceed.
#Plotting the ts with dates in the x-axis  
price_ts <- ts(training_data_price$MeanPrice,frequency = 365, 
               start = c(2015, which(day(ymd("2015-01-01")) == 
                                       training_data_price$day[1])))
plot(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2015-2020")
ggtsdisplay(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Spot price from 2015 through 2019")

#Making time series of consumption
consumption_ts <- ts(training_data_consumption$DailyConsumption, frequency = 365, 
                     start = c(2015, which(day(ymd("2015-01-01")) == 
                                             training_data_consumption$day[1])))
plot(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2020")
ggtsdisplay(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption of electricity from 2015 through 2019")

###
#outlier removal
# Function to handle outliers using LOESS smoothing
handle_outliers_ts <- function(ts_data, span = 0.05, threshold = 3) {
  # Fit a loess model
  loess_fit <- loess(ts_data ~ seq_along(ts_data), span = span)
  
  # Calculate smoothed values
  smoothed <- predict(loess_fit)
  
  # Calculate residuals
  residuals <- ts_data - smoothed
  
  # Calculate the threshold based on standard deviation
  limit <- threshold * sd(residuals, na.rm = TRUE)
  
  # Identify outliers
  outliers <- which(abs(residuals) > limit)
  
  # Replace outliers
  ts_data[outliers] <- smoothed[outliers]
  
  return(ts_data)
}

# Applying the function to price and consumption time series
price_ts <- handle_outliers_ts(price_ts)
consumption_ts <- handle_outliers_ts(consumption_ts)

############################################################################
#creating the ACF to find what should be done to the time series to achieve stationarity
Acf(price_ts, lag.max = 60, main = "ACF of spot prices with lag 60")
Acf(consumption_ts, lag.max = 60, main = "ACF of gross consumption with lag 60")

### 
# Performing ADF tests on them all
adf_pricets <- ur.df(price_ts, type = "drift", lags = 1)
summary(adf_pricets)
adf_consts <- ur.df(consumption_ts, type = "drift", lags = 1)
summary(adf_consts)

adf.test(price_ts)
adf.test(consumption_ts)

####################################
# Decomposition of each ts
# Decomposition of price time series
#MSTL
msts <- msts(price_ts, seasonal.periods = c(7, 365))
mstl <- mstl(msts)
autoplot(mstl, main = "Decomposition of spot prices")

cmsts <- msts(consumption_ts, seasonal.periods = c(7, 365))
cmstl <- mstl(cmsts)
autoplot(cmstl, main = "Decomposition of Gross Consumption")
#################################
#making the time series as a linear regression where we have the time series in 2 components
#X_t=s_t+Y_t, one is deterministic and one is stochastic

#setting a time interval, which is just our days in the whole periode
t <- 1:length(training_data_price$MeanPrice)

#Constructing the deterministic ts for price 
st_price <- lm(training_data_price$MeanPrice ~ t 
               + sin(t*2*pi/7) + cos(t*2*pi/7)
               + sin(t*2*pi/30) + cos(t*2*pi/30)
               + sin(t*2*pi/180) + cos(t*2*pi/180) 
               + sin(t*2*pi/365) + cos(t*2*pi/365))

# Now for the consumption
st_consumption <- lm(training_data_consumption$DailyConsumption ~ t 
                     + sin(t*2*pi/7) + cos(t*2*pi/7)
                     + sin(t*2*pi/30) + cos(t*2*pi/30)
                     + sin(t*2*pi/180) + cos(t*2*pi/180) 
                     + sin(t*2*pi/365) + cos(t*2*pi/365))

# Generate fitted values using the linear model
fitted_values_prices <- fitted(st_price)
fitted_values_consumption <- fitted(st_consumption)

# Subtract the fitted values from price_ts
adjusted_pricets <- price_ts - fitted_values_prices
adjusted_consumptionts <- consumption_ts - fitted_values_consumption

#plots showing the original the determisistic series and then the adjusted
ts.plot(fitted_values_prices, price_ts, adjusted_pricets, gpars = list(col = c("red", "blue", "black")))
ts.plot(fitted_values_consumption, consumption_ts, adjusted_consumptionts, gpars = list(col = c("red", "blue", "black")))
autoplot(consumption_ts, series = "cons", ylab = "hej", colour = "black") + 
  autolayer(adjusted_consumptionts, series = "adjcons") + 
  autolayer(st_consts, series = "adjusted")

# Display using ggplots
ggtsdisplay(adjusted_pricets, lag.max = 60)
ggtsdisplay(adjusted_consumptionts, lag.max = 60)
Acf(adjusted_consumptionts, lag.max = 60)

#looking at the ACF of each ts we still have some trend and season remaining that we want to remove with a SARIMA
# Tests for which model desribes the season and trend, RUN AT YOUR own discretion
#test_price <- auto.arima(adjusted_pricets, stepwise = FALSE, approximation = FALSE, D = 1)
#test_consump <- auto.arima(adjusted_consumptionts, stepwise = FALSE, approximation = FALSE, D = 1)

################################################################################
# Creating the optimal ts given the results from the previous tests
###
#We start with the price
price_arima <- Arima(adjusted_pricets,
                     order = c(4,1,1),
                     seasonal = list(order = c(1,1,1), period = 7))
checkresiduals(price_arima, lag.max = 49)
###
# Then for consumption
consumption_arima <- Arima(adjusted_consumptionts,
                           order = c(5,1,0),
                           seasonal = list(order = c(1,1,1), period = 7))
checkresiduals(consumption_arima, lag.max = 49)

#fitting the values
pricets_fit <- fitted(price_arima)
consumptionts_fit <- fitted(consumption_arima)

# making the residuals
res_price <- adjusted_pricets - pricets_fit
Acf(res_price, lag.max = 90)
autoplot(price_ts, series = "price_ts") + autolayer(res_price, series = "res_price") + 
  autolayer(pricets_fit, series = "fitted price")

#for consumption
res_consumption <- adjusted_consumptionts - consumptionts_fit
Acf(res_consumption, lag.max = 30)
autoplot(consumption_ts, series = "consumption_ts") + autolayer(res_consumption, series = "res_cons") + 
  autolayer(consumptionts_fit, series = "fitted consumption")

#making the ggtsdisplays
ggtsdisplay(res_price, main = "Cleaned spot price model")
ggtsdisplay(res_consumption, main = "Cleaned consumption model")

################################################################################
# Constructing the VAR
# Firstly we test the number of p
#crafting the collected ts to make the var model
colle_ts <- cbind(pricets_fit , consumptionts_fit)

###
#Selcting the number of p to include
lag_select <- VARselect(colle_ts, lag.max = 10, type = "const")
lag_select$selection

aic_values <- numeric(15)
bic_values <- numeric(15)

for (i in 1:20) {
  model <- VAR(colle_ts, p=i, type="const")
  aic_values[i] <- AIC(model)
  bic_values[i] <- BIC(model)
}

# Plot AIC and BIC values to visualize the optimal lag
plot(aic_values, type='b', col='blue', xlab='Number of Lags', ylab='AIC', main='AIC and BIC by Lag')
points(bic_values, type='b', col='red')
legend("topright", legend=c("AIC", "BIC"), col=c("blue", "red"), pch=1)


Model1 <- VAR(colle_ts, p = 8, type = "const", season = NULL, exog = NULL)
restricted_model <- restrict(Model1)

#summary
summary(Model1)
summary(restricted_model)

res_var <- residuals(Model1)
Acf(res_var)

plot(res_var)

plot(restricted_model$datamat$pricets_fit)
price_var <- ts(restricted_model$datamat$pricets_fit, frequency = 365, 
                start = c(2015, which(day(ymd("2015-01-01")) == 
                                        training_data_price$day[1])))
#plot of price
autoplot(price_var, main = "VAR prices")
ggtsdisplay(price_var)

consumption_var <- ts(restricted_model$datamat$consumptionts_fit, frequency = 365, 
                start = c(2015, which(day(ymd("2015-01-01")) == 
                                        training_data_price$day[1])))
#plot of consumption
autoplot(consumption_var, main = "VAR prices")
ggtsdisplay(consumption_var)

#plot of original and the fitted
# Convert time series to data frames for ggplot
price_var_df <- data.frame(Date = time(price_var), Fitted = as.numeric(price_var))
price_ts_df <- data.frame(Date = time(price_ts), Actual = as.numeric(price_ts))

# Merge data frames by Date
plot_data <- merge(price_var_df, price_ts_df, by = "Date", all = TRUE)

# Create the plot
ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = Fitted, colour = "Fitted"), size = 1) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
  labs(title = "Comparison of Fitted and Actual Electricity Prices",
       x = "Year",
       y = "Electricity Price",
       colour = "Legend") +
  scale_colour_manual(values = c("Fitted" = "red", "Actual" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Convert time series to data frames for ggplot
consumption_var_df <- data.frame(Date = time(consumption_var), Fitted = as.numeric(consumption_var))
consumption_ts_df <- data.frame(Date = time(consumption_ts), Actual = as.numeric(consumption_ts))

# Merge data frames by Date
plot_data <- merge(consumption_var_df, consumption_ts_df, by = "Date", all = TRUE)

# Create the plot
ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = Fitted, colour = "Fitted"), size = 1) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
  labs(title = "Comparison of Fitted and Actual Electricity Consumption",
       x = "Year",
       y = "Electricity Consumption",
       colour = "Legend") +
  scale_colour_manual(values = c("Fitted" = "red", "Actual" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Checking the residuals of each ts in the var
Acf(Model1$varresult$pricets_fit$residuals)
Acf(Model1$varresult$consumptionts_fit$residuals)
#restricted model
Acf(restricted_model$varresult$pricets_fit$residuals, lag.max = 30)
Acf(restricted_model$varresult$consumptionts_fit$residuals, lag.max = 30)

################################################################################
###
#Impuslse response function
irf_price <- irf(restricted_model, impulse = "pricets_fit", response = "consumptionts_fit", n.ahead = 10, ortho = TRUE,
               cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 100)
plot(irf_price)

irf_consumption <- irf(restricted_model, impulse = "consumptionts_fit", response = "pricets_fit", n.ahead = 10, ortho = TRUE,
                 cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 100)
plot(irf_consumption)
################################################################################
#Making predictions using the VAR(6) model we have constructed
# Forecast future values
forecast_valres <- predict(restricted_model, n.ahead = 366)

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_valres, xlim = c(2019.95, 2020.5), main = "Forecast of future values in")

###
# Transforming the predictions back to look at the predictions compared to observed values
future_t <- 1:366 
future_t <- future_t + 1826

# Create a new data frame for these future time points
future_data <- data.frame(
  t = future_t,
  sin_t_week = sin(future_t * 2 * pi / 7),
  cos_t_week = cos(future_t * 2 * pi / 7),
  sin_t_monthly = sin(future_t * 4 * pi / 30),
  cos_t_monthly = cos(future_t * 4 * pi / 30),
  sin_t_halfyearly = sin(future_t * 2 * pi / 180),
  cos_t_halfyearly = cos(future_t * 2 * pi / 180),
  sin_t_yearly = sin(future_t * 2 * pi / 365),
  cos_t_yearly = cos(future_t * 2 * pi / 365)
)
# Assuming Deterministisk is your linear model
price_st_predict <- predict(st_price, newdata = future_data)
price_prediction <- forecast_valres$fcst$pricets_fit + price_st_predict

# Plotting final forecasts against actual data
plot(testing_data_price$MeanPrice, type = "l", col = "blue", ylim = range(c(testing_data_price$MeanPrice, forecast_valres$fcst$pricets_fit)), main = "Final Forecasts vs Actual Data")
lines(price_prediction[,1], col = "red")
lines(price_prediction[,2], col = "grey")
lines(price_prediction[,3], col = "grey")
legend("topright", legend = c("Actual", "Forecast", "Confidence"), col = c("blue", "red", "grey"), lty = 1)

# For consumption
consumption_st_predict <- predict(st_consumption, newdata = future_data)
consumption_prediction <- forecast_valres$fcst$consumptionts_fit + consumption_st_predict

plot(testing_data_consumption$DailyConsumption, type = "l", col = "blue", ylim = range(c(testing_data_consumption$DailyConsumption, forecast_valres$fcst$consumptionts_fit)), main = "Final Forecasts vs Actual Data")
lines(consumption_prediction[,1], col = "red")
lines(consumption_prediction[,2], col = "grey")
lines(consumption_prediction[,3], col = "grey")
legend("topright", legend = c("Actual", "Forecast", "Confidence"), col = c("blue", "red", "grey"), lty = 1)

################################################################################
################################################################################
#breakout tests
r1 <- breakpoints(price_ts ~ 1)
r12 <- confint(r1)
plot(price_ts, main = "Regime Shifts in Spot Prices", ylab = "Spot prices")
lines(r1, col = "navyblue")
lines(r12, col = "red")
legend("topright", legend = c("Breakpoints", "Confidence Intervals"), 
       col = c("navyblue", "red"), lty = 1, cex = 0.8)

############################################################################################################
############################################################################################################
############################################################################################################
#creating model 2
start_date <- as.Date("2019-01-01") + days(53)
#splitting it up
training_data_price_2 <- subset(daily_price_means, as.Date(start_date) < day & day < as.Date("2020-01-01"))
training_data_consumption_2 <- subset(daily_cons_means, as.Date(start_date) < day & day < as.Date("2020-01-01"))

#making the ts
price_ts_2 <- ts(training_data_price_2$MeanPrice,frequency = 365, 
               start = c(2019, 53))
plot(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2019-2020")
ggtsdisplay(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Spot price from 2019 through 2020")

#Making time series of consumption
consumption_ts_2 <- ts(training_data_consumption_2$DailyConsumption, frequency = 365, 
                     start = c(2019, 53))
plot(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2018-2020")
ggtsdisplay(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption of electricity from 2018 through 2019")

# Applying the function to price and consumption time series
price_ts_2 <- handle_outliers_ts(price_ts_2)
consumption_ts_2 <- handle_outliers_ts(consumption_ts_2)

#setting a time interval, which is just our days in the whole periode
i <- 1:length(training_data_price_2$MeanPrice)

#Constructing the deterministic ts for price 
st_price_2 <- lm(training_data_price_2$MeanPrice ~ i 
                 + sin(i*2*pi/7) + cos(i*2*pi/7) 
                 + sin(i*2*pi/30) + cos(i*2*pi/30) 
                 + sin(i*2*pi/180) + cos(i*2*pi/180)
                 + sin(i*2*pi/365) + cos(i*2*pi/365))
# Now for the consumption
st_consumption_2 <- lm(training_data_consumption_2$DailyConsumption ~ i 
                       + sin(i*2*pi/7) + cos(i*2*pi/7) 
                       + sin(i*2*pi/30) + cos(i*2*pi/30) 
                       + sin(i*2*pi/180) + cos(i*2*pi/180)
                       + sin(i*2*pi/365) + cos(i*2*pi/365))

# Generate fitted values using the linear model
fitted_values_prices_2 <- fitted(st_price_2)
fitted_values_consumption_2 <- fitted(st_consumption_2)

# Subtract the fitted values from price_ts
adjusted_pricets_2 <- price_ts_2 - fitted_values_prices_2
adjusted_consumptionts_2 <- consumption_ts_2 - fitted_values_consumption_2

Acf(adjusted_pricets_2, lag.max = 49)
Acf(adjusted_consumptionts_2, lag.max = 49)

ggtsdisplay(adjusted_pricets_2)
ggtsdisplay(adjusted_consumptionts_2)
#Constructing the SARIMA #(3,1,1)(2,1,1)
price_arima_2 <- Arima(diff(adjusted_pricets_2),
                     order = c(3,1,2),
                     seasonal = list(order = c(1,1,1), period = 7))
checkresiduals(price_arima_2, lag.max = 90)
consumption_arima_2 <- Arima(diff(adjusted_consumptionts_2),
                           order = c(5,1,1),
                           seasonal = list(order = c(1,1,1), period = 7))
checkresiduals(consumption_arima_2)

pricets_fit_2 <- fitted(price_arima_2)
consumptionts_fit_2 <- fitted(consumption_arima_2)

# making the residuals
res_price_2 <- adjusted_pricets_2 - pricets_fit_2
Acf(res_price_2, lag.max = 30)
autoplot(price_ts_2, series = "price_ts") + autolayer(res_price_2, series = "res_price") + 
  autolayer(pricets_fit_2, series = "fitted price")

#for consumption
res_consumption_2 <- adjusted_consumptionts_2 - consumptionts_fit_2
Acf(res_consumption_2, lag.max = 180)
autoplot(consumption_ts_2, series = "consumption_ts") + autolayer(res_consumption_2, series = "res_cons") + 
  autolayer(consumptionts_fit_2, series = "fitted consumption")
#making the ggtsdisplays
ggtsdisplay(res_price_2, main = "Cleaned spot price model")
ggtsdisplay(res_consumption_2, main = "Cleaned consumption model")

################################################################################
# Constructing the VAR
# Firstly we test the number of p
# crafting the collected ts to make the var model
colle_ts_2 <- cbind(pricets_fit_2 , consumptionts_fit_2)

#Selcting the number of lags to include
aic_values <- numeric(15)
bic_values <- numeric(15)

for (i in 1:20) {
  model2 <- VAR(colle_ts_2, p=i, type="const")
  aic_values[i] <- AIC(model2)
  bic_values[i] <- BIC(model2)
}

# Plot AIC and BIC values to visualize the optimal lag
plot(aic_values, type='b', col='blue', xlab='Number of Lags', ylab='AIC', main='AIC and BIC by Lag')
points(bic_values, type='b', col='red')
legend("topright", legend=c("AIC", "BIC"), col=c("blue", "red"), pch=1)


Model2 <- VAR(colle_ts_2, p = 7, type = "const", season = NULL, exog = NULL)
restricted_model_2 <- restrict(Model2)

Acf(restricted_model_2$varresult$pricets_fit$residuals)
Acf(restricted_model_2$varresult$consumptionts_fit$residuals, lag.max = 60)

##
#predictions
forecast_valres_2 <- predict(restricted_model_2, n.ahead = 366)

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_valres_2, xlim = c(2019.95, 2020.05), main = "New forecast of future values in 2020")
###
# Transforming the predictions back to look at the predictions compared to observed values
future_i_2 <- 1:366 
future_i_2 <- future_i_2 + 311

# Create a new data frame for these future time points
future_data_2 <- data.frame(
  i = future_i_2,
  sin_i_week = sin(future_i_2 * 2 * pi / 7),
  cos_i_week = cos(future_i_2 * 2 * pi / 7),
  sin_i_monthly = sin(future_i_2 * 4 * pi / 30),
  cos_i_monthly = cos(future_i_2 * 4 * pi / 30),
  sin_i_halfyearly = sin(future_i_2 * 2 * pi / 180),
  cos_i_halfyearly = cos(future_i_2 * 2 * pi / 180),
  sin_i_yearly = sin(future_i_2 * 2 * pi / 365),
  cos_i_yearly = cos(future_i_2 * 2 * pi / 365)
)
# making the deterministic part
price_st_predict_2 <- predict(st_price_2, newdata = future_data_2)
price_prediction_2 <- forecast_valres_2$fcst$pricets_fit + price_st_predict_2

# Plotting final forecasts against actual data
plot(testing_data_price$MeanPrice, type = "l", col = "blue", ylim = range(c(testing_data_price$MeanPrice, forecast_valres$fcst$pricets_fit)), main = "Final Forecasts vs Actual Data")
lines(price_prediction[,1], col = "red")
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)

# For consumption
consumption_st_predict <- predict(st_consumption, newdata = future_data)
consumption_prediction <- forecast_valres$fcst$consumptionts_fit + consumption_st_predict

plot(testing_data_consumption$DailyConsumption, type = "l", col = "blue", ylim = range(c(testing_data_consumption$DailyConsumption, forecast_valres$fcst$consumptionts_fit)), main = "Final Forecasts vs Actual Data")
lines(consumption_prediction[,1], col = "red")
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)








price_var_2 <- ts(restricted_model_2$datamat$pricets_fit_2, frequency = 365, 
                  start = c(2019, 53))
#plot of price
autoplot(price_var_2, main = "VAR prices")

consumption_var_2 <- ts(restricted_model_2$datamat$consumptionts_fit_2, frequency = 365, 
                        start = c(2019, 53))
#plot of consumption
autoplot(consumption_var_2, main = "VAR consumption")

#plot of original and the fitted
# Convert time series to data frames for ggplot
price_var_df_2 <- data.frame(Date = time(price_var_2), Fitted = as.numeric(price_var_2))
price_ts_df_2 <- data.frame(Date = time(price_ts_2), Actual = as.numeric(price_ts_2))

# Merge data frames by Date
plot_data_2 <- merge(price_var_df_2, price_ts_df_2, by = "Date", all = TRUE)

# Create the plot
ggplot(data = plot_data_2, aes(x = Date)) +
  geom_line(aes(y = Fitted, colour = "Fitted"), size = 1) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
  labs(title = "Comparison of Fitted and Actual Electricity Prices",
       x = "Year",
       y = "Electricity Price",
       colour = "Legend") +
  scale_colour_manual(values = c("Fitted" = "red", "Actual" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Convert time series to data frames for ggplot
consumption_var_df <- data.frame(Date = time(consumption_var), Fitted = as.numeric(consumption_var))
consumption_ts_df <- data.frame(Date = time(consumption_ts), Actual = as.numeric(consumption_ts))

# Merge data frames by Date
plot_data <- merge(consumption_var_df, consumption_ts_df, by = "Date", all = TRUE)

# Create the plot
ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = Fitted, colour = "Fitted"), size = 1) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
  labs(title = "Comparison of Fitted and Actual Electricity Consumption",
       x = "Year",
       y = "Electricity Consumption",
       colour = "Legend") +
  scale_colour_manual(values = c("Fitted" = "red", "Actual" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
