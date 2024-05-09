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

# Plotting the original and cleaned time series for comparison
par(mfrow=c(2,1))
plot(price_ts, main = "Original Price Time Series")
plot(ts(price_ts_cleaned, frequency = 365, start = c(2015,1)), main = "Cleaned Price Time Series")

plot(consumption_ts, main = "Original Consumption Time Series")
plot(ts(consumption_ts_cleaned, frequency = 365, start = c(2015,1)), main = "Cleaned Consumption Time Series")


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
st_price <- lm(training_data_price$MeanPrice ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/14) + 
                 cos(t*2*pi/14) + sin(t*2*pi/30) + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) + 
                 sin(t*2*pi/180) + cos(t*2*pi/180) + sin(t*2*pi/365) + cos(t*2*pi/365) + + sin(t*2*pi/3.5) +
                 cos(t*2*pi/3.5) + sin(t*2*pi/2) + cos(t*2*pi/2))

# Now for the consumption
st_consumption <- lm(training_data_consumption$DailyConsumption ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/14) + 
                       cos(t*2*pi/14) +sin(t*2*pi/30) + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) +
                       sin(t*2*pi/180) + cos(t*2*pi/180) + sin(t*2*pi/365) + cos(t*2*pi/365))

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
                     order = c(3,1,1),
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

#running kpss test
#for original ts
kpss.test(price_ts, null = "Level")
kpss.test(price_ts, null = "Trend")
kpss.test(consumption_ts, null = "Level")


#For the final ts
kpss.test(pricets_fit, null = "Level")
kpss.test(pricets_fit, null = "Trend")
kpss.test(pricets_fit, null = c("Level", "Trend"))
kpss.test(consumptionts_fit, null = "Level")

kpss.test(res_consumption, null = "Trend")

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

consumption_var <- ts(restricted_model$datamat$consumptionts_fit, frequency = 365, 
                start = c(2015, which(day(ymd("2015-01-01")) == 
                                        training_data_price$day[1])))
#plot of consumption
autoplot(consumption_var, main = "VAR prices")

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

#Serial correlation
serial_price <- serial.test(restricted_model, lags.pt = 12, type = "PT.asymptotic")
serial_price


################################################################################
###
#Impuslse response function
irf_price <- irf(restricted_model, impulse = "pricets_fit", response = "consumptionts_fit", n.ahead = 40, ortho = TRUE,
               cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 100)
plot(irf_price)

irf_consumption <- irf(restricted_model, impulse = "consumptionts_fit", response = "pricets_fit", n.ahead = 40, ortho = TRUE,
                 cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 100)
plot(irf_consumption)
################################################################################
#Making predictions using the VAR(6) model we have constructed
# Forecast future values
forecast_values <- forecast(Model1, h = 90, level = 95)  
forecast_valres <- predict(restricted_model, n.ahead = 90)

predict <- forecast(restricted_model, h = 14, level = 95)
predict_2 <- xts(predict$forecast$pricets_fit$mean, order.by = as.Date("2020-01-01") + 1:14)
last_known_price <- training_data_price[nrow(training_data_price), 'MeanPrice']
predpred <- cumsum(c(last_known_price, predict_2))

forecasted_prices_original <- cumsum(c(last_known_price, forecasted_prices_stochastic))

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_values, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")
autoplot(forecast_valres, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")

###
# Assuming your last actual price value is stored in `last_actual_price` and the last 7 price values in `last_season_price_values`

# We need to initialize the undifferencing
last_actual_price <- tail(training_data_price$MeanPrice, 1)
last_season_price_values <- tail(training_data_price$MeanPrice, 7)

# Forecasted values from your VAR output (assuming 'fcst' column is extracted)
price_forecasts <- forecast_valres$fcst$pricets_fit[, "fcst"]
undiff_prices <- cumsum(c(last_actual_price, price_forecasts))[-1]

###
#consumption
# Assuming the last 7 consumption values
last_season_consumption_values <- tail(training_data_consumption$DailyConsumption, 7)

undifference_consumption <- function(fcst, last_season_values) {
  n <- length(fcst)
  undiff_values <- numeric(n)
  undiff_values[1] <- fcst[1] + last_season_values[1]
  
  for (i in 2:n) {
    undiff_values[i] <- fcst[i] + last_season_values[i %% 7 + 1]
  }
  
  return(undiff_values)
}

# Forecasted values from your VAR output for consumption
consumption_forecasts <- forecast_valres$fcst$consumptionts_fit[, "fcst"]
undiff_consumption <- undifference_consumption(consumption_forecasts, last_season_consumption_values)

###
# adding back the deterministic parts
# Total length of the time series including forecast period
total_length <- length(training_data_price$MeanPrice) + length(undiff_prices)

# Create a new time variable for the extended period
t_extended <- 1:total_length

# Evaluate the deterministic components over the entire period
det_price <- predict(st_price, newdata = data.frame(t = t_extended))
det_consumption <- predict(st_consumption, newdata = data.frame(t = t_extended))

# Now det_price and det_consumption contain the deterministic part for both training and forecast periods
# Assuming the forecast length is the same as the length of your undifferenced forecasts
forecast_length <- length(undiff_prices)  # Same should be true for undiff_consumption

# Extract the deterministic parts for the forecast period
det_price_forecast <- tail(det_price, forecast_length)
det_consumption_forecast <- tail(det_consumption, forecast_length)

# Final forecasts with deterministic components added back
final_forecast_prices <- undiff_prices + det_price_forecast
final_forecast_consumption <- undiff_consumption + det_consumption_forecast

################################################################################
###
# Diagnosing the errors with the predictions.
# Assuming you have 't_extended' that includes both training and forecasting periods
all_det_price <- predict(st_price, newdata = data.frame(t = t_extended))

# Plot all deterministic values
plot(all_det_price, type = "l", col = "blue", main = "Complete Deterministic Component of Price")
points(1827:length(all_det_price), all_det_price[1827:length(all_det_price)], col = "red", type = "l")
legend("topright", legend = c("Fitted", "Forecasted"), col = c("blue", "red"), lty = 1)

# Calculate and plot the final forecasts
final_forecasts <- undiff_prices + det_price_forecast

# Plotting final forecasts against actual data
plot(testing_data_price$MeanPrice, type = "l", col = "blue", ylim = range(c(testing_data_price$MeanPrice, final_forecasts)), main = "Final Forecasts vs Actual Data")
lines(final_forecasts, col = "red")
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)

# Calculate error metrics
actuals <- testing_data_price$MeanPrice
forecasts <- final_forecasts[1:length(actuals)]  # Ensure the lengths are the same
errors <- actuals - forecasts
RMSE <- sqrt(mean(errors^2))
MAE <- mean(abs(errors))
MAPE <- mean(abs(errors/actuals))

# Print the error metrics
print(paste("RMSE:", RMSE))
print(paste("MAE:", MAE))
print(paste("MAPE:", MAPE))


################################################################################
################################################################################
#calculating the breakpoint and splitpoint for constructing second model
chow_test <- chow.test(price_ts, SB = 1514)
summary(chow_test)

breakpoint_date <- as.Date("2015-01-01") + days(1514) 
breakpoint_date

#breakout tests
r1 <- breakpoints(price_ts ~ 1)
r12 <- confint(r1)
plot(price_ts)
lines(r1)
lines(r12)

x1 <- stability(price_ts, type = "mv-chow-test")
plot(x1)
ny <- chow.test(x1)
summary(ny)

# Calculate the correct index positions directly, assuming daily data from the start date
breakpoint_index <- 1348  # Observation for the breakpoint
splitpoint_index <- 1416  # Observation for the sample split

# Create the plot with vertical lines
ggplot(data = as.data.frame(price_ts), aes(x = seq_along(price_ts), y = price_ts)) +
  geom_line() +  # Plot the time series as a line plot
  geom_vline(xintercept = breakpoint_index, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = splitpoint_index, color = "blue", linetype = "dashed", size = 1) +
  xlim(c(1000, 1800)) + 
  labs(title = "Price Time Series with Structural Breaks", x = "Time", y = "Price") +
  theme_minimal()

# Create the plot with vertical lines for consumption
ggplot(data = as.data.frame(consumption_ts), aes(x = seq_along(consumption_ts), y = consumption_ts)) +
  geom_line() +  # Plot the time series as a line plot
  geom_vline(xintercept = breakpoint_index, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = splitpoint_index, color = "blue", linetype = "dashed", size = 1) +
  xlim(c(1000, 1800)) +
  labs(title = "Consumption Time Series with Structural Breaks", x = "Time", y = "Price") +
  theme_minimal()

############################################################################################################
############################################################################################################
############################################################################################################
#creating model 2
#splitting it up
training_data_price_2 <- subset(daily_price_means, as.Date("2019-02-23") < day & day < as.Date("2020-01-01"))
training_data_consumption_2 <- subset(daily_cons_means, as.Date("2019-02-23") < day & day < as.Date("2020-01-01"))

start_date <- as.Date("2019-01-01") + days(53)

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

#setting a time interval, which is just our days in the whole periode
i <- 1:length(training_data_price_2$MeanPrice)

#Constructing the deterministic ts for price 
st_price_2 <- lm(training_data_price_2$MeanPrice ~ i + sin(i*2*pi/7) + cos(i*2*pi/7) + sin(i*2*pi/30) 
               + cos(i*2*pi/30) + sin(i*2*pi/90) + cos(i*2*pi/90) + sin(i*2*pi/180) +
                 sin(i*2*pi/365) + cos(i*2*pi/365) + sin(i*2*pi/3.5) +
                 cos(i*2*pi/3.5) + sin(i*2*pi/2) + cos(i*2*pi/2) + sin(i*2*pi/14) +
                 cos(i*2*pi/14))
# Now for the consumption
st_consumption_2 <- lm(training_data_consumption_2$DailyConsumption ~ i + sin(i*2*pi/7) + cos(i*2*pi/7) + sin(i*2*pi/30) 
                     + cos(i*2*pi/30) + sin(i*2*pi/90) + cos(i*2*pi/90) + sin(i*2*pi/180) +
                       sin(i*2*pi/365) + cos(i*2*pi/365))

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
                           order = c(5,1,0),
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

#Serial correlation
serial_price_2 <- serial.test(restricted_model_2, lags.pt = 12, type = "PT.asymptotic")
serial_price_2


##
#predictions
forecast_values_2 <- predict(Model2, n.ahead = 90)  
forecast_resval_2 <- predict(restricted_model_2, n.ahead = 90)

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_values_2, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")
autoplot(forecast_resval_2, xlim = c(2019.95, 2020.05), main = "New forecast of future values in 2020")


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
