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


# Creating a scatterplot with both df to be done
#ggplot()

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

#autoplot the ts together
autoplot(cbind(price_ts, consumption_ts))


stability()

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

#anden test type
kpss.test(price_ts, null = "Level")
kpss.test(price_ts, null = "Trend")

#For den færdige test
kpss.test(pricets_fit, null = "Level")
kpss.test(pricets_fit, null = "Trend")
kpss.test(pricets_fit, null = c("Level", "Trend"))

kpss.test(consumptionts_fit, null = "Level")

####################################
# Decomposition of each ts
# Decomposition of price time series
decomposition_price <- stl(price_ts, s.window = "periodic")
autoplot(decomposition_price, ts.colour = "darkblue") + ggtitle("Decomposition of Spot Prices")

# Extract residuals
residuals_pricets <- decomposition_price$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_pricets, lag.max = 90)
Pacf(residuals_pricets, lag.max = 30)
ggtsdisplay(residuals_pricets)

###
#Decomposition of consumption
de_consump <- stl(consumption_ts, s.window = "periodic")
autoplot(de_consump, ts.colour = "darkblue") + ggtitle("Decomposition of Gross Consumption")

# Extract residuals
residuals_cons <- de_consump$time.series[, "remainder"]

#MSTL
msts <- msts(price_ts, seasonal.periods = c(7, 365))

mstl <- mstl(msts)

autoplot(mstl)

cmsts <- msts(consumption_ts, seasonal.periods = c(7, 365))
cmstl <- mstl(cmsts)
autoplot(cmstl)

#######################################################################
#deseasonalize the ts' first with price
ndiffs(price_ts)
nsdiffs(price_ts)
#Reveal that differencing 1 time might be a good idea.

diff_price_ts <- diff(price_ts)
ggtsdisplay(diff_price_ts)

adf_test <- ur.df(diff_price_ts)
summary(adf_test)

Acf(diff_price_ts)
Pacf(diff_price_ts)
#tests reveal, differencing wont help

#################################
#making the time series as a linear regression where we have the time series in 2 components
#X_t=s_t+Y_t, one is deterministic and one is stochastic

#setting a time interval, which is just our days in the whole periode
t <- 1:length(training_data_price$MeanPrice)

#Constructing the deterministic ts for price 
st_price <- lm(training_data_price$MeanPrice ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/30) 
               + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) + sin(t*2*pi/180) +
                 sin(t*2*pi/365) + cos(t*2*pi/365))
# Now for the consumption
st_consumption <- lm(training_data_consumption$DailyConsumption ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/30) 
                     + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) + sin(t*2*pi/180) +
                       sin(t*2*pi/365) + cos(t*2*pi/365))

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
ggtsdisplay(adjusted_pricets)
ggtsdisplay(adjusted_consumptionts)

# Perform ADF test on the adjusted models
adf_test_pricets <- ur.df(adjusted_pricets, type = "drift", lags = 1)
summary(adf_test_pricets)

# Tests for which model desribes the season and trend, RUN AT YOUR own discretion
#test_price <- auto.arima(adjusted_pricets, stepwise = FALSE, approximation = FALSE, D = 1)
#test_consump <- auto.arima(adjusted_consumptionts, stepwise = FALSE, approximation = FALSE, D = 1)

################################################################################
# Creating the optimal ts given the results from the previous tests
###
#We start with the price
price_arima <- Arima(adjusted_pricets,
                     order = c(3,1,1),
                     seasonal = list(order = c(0,1,0), period = 7))
checkresiduals(price_arima)

# Extract residuals from ARIMA model
residuals <- residuals(price_arima)

# Perform ADF test on the residuals
adf_test <- ur.df(residuals, type = "drift", lags = 1)
summary(adf_test)

###
# Then for consumption
consumption_arima <- Arima(adjusted_consumptionts,
                           order = c(5,0,0),
                           seasonal = list(order = c(0,1,0), period = 7))
checkresiduals(consumption_arima)

# Extract residuals from ARIMA model
residuals_consumption <- residuals(consumption_arima)

# Perform ADF test on the residuals
adf_test_consumption <- ur.df(residuals_consumption, type = "drift", lags = 1)
summary(adf_test_consumption)

ggtsdisplay(residuals_consumption)

checkresiduals(price_arima)
checkresiduals(consumption_arima)

pricets_fit <- fitted(price_arima)
consumptionts_fit <- fitted(consumption_arima)

# making the residuals
res_price <- adjusted_pricets - pricets_fit
Acf(res_price, lag.max = 30)
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

#Selcting the number of p to include
lag_select <- VARselect(colle_ts, lag.max = 365, type = "const")
lag_select$selection

Model1 <- VAR(colle_ts, p = 7, type = "const", season = NULL, exog = NULL)
restricted_model <- restrict(Model1)

#summary
summary(Model1)
summary(restricted_model)

res_var <- residuals(Model1)
Acf(res_var)

#Checking the residuals of each ts in the var
Acf(Model1$varresult$pricets_fit$residuals)
Acf(Model1$varresult$consumptionts_fit$residuals)
#restricted model
Acf(restricted_model$varresult$pricets_fit$residuals, lag.max = 90)
Acf(restricted_model$varresult$consumptionts_fit$residuals, lag.max = 90)

#Checking nomality of the residuals
ggqqplot(Model1$varresult$pricets_fit$residuals)
ggqqplot(Model1$varresult$consumptionts_fit$residuals)
#restricted
ggqqplot(restricted_model$varresult$pricets_fit$residuals)
ggqqplot(restricted_model$varresult$consumptionts_fit$residuals)
#shapiro test
shapiro.test(Model1$varresult$pricets_fit$residuals)
shapiro.test(restricted_model$varresult$consumptionts_fit$residuals)

Acf(Model1$varresult$pricets_fit$residuals)
Acf(Model1$varresult$consumptionts_fit$residuals)


################################################################################
#Making predictions using the VAR(7) model we have constructed
# Forecast future values
forecast_values <- predict(Model1, n.ahead = 365)  

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_values, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")

# Assuming 'Model1' is your fitted VAR model
# 'n.ahead' is the number of periods you want to forecast 
n.ahead <- nrow(testing_data_price)

# Generate predictions
predictions <- predict(Model1, n.ahead=n.ahead)

# Accessing the forecast for the first variable (price)
# The $fcst element of 'predictions' should be a list with named elements for each variable
forecasted_prices <- predictions$fcst$pricets_fit[, "fcst"]

# Accessing the forecast for the second variable (consumption)
forecasted_consumption <- predictions$fcst$consumptionts_fit[, "fcst"]

# Now plot the actual and forecasted prices
plot(testing_data_price$day, testing_data_price$MeanPrice, type='l', col='blue', ylim=range(c(forecasted_prices, testing_data_price$MeanPrice)))
lines(testing_data_price$day, forecasted_prices, col='red')

# Now plot the actual and forecasted prices
plot(testing_data_consumption$day, testing_data_consumption$DailyConsumption, type='l', col='blue', ylim=range(c(forecasted_consumption, testing_data_consumption$DailyConsumption)))
lines(testing_data_consumption$day, forecasted_consumption, col='red')



chow_test <- chow.test(Model1, SB = 1460)
summary(chow_test)

breakpoint_date <- as.Date("2015-01-01") + days(1460) 
breakpoint_date


x1 <- stability(Model1, type = "mv-chow-test")
plot(x1)
ny <- chow.test(x1)
summary(ny)







# Assuming 'price_ts' is your time series object
# Calculate the correct index positions directly, assuming daily data from the start date
breakpoint_index <- 1356  # Observation for the breakpoint
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
training_data_price_2 <- subset(daily_price_means, as.Date("2018-12-31") < day & day < as.Date("2020-01-01"))
training_data_consumption_2 <- subset(daily_cons_means, as.Date("2018-12-31") < day & day < as.Date("2020-01-01"))

#making the ts
price_ts_2 <- ts(training_data_price_2$MeanPrice,frequency = 365, 
               start = c(2018, which(day(ymd("2018-12-31")) == 
                                       training_data_price_2$day[1])))
plot(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2019-2020")
ggtsdisplay(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Spot price from 2019 through 2020")

#Making time series of consumption
consumption_ts_2 <- ts(training_data_consumption_2$DailyConsumption, frequency = 365, 
                     start = c(2018, which(day(ymd("2018-12-31")) == 
                                             training_data_consumption_2$day[1])))
plot(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2018-2020")
ggtsdisplay(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption of electricity from 2018 through 2019")

#setting a time interval, which is just our days in the whole periode
i <- 1:length(training_data_price_2$MeanPrice)

#Constructing the deterministic ts for price 
st_price_2 <- lm(training_data_price_2$MeanPrice ~ i + sin(i*2*pi/7) + cos(i*2*pi/7) + sin(i*2*pi/30) 
               + cos(i*2*pi/30) + sin(i*2*pi/90) + cos(i*2*pi/90) + sin(i*2*pi/180) +
                 sin(i*2*pi/365) + cos(i*2*pi/365))
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

ggtsdisplay(adjusted_pricets_2)
ggtsdisplay(adjusted_consumptionts_2)
#Constructing the SARIMA
price_arima_2 <- Arima(adjusted_pricets_2,
                     order = c(3,1,1),
                     seasonal = list(order = c(0,1,0), period = 7))
checkresiduals(price_arima_2)
consumption_arima_2 <- Arima(adjusted_consumptionts_2,
                           order = c(5,0,0),
                           seasonal = list(order = c(0,1,0), period = 7))
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
Acf(res_consumption_2, lag.max = 30)
autoplot(consumption_ts_2, series = "consumption_ts") + autolayer(res_consumption_2, series = "res_cons") + 
  autolayer(consumptionts_fit_2, series = "fitted consumption")
#making the ggtsdisplays
ggtsdisplay(res_price_2, main = "Cleaned spot price model")
ggtsdisplay(res_consumption_2, main = "Cleaned consumption model")

################################################################################
# Constructing the VAR
# Firstly we test the number of p
#crafting the collected ts to make the var model
colle_ts_2 <- cbind(pricets_fit_2 , consumptionts_fit_2)

#Selcting the number of p to include
lag_select_2 <- VARselect(colle_ts_2, type = "const")
lag_select_2$selection

Model2 <- VAR(colle_ts, p = 10, type = "const", season = NULL, exog = NULL)
restricted_model_2 <- restrict(Model2)

Acf(restricted_model_2$varresult$pricets_fit$residuals)
Acf(restricted_model_2$varresult$consumptionts_fit$residuals)
##
#predictions
forecast_values_2 <- predict(Model2, n.ahead = 90)  
forecast_resval_2 <- predict(restricted_model_2, n.ahead = 90)

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_values_2, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")
autoplot(forecast_resval_2, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")
