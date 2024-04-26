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
#library(fpp2)

# Fetch data for elctiricity prices
elecprices <- GET("https://api.energidataservice.dk/dataset/Elspotprices?limit=1000000&start=2015-01-01T00:00&end=2019-01-01T00:00&filter={\"PriceArea\":\"DK1\"}")
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

#We can now do the same for consumption, where we will end up with a dataframe with both data
#Fetching consumtion datasheet
consump <- GET("https://api.energidataservice.dk/dataset/ProductionConsumptionSettlement?limit=1000000&start=2015-01-01T00:00&end=2019-01-01T00:00&filter={\"PriceArea\":\"DK1\"}")
consumpcontent <- content(consump, as = "text")

constrjson <- fromJSON(consumpcontent)
Consumption <- constrjson$records

Consumption$HourDK <- ymd_hms(Consumption$HourDK)
Consumption$day <- floor_date(Consumption$HourDK, "day")

daily_cons_means <- Consumption %>%
  group_by(day) %>%
  summarise(DailyConsumption = mean(GrossConsumptionMWh, na.rm = TRUE))

# Creating a scatterplot with both df to be done
#ggplot()

#########################################################################
# Now, daily_means is our new dataframe with the mean price for each week
#We can now create an initial time series to check for how we want to proceed.
#Plotting the ts with dates in the x-axis  
price_ts <- ts(daily_price_means$MeanPrice,frequency = 365, 
                             start = c(2015, which(day(ymd("2015-01-01")) == 
                                                     daily_price_means$day[1])))
plot(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2015-2019")
ggtsdisplay(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Spot prices, 2015-2019")

#Making time series of consumption
consumption_ts <- ts(daily_cons_means$DailyConsumption, frequency = 365, 
               start = c(2015, which(day(ymd("2015-01-01")) == 
                                       daily_cons_means$day[1])))
plot(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2019")
ggtsdisplay(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2019")

#constructing ts of logged consumption 
logged_consumption_ts <- log(consumption_ts)
ggtsdisplay(logged_consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2019")

#autoplot the ts together
autoplot(cbind(price_ts, consumption_ts))

############################################################################
#creating the ACF to find what should be done to the time series to achieve stationarity
Acf(price_ts, lag.max = 30)
Acf(consumption_ts, lag.max = 30)
Acf(logged_consumption_ts, lag.max = 30)

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

####################################
# Decomposition of each ts
# Decomposition of price time series
decomposition_price <- stl(price_ts, s.window = "periodic")
autoplot(decomposition_price, ts.colour = "darkblue") + ggtitle("Decomposition of Spot Prices")

# Extract residuals
residuals_pricets <- decomposition_price$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_pricets, lag.max = 30)
Pacf(residuals_pricets, lag.max = 30)
ggtsdisplay(residuals_pricets)

###
#Decomposition of consumption
de_consump <- stl(consumption_ts, s.window = "periodic")
autoplot(de_consump, ts.colour = "darkblue") + ggtitle("Decomposition of Gross Consumption")

# Extract residuals
residuals_cons <- de_consump$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_cons, lag.max = 30)
Pacf(residuals_cons, lag.max = 30)
ggtsdisplay(residuals_cons, lag.max = 90)

###
#Decomposition of logged consumption
de_logcons <- stl(logged_consumption_ts, s.window = "periodic")
autoplot(de_logcons, ts.colour = "darkblue") + ggtitle("Decomposition of Gross Consumption")

# Extract residuals
residuals_logcons <- de_logcons$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_logcons, lag.max = 20)
ggtsdisplay(residuals_logcons)

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

###
#for consumption
ndiffs(consumption_ts)
nsdiffs(consumption_ts)
#tests reveal, differencing wont help

###
# For log consumption
ndiffs(logged_consumption_ts)
nsdiffs(logged_consumption_ts)
#Same as normal consumption

#################################
#making the time series as a linear regression where we have the time series in 2 components
#X_t=s_t+Y_t, one is deterministic and one is stochastic

#setting a time interval, which is just our days in the whole periode
t <- 1:length(daily_price_means$MeanPrice)

#Constructing the deterministic ts for price 
st_price <- lm(daily_price_means$MeanPrice ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/30) 
                + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) + sin(t*2*pi/180) +
                  sin(t*2*pi/365) + cos(t*2*pi/365))
# Now for the consumption
st_consumption <- lm(daily_cons_means$DailyConsumption ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/30) 
                     + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) + sin(t*2*pi/180) +
                       sin(t*2*pi/365) + cos(t*2*pi/365))

# Generate predicted values using the linear model
predicted_values_prices <- fitted(st_price)
predicted_values_consumption <- fitted(st_consumption)


# Convert values into a time series object
st_pricets <- ts(predicted_values_prices,frequency = 365, 
                 start = c(2015, which(day(ymd("2015-01-01")) == 
                                         daily_price_means$day[1])))

# Convert values into a time series object
st_consts <- ts(predicted_values_consumption,frequency = 365, 
                 start = c(2015, which(day(ymd("2015-01-01")) == 
                                         daily_price_means$day[1])))

# Plotting both series aginst original
ts.plot(st_pricets, price_ts, gpars = list(col = c("red", "blue")))
ts.plot(st_consts, consumption_ts, gpars = list(col = c("red", "blue")))

# Display them individualy with acf and pacf
ggtsdisplay(st_pricets)
ggtsdisplay(st_consts)

Acf(st_consts, lag.max = 90)

# Subtract the predicted values from price_ts
adjusted_pricets <- price_ts - predicted_values_prices
adjusted_consumptionts <- consumption_ts - predicted_values_consumption

ts.plot(st_pricets, price_ts, adjusted_pricets, gpars = list(col = c("red", "blue", "black")))
ts.plot(st_consts, consumption_ts, adjusted_consumptionts, gpars = list(col = c("red", "blue", "black")))

autoplot(consumption_ts, series = "cons", ylab = "hej", colour = "black") + 
  autolayer(adjusted_consumptionts, series = "adjcons") + 
  autolayer(st_consts, series = "adjusted")
  

# Display using ggplots
ggtsdisplay(adjusted_pricets)
ggtsdisplay(adjusted_consumptionts)

# Perform ADF test on the adjusted models
adf_test_pricets <- ur.df(adjusted_pricets, type = "drift", lags = 1)
summary(adf_test_pricets)

# Tests for which model desribes the season and trend, run at ur own discretion
test_price <- auto.arima(adjusted_pricets, stepwise = FALSE, approximation = FALSE, D = 1)
test_consump <- auto.arima(adjusted_consumptionts, stepwise = FALSE, approximation = FALSE, D = 1)

################################################################################
# Creating the optimal ts given the results from the previous tests

###
#We start with the price
price_fit <- Arima(adjusted_pricets,
                   order = c(3,1,1),
                   seasonal = c(0,1,0))
checkresiduals(price_fit)

price_fit %>%
  forecast(h = 12) %>%
  autoplot()

# Extract residuals from ARIMA model
residuals <- residuals(price_fit)

# Perform ADF test on the residuals
adf_test <- ur.df(residuals, type = "drift", lags = 1)
summary(adf_test)

###
# Then for consumption
consumption_fit <- Arima(adjusted_consumptionts,
                   order = c(5,0,0),
                   seasonal = c(0,1,0))
checkresiduals(consumption_fit)

consumption_fit %>%
  forecast(h = 12) %>%
  autoplot()

# Extract residuals from ARIMA model
residuals_consumption <- residuals(consumption_fit)

# Perform ADF test on the residuals
adf_test_consumption <- ur.df(residuals_consumption, type = "drift", lags = 1)
summary(adf_test_consumption)

ggtsdisplay(residuals_consumption)

################################################################################
# Constructing the VAR
# Firstly we test the number of p
pricets_fit <- fitted(price_fit)
consumptionts_fit <- fitted(consumption_fit)

colle_ts <- cbind(pricets_fit , consumptionts_fit)

#Selcting the number of p to include
lag_select <- VARselect(colle_ts, lag.max = 365, type = "const")
lag_select$selection

Model1 <- VAR(colle_ts, p = 29, type = "const", season = NULL, exog = NULL)
summary(Model1)

res_var <- residuals(Model1)

adf_test_var <- ur.df(res_var[,1], type = "none", lags = 29)
adf.test(res_var[,1])

summary(adf_test_var)

################################################################################
#Making predictions using the VAR(29) model we have constructed

# Forecast future values
forecast_values <- predict(Model1, n.ahead = 30)  # Change 10 to the number of periods you want to forecast ahead

# Plot the forecasted values, zooming in on the last part
plot(forecast_values, xlim = c(2018.8, 2019.2))  
autoplot(forecast_values, xlim = c(2018.9, 2019.1))

################################################################################
# Fething data ones again cus we are rookies at programming.
# Fetch data for elctiricity prices
elecprices_fut <- GET("https://api.energidataservice.dk/dataset/Elspotprices?limit=1000000&start=2015-01-01T00:00&end=2020-01-01T00:00&filter={\"PriceArea\":\"DK1\"}")
elpriccontent_fut <- content(elecprices_fut, as = "text")

# Parse JSON content
strjson_fut <- fromJSON(elpriccontent_fut)

# Convert to data frame
Elpriser_fut <- strjson_fut$records

# Adjusting the hours to represent readable dates and hours.
Elpriser_fut$HourDK <- ymd_hms(Elpriser_fut$HourDK)

# Add a column that indicates the daily number
Elpriser_fut$day <- floor_date(Elpriser_fut$HourDK, "day")

# Group by days and calculate the daily mean
daily_price_means_fut <- Elpriser_fut %>%
  group_by(day) %>%
  summarise(MeanPrice = mean(SpotPriceDKK, na.rm = TRUE))

#We can now do the same for consumption, where we will end up with a dataframe with both data
#Fetching consumtion datasheet
consump_fut <- GET("https://api.energidataservice.dk/dataset/ProductionConsumptionSettlement?limit=1000000&start=2015-01-01T00:00&end=2020-01-01T00:00&filter={\"PriceArea\":\"DK1\"}")
consumpcontent_fut <- content(consump_fut, as = "text")

constrjson_fut <- fromJSON(consumpcontent_fut)
Consumption_fut <- constrjson_fut$records

Consumption_fut$HourDK <- ymd_hms(Consumption_fut$HourDK)
Consumption_fut$day <- floor_date(Consumption_fut$HourDK, "day")

daily_cons_means_fut <- Consumption_fut %>%
  group_by(day) %>%
  summarise(DailyConsumption = mean(GrossConsumptionMWh, na.rm = TRUE))

###
daily_price_means_fut_ts <- ts(daily_price_means_fut$MeanPrice,frequency = 365, 
                               start = c(2015, which(day(ymd("2015-01-01")) == 
                                                       daily_price_means_fut$day[1])))

daily_cons_means_fut_ts <- ts(daily_cons_means_fut$DailyConsumption, frequency = 365, 
                              start = c(2015, which(day(ymd("2015-01-01")) == 
                                                      daily_cons_means_fut$day[1])))


observed <- cbind(daily_price_means_fut_ts, daily_cons_means_fut_ts)
obs_lag_select <- VARselect(observed, lag.max = 365, type = "const")
obs_lag_select$selection


plot(daily_price_means_fut, xlim = c(2018, 2019))

#observed <- observed[, -which(names(observed) == "day")[2]]

