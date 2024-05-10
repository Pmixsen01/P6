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

# Fetch data for electricity prices
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
#Fetching consumption datasheet
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
# Daily_means is our new dataframe with the mean price/connsumption for each week
#We can now create an initial time series to check for how we want to proceed.
price_ts <- ts(training_data_price$MeanPrice,frequency = 365, 
               start = c(2015, which(day(ymd("2015-01-01")) == 
                                       training_data_price$day[1])))
#Making time series of consumption
consumption_ts <- ts(training_data_consumption$DailyConsumption, frequency = 365, 
                     start = c(2015, which(day(ymd("2015-01-01")) == 
                                             training_data_consumption$day[1])))

########################
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

cmsts <- msts(consumption_ts, seasonal.periods = c(7, 365))
cmstl <- mstl(cmsts)


#################################
#making the time series as a linear regression where we have the time series in 2 components
#X_t=s_t+Y_t, one is deterministic and one is stochastic

#setting a time interval, which is just our days in the whole periode
t <- 1:length(training_data_price$MeanPrice)

#Constructing the deterministic ts for price 
st_price <- lm(training_data_price$MeanPrice ~ t 
               + sin(t*2*pi/7) + cos(t*2*pi/7)
               + sin(t*2*pi/30) + cos(t*2*pi/30) 
               + sin(t*2*pi/180) + cos(t*2*pi/180) # halvårlig 182.5
               + sin(t*2*pi/365) + cos(t*2*pi/365))
# Now for the consumption
st_consumption <- lm(training_data_consumption$DailyConsumption ~ t 
                     + sin(t*2*pi/7) + cos(t*2*pi/7)
                     + sin(t*2*pi/30) + cos(t*2*pi/30) 
                     + sin(t*2*pi/180) + cos(t*2*pi/180) # halvårlig 182.5
                     + sin(t*2*pi/365) + cos(t*2*pi/365))

# Generate fitted values using the linear model
fitted_values_prices <- fitted(st_price)
fitted_values_consumption <- fitted(st_consumption)

# Subtract the fitted values from price_ts
adjusted_pricets <- price_ts - fitted_values_prices
adjusted_consumptionts <- consumption_ts - fitted_values_consumption

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
###
# Then for consumption
consumption_arima <- Arima(adjusted_consumptionts,
                           order = c(5,1,0),
                           seasonal = list(order = c(1,1,1), period = 7))

#fitting the values
pricets_fit <- fitted(price_arima)
consumptionts_fit <- fitted(consumption_arima)

# making the residuals
res_price <- adjusted_pricets - pricets_fit
res_consumption <- adjusted_consumptionts - consumptionts_fit

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
kpss.test(consumptionts_fit, null = "Trend")
kpss.test(consumptionts_fit, null = c("Level", "Trend"))


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

Model1 <- VAR(colle_ts, p = 8, type = "const", season = NULL, exog = NULL)
restricted_model <- restrict(Model1)

summary(Model1)
summary(restricted_model)

res_var <- residuals(Model1)

price_var <- ts(restricted_model$datamat$pricets_fit, frequency = 365, 
                start = c(2015, which(day(ymd("2015-01-01")) == 
                                        training_data_price$day[1])))

consumption_var <- ts(restricted_model$datamat$consumptionts_fit, frequency = 365, 
                      start = c(2015, which(day(ymd("2015-01-01")) == 
                                              training_data_price$day[1])))

#plot of original and the fitted
# Convert time series to data frames for ggplot
price_var_df <- data.frame(Date = time(price_var), Fitted = as.numeric(price_var))
price_ts_df <- data.frame(Date = time(price_ts), Actual = as.numeric(price_ts))

# Merge data frames by Date
plot_data <- merge(price_var_df, price_ts_df, by = "Date", all = TRUE)

# Convert time series to data frames for ggplot
consumption_var_df <- data.frame(Date = time(consumption_var), Fitted = as.numeric(consumption_var))
consumption_ts_df <- data.frame(Date = time(consumption_ts), Actual = as.numeric(consumption_ts))

# Merge data frames by Date
plot_data <- merge(consumption_var_df, consumption_ts_df, by = "Date", all = TRUE)
################################################################################
###
#Impuslse response function
irf_price <- irf(restricted_model, impulse = "pricets_fit", response = "consumptionts_fit", n.ahead = 40, ortho = TRUE,
                 cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 100)

irf_consumption <- irf(restricted_model, impulse = "consumptionts_fit", response = "pricets_fit", n.ahead = 40, ortho = TRUE,
                       cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 100)
################################################################################
#Making predictions using the VAR(6) model we have constructed
# Forecast future values
forecast_values <- forecast(Model1, h = 90, level = 95)  
forecast_valres <- predict(restricted_model, n.ahead = 90)

################################################################################
################################################################################
#calculating the breakpoint and splitpoint for constructing second model
#breakout tests
r1 <- breakpoints(price_ts ~ 1)
r12 <- confint(r1)

############################################################################################################
############################################################################################################
############################################################################################################
#creating model 2
#splitting it up
training_data_price_2 <- subset(daily_price_means, as.Date("2019-02-23") < day & day < as.Date("2020-01-01"))
training_data_consumption_2 <- subset(daily_cons_means, as.Date("2019-02-23") < day & day < as.Date("2020-01-01"))

start_date <- as.Date("2019-01-01") + days(53)

#making the ts for spot prices
price_ts_2 <- ts(training_data_price_2$MeanPrice,frequency = 365, 
                 start = c(2019, 53))
#Making time series of consumption
consumption_ts_2 <- ts(training_data_consumption_2$DailyConsumption, frequency = 365, 
                       start = c(2019, 53))

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

#Constructing the SARIMA #(3,1,1)(2,1,1)
price_arima_2 <- Arima(diff(adjusted_pricets_2),
                       order = c(3,1,2),
                       seasonal = list(order = c(1,1,1), period = 7))

consumption_arima_2 <- Arima(diff(adjusted_consumptionts_2),
                             order = c(5,1,0),
                             seasonal = list(order = c(1,1,1), period = 7))

# making the residuals
pricets_fit_2 <- fitted(price_arima_2)
consumptionts_fit_2 <- fitted(consumption_arima_2)
res_price_2 <- adjusted_pricets_2 - pricets_fit_2
res_consumption_2 <- adjusted_consumptionts_2 - consumptionts_fit_2



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

Model2 <- VAR(colle_ts_2, p = 7, type = "const", season = NULL, exog = NULL)
restricted_model_2 <- restrict(Model2)

#Serial correlation
serial_price_2 <- serial.test(restricted_model_2, lags.pt = 12, type = "PT.asymptotic")
serial_price_2


##
#predictions
forecast_values_2 <- predict(Model2, n.ahead = 90)  
forecast_resval_2 <- predict(restricted_model_2, n.ahead = 90)

price_var_2 <- ts(restricted_model_2$datamat$pricets_fit_2, frequency = 365, 
                  start = c(2019, 53))
consumption_var_2 <- ts(restricted_model_2$datamat$consumptionts_fit_2, frequency = 365, 
                        start = c(2019, 53))
# Convert time series to data frames for ggplot
price_var_df_2 <- data.frame(Date = time(price_var_2), Fitted = as.numeric(price_var_2))
price_ts_df_2 <- data.frame(Date = time(price_ts_2), Actual = as.numeric(price_ts_2))

# Merge data frames by Date
plot_data_2 <- merge(price_var_df_2, price_ts_df_2, by = "Date", all = TRUE)

# Convert time series to data frames for ggplot
consumption_var_df <- data.frame(Date = time(consumption_var), Fitted = as.numeric(consumption_var))
consumption_ts_df <- data.frame(Date = time(consumption_ts), Actual = as.numeric(consumption_ts))

# Merge data frames by Date
plot_data <- merge(consumption_var_df, consumption_ts_df, by = "Date", all = TRUE)

