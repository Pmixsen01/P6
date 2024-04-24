library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)
library(forecast)
library(vars)
library(ggfortify)

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

#taking the log of every entry in daily_cons_means
logged_daily_cons_means <- daily_cons_means %>%
  mutate(LoggedConsumption = log(DailyConsumption))

#########################################################################
# Now, daily_means is our new dataframe with the mean price for each week
#We can now create an initial time series to check for how we want to proceed.
#Plotting the ts with dates in the x-axis  
price_ts <- ts(daily_price_means$MeanPrice,frequency = 365, 
                             start = c(2015, which(day(ymd("2015-01-01")) == 
                                                     daily_price_means$day[1])))
plot(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2015-2019")

#Making time series of consumption
consumption_ts <- ts(daily_cons_means$DailyConsumption, frequency = 365, 
               start = c(2015, which(day(ymd("2015-01-01")) == 
                                       daily_cons_means$day[1])))
plot(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2020")

#constructing ts of logged consumption 
logged_consumption_ts <- ts(logged_daily_cons_means$LoggedConsumption, frequency = 365, 
                            start = c(2015, which(day(ymd("2015-01-01")) == 
                                                    logged_daily_cons_means$day[1])))
plot(logged_consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2020")

############################################################################
#creating the ACF to find what should be done to the time series to achieve stationarity
Acf(price_ts, lag.max = 30)
Acf(consumption_ts, lag.max = 30)
Acf(logged_consumption_ts, lag.max = 365)
#Acf(total_ts)

#multibel linear regression

####################################
#Decomposition of each ts
# Decomposition of price time series
decomposition_price <- stl(price_ts, s.window = "periodic")
autoplot(decomposition_price, ts.colour = "darkblue") + ggtitle("Decomposition of Spot Prices")

# Extract residuals
residuals_price <- decomposition_price$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_price, lag.max = 30)
Pacf(residuals_price, lag.max = 30)

de_de_residuals_price <- stl(residuals_price, s.window = "periodic")
autoplot(de_de_residuals_price)

#Decomposition of consumption
de_consump <- stl(consumption_ts, s.window = "periodic")
autoplot(de_consump, ts.colour = "darkblue") + ggtitle("Decomposition of Gross Consumption")

# Extract residuals
residuals_cons <- de_consump$time.series[, "remainder"]

de_de_residuals_cons <- stl(residuals_cons, s.window = "periodic")
autoplot(de_de_residuals_cons)

Acf(de_de_residuals_cons$time.series[, "remainder"], lag.max = 30)

# Plot ACF and PACF of residuals
Acf(residuals_cons, lag.max = 365)
acf(residuals_cons)
Pacf(residuals_cons, lag.max = 30)

#Decomposition of logged consumption
de_logcons <- stl(logged_consumption_ts, s.window = "periodic")
autoplot(de_logcons, ts.colour = "darkblue") + ggtitle("Decomposition of Gross Consumption")

# Extract residuals
residuals_logcons <- de_logcons$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_logcons, lag.max = 20)

#######################
#deseasonalize price_ts
diff_price_ts <- diff(price_ts)
autoplot(diff_price_ts)

diff_diff_price_ts <- diff(diff_price_ts)
autoplot(diff_diff_price_ts)

adf_test <- ur.df(diff_price_ts)
summary(adf_test)

Acf(diff_price_ts)
Pacf(diff_price_ts)

Acf(diff_diff_price_ts)
Pacf(diff_diff_price_ts)

adf_test2 <- ur.df(diff_diff_price_ts)
summary(adf_test2)

#################################
#making for loop for differencing
diff_test_price <- diff(price_ts)
for (i in 1:10){
  diff_test_price <- diff(diff_test_price)
}
Acf(diff_test_price)

#cons
diff_test_consumption <- diff(consumption_ts)
acf(diff_test_consumption)
for (i in 1:10){
  diff_test_consumption <- diff(diff_test_consumption)
}
Acf(diff_test_consumption, lag.max = 30)
acf(diff_test_consumption)

#making it as a linear regression where we have the time series in 2 components
#X_t=s_t+Y_t

t <- 1:length(daily_price_means$MeanPrice)
st_price <- lm(daily_price_means$MeanPrice ~ t + sin(t*2*pi/7) + cos(t*2*pi/7) + sin(t*2*pi/30) 
                + cos(t*2*pi/30) + sin(t*2*pi/90) + cos(t*2*pi/90) + sin(t*2*pi/180) +
                  sin(t*2*pi/365) + cos(t*2*pi/365))

# Generate predicted values using the linear model
predicted_values <- fitted(st_price)


ts.plot(st_pricets, price_ts, gpars = list(col = c("red", "blue")))


# Convert values into a time series object
st_pricets <- ts(predicted_values,frequency = 365, 
                 start = c(2015, which(day(ymd("2015-01-01")) == 
                                         daily_price_means$day[1])))

# Subtract the predicted values from price_ts
Adjusted_ts <- price_ts - predicted_values

plot(Adjusted_ts)

Acf(Adjusted_ts)
Pacf(Adjusted_ts)



de_adj <- stl(Adjusted_ts, s.window = "periodic")
autoplot(de_adj)
autoplot(decomposition_price)

# Extract residuals
residuals_adj_price <- de_adj$time.series[, "remainder"]

# Plot ACF and PACF of residuals
Acf(residuals_adj_price, lag.max = 365)
Acf(residuals_price, lag.max = 365)

#fun and stuff

#Not needed
#Puting them in the same dataframe
total_df <- tibble(daily_price_means, daily_cons_means$DailyConsumption)
colnames(total_df)[2:3] <- c("Daily Prices","Daily Consumption") 

total_ts <- ts(total_df[2:3], frequency = 365, 
               start = c(2015, which(day(ymd("2015-01-01")) == 
                                       total_df$day[1])))
plot(total_ts, xlab = "Year", main = "Daily Price and Consumption of Electricity, 2015-2024", col = "darkblue")
