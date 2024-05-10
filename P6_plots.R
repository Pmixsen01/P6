source("~/Desktop/P6/P6_kun_functioner.R")
############################################################################
# plot of spot prices time series
plot(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2015-2020")
ggtsdisplay(price_ts, xlab = "Year", ylab = "Daily Prices", main = "Spot price from 2015 through 2019")

# plot of gross consumption time series
plot(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2015-2020")
ggtsdisplay(consumption_ts, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption of electricity from 2015 through 2019")

############################################################################
# plotting the ACF for the time series
Acf(price_ts, lag.max = 60, main = "ACF of spot prices with lag 60")
Acf(consumption_ts, lag.max = 60, main = "ACF of gross consumption with lag 60")

############################################################################
# plots of decomposition #MSTL
autoplot(mstl, main = "Decomposition of spot prices")
autoplot(cmstl, main = "Decomposition of Gross Consumption")

# plots of adjusted, time series
ggtsdisplay(adjusted_pricets, main = "Adjusted spot prices",  lag.max = 60)
ggtsdisplay(adjusted_consumptionts, main = "Adjusted gross consumption", lag.max = 60)
# ACF for adjusted, time series
Acf(adjusted_pricets, lag.max = 30, main = "ACF for adjusted spot prices up to 30 lags")
Acf(adjusted_consumptionts, lag.max = 30,  main = "ACF for adjusted gross consumption up to 30 lags")

################################################################################
# ARIMA residuals plot for spot prices
checkresiduals(price_arima, lag.max = 49)
###
# ARIMA residuals plot for gross consumption
checkresiduals(consumption_arima, lag.max = 49)

# ACF plots of residuls
Acf(res_price, lag.max = 30)
Acf(res_consumption, lag.max = 30)

# making the ggtsdisplays
ggtsdisplay(res_price, main = "Cleaned spot price model")
ggtsdisplay(res_consumption, main = "Cleaned consumption model")


#################################################################################
############################ Plots for the VAR model ############################ 
# Plot AIC and BIC values to visualize the optimal lag
plot(aic_values, type='b', col='blue', xlab='Number of Lags', ylab='AIC', main='AIC and BIC by Lag')
points(bic_values, type='b', col='red')
legend("topright", legend=c("AIC", "BIC"), col=c("blue", "red"), pch=1)

# acf plot for the residuals of var model
Acf(res_var)

#plot of spot prices VAR model
autoplot(price_var, main = "VAR prices")
#plot of gross consumption VAR model
autoplot(consumption_var, main = "VAR prices")

#plot of original and the fitted, spot prices
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

#plot of original and the fitted, gross consumption
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
#Impuslse response function plots
plot(irf_price)
plot(irf_consumption)

################################################################################
# Plot the forecasted values, zooming in on the last part
autoplot(forecast_values, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")
autoplot(forecast_valres, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")

################################################################################
################################################################################
#breakout tests
plot(price_ts)
lines(r1)
lines(r12)

############################################################################################################
############################################################################################################
############################################################################################################
##plot of model 2 for spot prices
plot(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Spot prices, 2019-2020")
ggtsdisplay(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Spot price from 2019 through 2020")

##plot of model 2 for gross consumption
plot(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption, 2019-2020")
ggtsdisplay(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption from 2019 through 2020")


Acf(adjusted_pricets_2, lag.max = 49)
Acf(adjusted_consumptionts_2, lag.max = 49)

ggtsdisplay(adjusted_pricets_2, main = "Adjusted spot prices from 2019 through 2020" )
ggtsdisplay(adjusted_consumptionts_2, main = "Adjusted gross consumption from 2019 through 2020")

#Constructing the SARIMA #(3,1,1)(2,1,1)
checkresiduals(price_arima_2, lag.max = 90)
checkresiduals(consumption_arima_2)


# plot of the residuals
Acf(res_price_2, lag.max = 30)
Acf(res_consumption_2, lag.max = 180)

#making the ggtsdisplays
ggtsdisplay(res_price_2, main = "Cleaned spot price model from 2019 through 2020")
ggtsdisplay(res_consumption_2, main = "Cleaned consumption model from 2019 through 2020")

################################################################################
# Plot AIC and BIC values to visualize the optimal lag for VAR model
plot(aic_values, type='b', col='blue', xlab='Number of Lags', ylab='AIC', main='AIC and BIC by Lag')
points(bic_values, type='b', col='red')
legend("topright", legend=c("AIC", "BIC"), col=c("blue", "red"), pch=1)


Acf(restricted_model_2$varresult$pricets_fit$residuals)
Acf(restricted_model_2$varresult$consumptionts_fit$residuals, lag.max = 60)


##
# Plot the forecasted values, zooming in on the last part
autoplot(forecast_values_2, xlim = c(2019.95, 2020.05), main = "Forecast of future values in 2020")
autoplot(forecast_resval_2, xlim = c(2019.95, 2020.05), main = "New forecast of future values in 2020")



#plot of price
autoplot(price_var_2, main = "VAR prices")
#plot of consumption
autoplot(consumption_var_2, main = "VAR consumption")

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

