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
Acf(res_price, lag.max = 30, main = "ACF for residuals spot prices up to 30 lags")
Acf(res_consumption, lag.max = 30, main = "ACF for residuals Gross Consumption up to 30 lags")

# making the ggtsdisplays
ggtsdisplay(res_price, main = "Cleaned spot price model")
ggtsdisplay(res_consumption, main = "Cleaned consumption model")


#################################################################################
############################ Plots for the VAR model ############################ 

# Plot AIC and BIC values to visualize the optimal lag
plot(aic_values, type='b', col='blue', xlab='Number of Lags', ylab='AIC', main='AIC and BIC by Lag')
points(bic_values, type='b', col='red')
legend("topright", legend=c("AIC", "BIC"), col=c("blue", "red"), pch=1)

Acf(res_var)

plot(res_var)
plot(restricted_model$datamat$pricets_fit)

#plot of price
autoplot(price_var, main = "VAR prices")
ggtsdisplay(price_var)

#plot of consumption
autoplot(consumption_var, main = "VAR prices")
ggtsdisplay(consumption_var)

# Create the plot for prices
ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = Fitted, colour = "Fitted"), size = 1) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
  labs(title = "Comparison of Fitted and Actual Spot Prices",
       x = "Year",
       y = "Spot Price",
       colour = "Legend") +
  scale_colour_manual(values = c("Fitted" = "red", "Actual" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the plot for consumption
ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = Fitted, colour = "Fitted"), size = 1) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
  labs(title = "Comparison of Fitted and Actual Gross Consumption",
       x = "Year",
       y = "Gross Consumption",
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
plot(irf_price)
plot(irf_consumption)


################################################################################
#Making predictions using the VAR(6) model we have constructed
# Plot the forecasted values, zooming in on the last part
autoplot(forecast_valres, xlim = c(2019.95, 2020.5), main = "Forecast of future values in")

###
#plotting
# For Price Forecast
plot(testing_data_price$MeanPrice, type = "l", col = "green4",ylim = range(c(testing_data_price$MeanPrice, forecast_valres$fcst$pricets_fit)),
     xlim = c(1, 100), main = "Spot Price forecast compared to Observations", ylab = "Spot Prices", xlab = "Days")
lines(price_prediction[1:100,1], col = "red")  # Mean forecast
# Adjusting transparency of the confidence interval
polygon(c(1:100, 100:1), c(price_prediction[1:100,3], rev(price_prediction[1:100,2])), col = rgb(0, 0, 1, 0.2), border = NA)
lines(price_prediction[1:100,2], col = "navyblue")
lines(price_prediction[1:100,3], col = "navyblue")
legend("topright", legend = c("Actual", "Forecast", "Confidence"), col = c("green4", "red", "navyblue"), lty = 1, cex = 0.75)

# For Consumption Forecast
plot(testing_data_consumption$DailyConsumption, type = "l", col = "green4", ylim = range(c(testing_data_consumption$DailyConsumption, forecast_valres$fcst$consumptionts_fit)),
     xlim = c(1, 100), main = "Gross Consumption forecast compared to Observations", ylab = "Gross Consumption", xlab = "Days")
lines(consumption_prediction[1:100,1], col = "red")  # Mean forecast
# Adjusting transparency of the confidence interval
polygon(c(1:100, 100:1), c(consumption_prediction[1:100,3], rev(consumption_prediction[1:100,2])), col = rgb(0, 0, 1, 0.2), border = NA)
lines(consumption_prediction[1:100,2], col = "navyblue")
lines(consumption_prediction[1:100,3], col = "navyblue")
legend("topright", legend = c("Actual", "Forecast", "Confidence"), col = c("green4", "red", "navyblue"), lty = 1, cex = 0.75)

###############################################################################
################################################################################
# Breakpoint tests
plot(price_ts, main = "Regime Shifts in Spot Prices", ylab = "Spot prices")
lines(r1, col = "navyblue")
lines(r12, col = "red")
legend("topright", legend = c("Breakpoints", "Confidence Intervals"), 
       col = c("navyblue", "red"), lty = 1, cex = 0.6)




############################################################################################################
############################################################################################################
############################################################################################################
#creating model 2
#plots for price
plot(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Electricity Prices, 2019-2020")
ggtsdisplay(price_ts_2, xlab = "Year", ylab = "Daily Prices", main = "Spot price from 2019 through 2020")
#plors for consumption
plot(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Consumption, 2018-2020")
ggtsdisplay(consumption_ts_2, xlab = "Year", ylab = "Daily Consumption", main = "Gross consumption of electricity from 2018 through 2019")


Acf(adjusted_pricets_2, lag.max = 49)
Acf(adjusted_consumptionts_2, lag.max = 49)

ggtsdisplay(adjusted_pricets_2)
ggtsdisplay(adjusted_consumptionts_2)

#Splot the SARIMA #(3,1,1)(2,1,1)
checkresiduals(price_arima_2, lag.max = 49)
checkresiduals(consumption_arima_2, lag.max = 49)

#residuals
Acf(res_price_2, lag.max = 30)
autoplot(price_ts_2, series = "price_ts") + autolayer(res_price_2, series = "res_price") + 
  autolayer(pricets_fit_2, series = "fitted price")

Acf(res_consumption_2, lag.max = 180)
autoplot(consumption_ts_2, series = "consumption_ts") + autolayer(res_consumption_2, series = "res_cons") + 
  autolayer(consumptionts_fit_2, series = "fitted consumption")

#making the ggtsdisplays
ggtsdisplay(res_price_2, main = "Cleaned spot price model")
ggtsdisplay(res_consumption_2, main = "Cleaned consumption model")

ggtsdisplay(pricets_fit_2)
ggtsdisplay(adjusted_pricets_2)

################################################################################
# Constructing the VAR for model2
# Plot AIC and BIC values to visualize the optimal lag
plot(aic_values, type='b', col='blue', xlab='Number of Lags', ylab='AIC', main='AIC and BIC by Lag')
points(bic_values, type='b', col='red')
legend("topright", legend=c("AIC", "BIC"), col=c("blue", "red"), pch=1)

Acf(restricted_model_2$varresult$pricets_fit$residuals)
Acf(restricted_model_2$varresult$consumptionts_fit$residuals, lag.max = 60)

# Plot the forecasted values, zooming in on the last part
autoplot(forecast_valres_2, xlim = c(2019.95, 2020.05), main = "New forecast of future values in 2020")

# For Price Forecast
plot(testing_data_price$MeanPrice, type = "l", col = "green4",ylim = range(c(testing_data_price$MeanPrice, forecast_valres_2$fcst$pricets_fit)), 
     xlim = c(1, 100), main = "Spot Price forecast compared to Observations", ylab = "Spot Prices", xlab = "Days")
lines(price_prediction_2[1:100,1], col = "red")  # Mean forecast
# Adjusting transparency of the confidence interval
polygon(c(1:100, 100:1), c(price_prediction_2[1:100,3], rev(price_prediction_2[1:100,2])), col = rgb(0, 0, 1, 0.2), border = NA)
lines(price_prediction_2[1:100,2], col = "navyblue")
lines(price_prediction_2[1:100,3], col = "navyblue")
legend("topright", legend = c("Actual", "Forecast", "Confidence"), col = c("green4", "red", "navyblue"), lty = 1, cex = 0.8)

# For Consumption Forecast
plot(testing_data_consumption$DailyConsumption, type = "l", col = "green4", ylim = range(c(testing_data_consumption$DailyConsumption, forecast_valres_2$fcst$consumptionts_fit)), 
     xlim = c(1, 100), main = "Gross Consumption forecast compared to Observations", ylab = "Spot Prices", xlab = "Days")
lines(consumption_prediction_2[1:100,1], col = "red")  # Mean forecast
# Adjusting transparency of the confidence interval
polygon(c(1:100, 100:1), c(consumption_prediction_2[1:100,3], rev(consumption_prediction_2[1:100,2])), col = rgb(0, 0, 1, 0.2), border = NA)
lines(consumption_prediction_2[1:100,2], col = "navyblue")
lines(consumption_prediction_2[1:100,3], col = "navyblue")
legend("topright", legend = c("Actual", "Forecast", "Confidence"), col = c("green4", "red", "navyblue"), lty = 1, cex = 0.75)

#plot of VAR mdoels
autoplot(price_var_2, main = "VAR prices")
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


