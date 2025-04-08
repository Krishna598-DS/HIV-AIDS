# Load necessary libraries
library(forecast)

# Given data
hiv_prevalence <- c(0.32, 0.31, 0.3, 0.28, 0.27, 0.26, 0.25, 0.24, 0.3, 0.22, 0.21, 0.21)
years <- seq(2010, 2021)

# Plot the data
plot(years, hiv_prevalence, type = "o", xlab = "Year", ylab = "HIV Prevalence", main = "HIV Prevalence in India (2010-2021)")
# Check for stationarity
adf.test(hiv_prevalence)
# Load necessary library for ADF test
library(urca)

# ADF test for stationarity
adf_test_result <- ur.df(hiv_prevalence, type = "none", selectlags = "AIC")
print(adf_test_result)
# Difference the data
diff_hiv_prevalence <- diff(hiv_prevalence)

# Plot differenced data
plot(years[-1], diff_hiv_prevalence, type = "o", xlab = "Year", ylab = "Differenced HIV Prevalence", main = "Differenced HIV Prevalence in India (2011-2021)")
# ACF and PACF plots
acf(diff_hiv_prevalence)
pacf(diff_hiv_prevalence)
tsdiag(arima_model)
# Fit ARIMA model
arima_model <- auto.arima(hiv_prevalence)
# Forecasting the next 10 years
forecast_result <- forecast(arima_model, h = 10)
summary(arima_model)
# Print the forecasted values
print(forecast_result)
# Plotting the forecasted values
plot(forecast_result, main = "Forecast of PLHIV in India",
     xlab = "Year", ylab = "PLHIV")

