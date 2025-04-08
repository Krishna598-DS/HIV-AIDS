# Load necessary libraries
library(forecast)

# Given data
hiv_pmtct_need <- c(31362,	28886,	27386,	25516,	24738,	23236,	23489,	22063,	21571,	21252,	21084,	20612)
years <- seq(2010, 2021)

# Plot the data
plot(years, hiv_pmtct_need, type = "o", xlab = "Year", ylab = "HIV Prevalence", main = "PMTCT Need in India (2010-2021)")
# Check for stationarity
adf.test(hiv_prevalence)
# Load necessary library for ADF test
library(urca)

# ADF test for stationarity
adf_test_result <- ur.df(hiv_pmtct_need, type = "none", selectlags = "AIC")
print(adf_test_result)
# Difference the data
diff_hiv_pmtct_need <- diff(hiv_pmtct_need)

# Plot differenced data
plot(years[-1], diff_hiv_pmtct_need, type = "o", xlab = "Year", ylab = "Differenced HIV Prevalence", main = "Differenced HIV Prevalence in India (2011-2021)")
# ACF and PACF plots
acf(diff_hiv_pmtct_need)
pacf(diff_hiv_pmtct_need)
tsdiag(arima_model)

# Fit ARIMA model
arima_model <- auto.arima(hiv_pmtct_need)
# Forecasting the next 10 years
forecast_result <- forecast(arima_model,lead=10)
summary(arima_model)
# Print the forecasted values
print(forecast_result)
# Plotting the forecasted values
plot(forecast_result, main = "Forecast of PMTCT Need in India",
     xlab = "Year", ylab = "PMTCT Need")

