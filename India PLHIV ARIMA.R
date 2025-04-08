# Load necessary libraries
library(forecast)

# Given data
hiv_plhiv <- c(2702171,	2641018,	2601916,	2547307,	2506608,	2568079,	2534353,	3409965,	2393672,	2392008,	2397884,	2401284
)
years <- seq(2010, 2021)

# Plot the data
plot(years, hiv_plhiv, type = "o", xlab = "Year", ylab = "PLHIV", main = "PLHIV in India (2010-2021)")
# Check for stationarity
adf.test(hiv_plhiv)
# Load necessary library for ADF test
library(urca)

# ADF test for stationarity
adf_test_result <- ur.df(hiv_plhiv, type = "none", selectlags = "AIC")
print(adf_test_result)
# Difference the data
diff_hiv_plhiv <- diff(hiv_plhiv)

# Plot differenced data
plot(years[-1], diff_hiv_plhiv, type = "o", xlab = "Year", ylab = "Differenced PLHIV", main = "Differenced HIV plhiv (2011-2021)")
# ACF and PACF plots
acf(diff_hiv_plhiv)
pacf(diff_hiv_plhiv)
tsdiag(arima_model)
# Fit ARIMA model
arima_model <- auto.arima(hiv_plhiv)
# Forecasting the next 10 years
forecast_result <- forecast(arima_model, h = 10)
summary(arima_model)
# Print the forecasted values
print(forecast_result)
# Plotting the forecasted values
plot(forecast_result, main = "Forecast of PLHIV in India",
     xlab = "Year", ylab = "PLHIV")


