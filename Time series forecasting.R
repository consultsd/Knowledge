#Removing the objects saved in the console
rm(list = ls())

#Setting the work directory
setwd("C:/Users/Sharath P Dandamudi/Desktop")

library(readxl)

# Read the data from Excel into R
mydata <- read_excel("Train.xlsx")

str(mydata)

# Look at the first few lines of mydata
head(mydata)

# Converting date from character format to date format
mydata$Datetime <- as.POSIXct(mydata$Datetime,format='%d-%m-%Y %H:%M')

str(mydata)

# The function ts() takes in three arguments:
#   
# data is set to everything in usnim_2002 except for the date column; it isn't 
# needed since the ts object will store time information separately.
# 
# start is set to the form c(year, period) to indicate the time of the 
# first observation. Here, January corresponds with period 1; likewise, 
# 
# a start date in April would refer to 2, July to 3, and October to 4.
# frequency is set to 4 because the data are quarterly.

# Create a ts object called myts
myts <- ts(mydata[,2], start = as.numeric(mydata[1,1]), frequency = 365.25*24)

# Create a ts object called myts
# myts <- ts(mydata[, 2:4], start = c(1981, 1), frequency = 4)

# Plot the data with facetting
autoplot(myts, facets = TRUE)
frequency(myts)

ggseasonplot(myts)

# Produce a polar coordinate season plot
ggseasonplot(myts, polar = TRUE)

# Make plots 
ggsubseriesplot(myts)

str(oil)
head(oil)
View(oil)
frequency(oil)

oil_df <- as.data.frame(oil)

# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)


# The trend in the data causes the autocorrelations for the first few lags to be positive.

# The dashed blue lines are critical values. Any correlations within these lines is not 
# significantly different from zero.

View(sunspot.year)

?sunspot.year

# Plots of annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Look at the ACF plot for sunspot.year and find the maximum ACF (y), 
# or the tallest bar on the grpah. Its corresponding lag (x) is 1, so, set it equal to maxlag_sunspot

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1


# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7


# Trends induce positive correlations in the early lags.
# Seasonality will induce peaks at the seasonal lags.
# Cyclicity induces peaks at the average cycle length.

set.seed(3)
wn <- ts(rnorm(36))

ggAcf(wn)

ggtitle("Sample ACF for white noise")


# White noise is a term that describes purely random data. You can conduct a Ljung-Box test 
# using the function below to confirm the randomness of a series; a p-value greater than 0.05 
# suggests that the data are not significantly different from white noise.

# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))


# ACF of the differenced series
ggAcf(diff(goog))

?Box.test

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")



# Use naive() to forecast the goog series
fcgoog <- naive(goog, h = 20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer, h = 16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)

# Residuals should look like white noise
# Residuals should be uncorrelated
# Residuals should have a mean of 0
# 
# Residuals should have constant variance
# Residuals should be normally distributed

# p value less than 0.05 indicates data is probably not white noise

# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
googwn <- TRUE

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
beerwn <- FALSE


View(gold)

# Create the training data as train
train <- window(gold, end=1000)

# Compute naive forecasts and save to naive_fc (108 - forecast for future)
naive_fc <- naive(train, h = 108)

# Compute mean forecasts and save to mean_fc (108 - forecast for future)
mean_fc <- meanf(train, h = 108)

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc

vn

# Create 3 training series
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
train3 <- window(vn[, "Melbourne"], end = c(2012, 4))

# Produce snaive forecasts
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# Compare forecast accuracy
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]


# The tsCV() function computes time series cross-validation errors. 
# It requires you to specify the time series, the forecast method, and the forecast horizon.

# Compute cross-validated errors for up to 8 steps ahead
e <- matrix(NA_real_, nrow = 1000, ncol = 8)
for (h in 1:8)
  e[, h] <- tsCV(goog, forecastfunction = naive, h = h)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


# The ses() function produces forecasts obtained using simple exponential smoothing (SES). 
# The parameters are estimated using least squares estimation. All you need to specify is 
# the time series and the forecast horizon; the default forecast time is h = 10 years

marathon

str(marathon)

# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)


# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))


# Create a training set using subset()
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive


# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa, h = 10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)


# Plot the data
autoplot(a10)

a10

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE


# Just because the residuals fail the white noise test doesn't mean the forecasts 
# will be bad. Most likely, the prediction intervals will be inaccurate but the point forecasts 
# will still be ok

# Plot forecasts
autoplot(fc)

frequency(hyndsight)

# Create training data with subset()
train <- subset(hyndsight, end = length(hyndsight) - 28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 28)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)


# Fit ETS model to austa in fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))


# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE


# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}


# Apply tsCV() for both methods
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)


# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Copy the best forecast MSE
bestmse <- mean(e2^2, na.rm = TRUE)


# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h = 20) %>% autoplot()

# Use a Box-Cox transformation to stabilize the variance of the pre-loaded time series

# Plot the series
autoplot(a10)

# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda = 0.0) %>% autoplot()
a10 %>% BoxCox(lambda = 0.1) %>% autoplot()
a10 %>% BoxCox(lambda = 0.2) %>% autoplot()
a10 %>% BoxCox(lambda = 0.3) %>% autoplot()

# Compare with BoxCox.lambda()
BoxCox.lambda(a10)

# Differencing is a way of making a time series stationary; this means that you remove any 
# systematic patterns such as trend and seasonality from the data. A white noise series is 
# considered a special case of a stationary time series.

# Plot the US female murder rate
autoplot(wmurders)

# Plot the differenced murder rate
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))


# With seasonal data, differences are often taken between observations in the same season 
# of consecutive years, rather than in consecutive periods. For example, with quarterly data, 
# one would take the difference between Q1 in one year and Q1 in the previous year. This is 
# called seasonal differencing.

# Plot the data
autoplot(h02)

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Take another difference and plot
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)

# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)


# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)
residualsok <- TRUE

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used
AICc <- -14.46
d <- 1

# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()

# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0,1,1), include.constant = FALSE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2,1,3), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant.
austa %>% Arima(order = c(0,0,1), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,2,1) model with no constant.
austa %>% Arima(order = c(0,2,1), include.constant = FALSE) %>% forecast() %>% autoplot()


# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h = 1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h = 1)

# Find MSE of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)


# Check that the logged h02 data have stable variance
h02 %>% log() %>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02, lambda = 0)

# Summarize the fitted model
summary(fit)

# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1

# Plot 2-year forecasts
fit %>% forecast(h = 24) %>% autoplot()


# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)

# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)

# AICc of better model
AICc <- 68.39

# Compute 2-year forecasts from better model
fit2 %>% forecast(h = 8) %>% autoplot()


# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007, 4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find best model based on RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)
