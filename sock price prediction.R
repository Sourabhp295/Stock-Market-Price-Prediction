################# Stock Market Price Prediction using R #################

##Importing Required Packages
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)


##Importing Dataset from Finance Websites...(Default yahoo)
getSymbols('AMZN', from = '2019-01-01', to = '2021-01-01')
View(AMZN)
#class(AMZN)


chartSeries(AMZN, subset = 'last 6 months', type = 'auto')
addBBands()

##Assigning columns of dataset  
Open_prices = AMZN[,1]
High_prices = AMZN[,2]
Low_prices = AMZN[,3]
Close_prices = AMZN[, 4]
Volume_prices = AMZN[,5]
Adjusted_prices = AMZN[,6]

par(mfrow = c(2,3))

plot(Open_prices, main = 'Opening Price of Stocks (Over a given period)')
plot(High_prices, main = 'Highest Price of Stocks (Over a given period)')
plot(Low_prices, main = 'Lowest Price of Stocks (Over a given period)')
plot(Close_prices, main = 'Closing Price of Stocks (Over a given period)')
plot(Volume_prices, main = 'Volume of Stocks (Over a given period)')
plot(Adjusted_prices, main = 'Adjusted Price of Stocks (Over a given period)')

Predic_Price = Adjusted_prices
#class(Predic_Price)


######## Finding the Linear Relation between observations ########

par(mfrow = c(1,2))
Acf(Predic_Price, main = 'ACF for differenced Series')
Pacf(Predic_Price, main = 'PACF for differenced Series ', col = '#cc0000')
Auto_cf = Acf(Predic_Price, plot = FALSE)
Auto_cf
PAuto_cf = Pacf(Predic_Price, plot = FALSE)
PAuto_cf

print(adf.test(Predic_Price))




################### Prediction of Return ##########################

return_AMZN <- 100*diff(log(Predic_Price))

AMZN_return_train <- return_AMZN[1:(0.9*length(return_AMZN))]

AMZN_return_test <- return_AMZN[(0.9*length(return_AMZN)+1):length(return_AMZN)]

auto.arima(AMZN_return_train, seasonal = FALSE)

fit <- Arima(AMZN_return_train, order = c(1,0,0))

preds <- predict(fit, n.ahead = (length(return_AMZN) - (0.9*length(return_AMZN))))$pred
preds


################## Forecasting Predicted Result ##################

test_forecast <- forecast(fit,h = 15)
test_forecast

par(mfrow = c(1,1))
plot(test_forecast, main = "Arima forecast for Apple Stock")

accuracy(preds, AMZN_return_test)