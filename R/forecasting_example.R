# Forecasting example https://www.simplilearn.com/tutorials/data-science-tutorial/time-series-forecasting-in-r
library(forecast)
library(tidyverse)
data("AirPassengers")

AirPassengers %>% head()
start(AirPassengers)
end(AirPassengers) 
sum(is.na(AirPassengers)) ## check for nulls
summary(AirPassengers)


## view graphs
plot(AirPassengers)
## split graph
tsdata <- ts(AirPassengers, frequency = 12) 
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)

## add a tend to the data
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))


#box plot
boxplot(AirPassengers~cycle(AirPassengers, xlab="Date", ylab = "Passenger Numbers (1000's)", main = "Monthly air passengers boxplot from 1949-1960"))

## Arima model (auto mode)
mymodel <- auto.arima(AirPassengers)
mymodel
plot.ts(mymodel$residuals)


## Forecast
myforecast <- forecast(mymodel, level=c(95), h=10*12)
plot(myforecast)


## validate the model
Box.test(mymodel$resid, lag=5, type="Ljung-Box")
Box.test(mymodel$resid, lag=10, type="Ljung-Box")
Box.test(mymodel$resid, lag=15, type="Ljung-Box")
## the p-values look good.



####### with ggplot 

