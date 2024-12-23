setwd('~/Documents/ECON138/Paper')
library(readr)
library(fpp)
library(fpp2)
library(fpp3)
library(tsibble)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(forecast)
library(stats)
library(readxl)
library(tsibble)
library(openintro)
library(quantmod)
library(prophet)
library(randomForest)
library(xts)
library(ggplot2)
library(rugarch)
library(zoo)
library(PerformanceAnalytics)
library(stargazer)
library(xts)
library(tsfknn)
rm(list=ls())


#yahoo finanace data
getSymbols("SPY",src="yahoo",from="2023-10-01",to = "2024-10-01")
head(SPY)

getSymbols("AAPL",src="yahoo",from="2023-10-01",to = "2024-08-14")
head(AAPL)
getSymbols("NVDA",src="yahoo",from="2023-10-01",to = "2024-10-01")
head(NVDA)
getSymbols("GLD",src="yahoo",from="2023-10-01",to = "2024-10-01")
head(GLD)

######## TEST ############
getSymbols("SPY",src="yahoo",from="2023-10-01",to = "2024-08-14")

getSymbols("AAPL",src="yahoo",from="2023-10-01",to = "2024-08-14")

getSymbols("NVDA",src="yahoo",from="2023-10-01",to = "2024-08-14")

getSymbols("GLD",src="yahoo",from="2023-10-01",to = "2024-08-14")



#SPY
stargazer(SPY, type = 'latex', summary = T, title = 'Summary Statistic: SPDR S&P 500 ETF Trust', covariate.labels = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted'))
#AAPL
stargazer(AAPL, type = 'latex', summary = T, title = 'Summary Statistic: Apple Inc.', covariate.labels = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted'))
#NVDA
stargazer(NVDA, type = 'latex', summary = T, title = 'Summary Statistic: Nvidia Corporation', covariate.labels = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted'))
#GLD
stargazer(GLD, type = 'latex', summary = T, title = 'Summary Statistic: SPDR Gold Trust', covariate.labels = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted'))


chartSeries(SPY, theme = chartTheme('white'))
chartSeries(AAPL, theme = chartTheme('white'))
chartSeries(NVDA, theme = chartTheme('white'))
chartSeries(GLD, theme = chartTheme('white'))

par(mfrow=c(1,2))
acf(SPY$SPY.Close, main = '')
pacf(SPY$SPY.Close, main = '')

ndiffs(SPY$SPY.Close)

par(mfrow=c(1,2))
acf(AAPL$AAPL.Close, main = '')
pacf(AAPL$AAPL.Close, main = '')

ndiffs(AAPL$AAPL.Close)

par(mfrow=c(1,2))
acf(NVDA$NVDA.Close, main = '')
pacf(NVDA$NVDA.Close, main = '')

ndiffs(NVDA$NVDA.Close)

par(mfrow=c(1,2))
acf(GLD$GLD.Close, main = '')
pacf(GLD$GLD.Close, main = '')

ndiffs(GLD$GLD.Close)

SPYarima <-  auto.arima(SPY$SPY.Close, lambda = 'auto')
AAPLarima <-  auto.arima(AAPL$AAPL.Close, lambda = 'auto')
NVDAarima <-  auto.arima(NVDA$NVDA.Close, lambda = 'auto')
GLDarima <-  auto.arima(GLD$GLD.Close, lambda = 'auto')

SPYa <-  arima(SPY$SPY.Close, order = c(1,1,0))
AAPLa <-  arima(AAPL$AAPL.Close, order = c(1,1,0))
NVDAa <-  arima(NVDA$NVDA.Close, order = c(2,1,2))
GLDa <-  arima(GLD$GLD.Close, order = c(1,1,0))

stargazer(SPYa, AAPLa, NVDAa, GLDa, dep.var.labels=c('SPY','AAPL','NVDA','GLD'))

s1=ugarchspec(variance.model=list(garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(1,0)))

SPYgarch = ugarchfit(data = SPY$SPY.Close, spec = s1)
plot(SPYgarch, which = 'all')

a1=ugarchspec(variance.model=list(garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(1,0)))

AAPLgarch = ugarchfit(data = AAPL$AAPL.Close, spec = a1)
plot(AAPLgarch, which = 'all')

n1=ugarchspec(variance.model=list(garchOrder=c(2,1)),
                 mean.model=list(armaOrder=c(1,2)))

NVDAgarch = ugarchfit(data = NVDA$NVDA.Close, spec = s1)
plot(NVDAgarch, which = 'all')

g1=ugarchspec(variance.model=list(garchOrder=c(2,1)),
                 mean.model=list(armaOrder=c(1,2)))

GLDgarch=ugarchfit(data = GLD$GLD.Close, spec = s1)
plot(GLDgarch, which = 'all')

################################################################################

autoplot(forecast(SPYa, h=30), ylab = 'SPY Close Price')+
  autolayer(spyts, series = "Original")
autoplot(forecast(AAPLa, h=30), ylab = 'AAPL Close Price')+
  autolayer(aaplts, series = "Original")
autoplot(forecast(NVDAa, h=30), ylab = 'NVDA Close Price')+
  autolayer(nvdats, series = "Original")
autoplot(forecast(GLDa, h=30), ylab = 'GLD Close Price')+
  autolayer(gldts, series = "Original")

SPYgarchforecast <- ugarchforecast(SPYgarch, n.ahead = 30)
plot(SPYgarchforecast, main='GARCH SPY')
AAPLgarchforecast <- ugarchforecast(AAPLgarch, n.ahead = 30)
plot(AAPLgarchforecast, main='GARCH AAPL')
NVDAgarchforecast <- ugarchforecast(NVDAgarch, n.ahead = 30)
plot(NVDAgarchforecast,  main='GARCH NVDA')
GLDgarchforecast <- ugarchforecast(GLDgarch, n.ahead = 30)
plot(GLDgarchforecast,  main='GARCH GLD')


# Define the list of dates
dates_list <- c(
  "2023-10-02", "2023-10-03", "2023-10-04", "2023-10-05", "2023-10-06",
  "2023-10-09", "2023-10-10", "2023-10-11", "2023-10-12", "2023-10-13",
  "2023-10-16", "2023-10-17", "2023-10-18", "2023-10-19", "2023-10-20",
  "2023-10-23", "2023-10-24", "2023-10-25", "2023-10-26", "2023-10-27",
  "2023-10-30", "2023-10-31", "2023-11-01", "2023-11-02", "2023-11-03",
  "2023-11-06", "2023-11-07", "2023-11-08", "2023-11-09", "2023-11-10",
  "2023-11-13", "2023-11-14", "2023-11-15", "2023-11-16", "2023-11-17",
  "2023-11-20", "2023-11-21", "2023-11-22", "2023-11-24", "2023-11-27",
  "2023-11-28", "2023-11-29", "2023-11-30", "2023-12-01", "2023-12-04",
  "2023-12-05", "2023-12-06", "2023-12-07", "2023-12-08", "2023-12-11",
  "2023-12-12", "2023-12-13", "2023-12-14", "2023-12-15", "2023-12-18",
  "2023-12-19", "2023-12-20", "2023-12-21", "2023-12-22", "2023-12-26",
  "2023-12-27", "2023-12-28", "2023-12-29", "2024-01-02", "2024-01-03",
  "2024-01-04", "2024-01-05", "2024-01-08", "2024-01-09", "2024-01-10",
  "2024-01-11", "2024-01-12", "2024-01-16", "2024-01-17", "2024-01-18",
  "2024-01-19", "2024-01-22", "2024-01-23", "2024-01-24", "2024-01-25",
  "2024-01-26", "2024-01-29", "2024-01-30", "2024-01-31", "2024-02-01",
  "2024-02-02", "2024-02-05", "2024-02-06", "2024-02-07", "2024-02-08",
  "2024-02-09", "2024-02-12", "2024-02-13", "2024-02-14", "2024-02-15",
  "2024-02-16", "2024-02-20", "2024-02-21", "2024-02-22", "2024-02-23",
  "2024-02-26", "2024-02-27", "2024-02-28", "2024-02-29", "2024-03-01",
  "2024-03-04", "2024-03-05", "2024-03-06", "2024-03-07", "2024-03-08",
  "2024-03-11", "2024-03-12", "2024-03-13", "2024-03-14", "2024-03-15",
  "2024-03-18", "2024-03-19", "2024-03-20", "2024-03-21", "2024-03-22",
  "2024-03-25", "2024-03-26", "2024-03-27", "2024-03-28", "2024-04-01",
  "2024-04-02", "2024-04-03", "2024-04-04", "2024-04-05", "2024-04-08",
  "2024-04-09", "2024-04-10", "2024-04-11", "2024-04-12", "2024-04-15",
  "2024-04-16", "2024-04-17", "2024-04-18", "2024-04-19", "2024-04-22",
  "2024-04-23", "2024-04-24", "2024-04-25", "2024-04-26", "2024-04-29",
  "2024-04-30", "2024-05-01", "2024-05-02", "2024-05-03", "2024-05-06",
  "2024-05-07", "2024-05-08", "2024-05-09", "2024-05-10", "2024-05-13",
  "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17", "2024-05-20",
  "2024-05-21", "2024-05-22", "2024-05-23", "2024-05-24", "2024-05-28",
  "2024-05-29", "2024-05-30", "2024-05-31", "2024-06-03", "2024-06-04",
  "2024-06-05", "2024-06-06", "2024-06-07", "2024-06-10", "2024-06-11",
  "2024-06-12", "2024-06-13", "2024-06-14", "2024-06-17", "2024-06-18",
  "2024-06-20", "2024-06-21", "2024-06-24", "2024-06-25", "2024-06-26",
  "2024-06-27", "2024-06-28", "2024-07-01", "2024-07-02", "2024-07-03",
  "2024-07-05", "2024-07-08", "2024-07-09", "2024-07-10", "2024-07-11",
  "2024-07-12", "2024-07-15", "2024-07-16", "2024-07-17", "2024-07-18",
  "2024-07-19", "2024-07-22", "2024-07-23", "2024-07-24", "2024-07-25",
  "2024-07-26", "2024-07-29", "2024-07-30", "2024-07-31", "2024-08-01",
  "2024-08-02", "2024-08-05", "2024-08-06", "2024-08-07", "2024-08-08",
  "2024-08-09", "2024-08-12", "2024-08-13", "2024-08-14", "2024-08-15",
  "2024-08-16", "2024-08-19", "2024-08-20", "2024-08-21", "2024-08-22",
  "2024-08-23", "2024-08-26", "2024-08-27", "2024-08-28", "2024-08-29",
  "2024-08-30", "2024-09-03", "2024-09-04", "2024-09-05", "2024-09-06",
  "2024-09-09", "2024-09-10", "2024-09-11", "2024-09-12", "2024-09-13",
  "2024-09-16", "2024-09-17", "2024-09-18", "2024-09-19", "2024-09-20",
  "2024-09-23", "2024-09-24", "2024-09-25", "2024-09-26", "2024-09-27",
  "2024-09-30"
)

# Create a data frame
dates <- data.frame(ds = as.Date(dates_list))

# View the first few rows of the data frame
head(dates)

SPYdf <- data.frame(ds = dates_list,
                 y = as.numeric(SPY[,4]))
AAPLdf <- data.frame(ds = dates_list,
                    y = as.numeric(AAPL[,4]))
NVDAdf <- data.frame(ds = dates_list,
                    y = as.numeric(NVDA[,4]))
GLDdf <- data.frame(ds = dates_list,
                    y = as.numeric(GLD[,4]))

spy.test <- SPYdf[1:219,]
spy.train <- SPYdf[220:251,]

aapl.test <- AAPLdf[1:219,]
aapl.train <- AAPLdf[220:251,]

nvda.test <- NVDAdf[1:219,]
nvda.train <- NVDAdf[220:251,]

gld.test <- GLDdf[1:219,]
gld.train <- GLDdf[220:251,]


SPYpredknn <- knn_forecasting(SPYdf$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")
AAPLpredknn <- knn_forecasting(AAPLdf$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")
NVDApredknn <- knn_forecasting(NVDAdf$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")
GLDpredknn <- knn_forecasting(GLDdf$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")

SPYknnTest <- knn_forecasting(spy.test$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")
AAPLknnTest <- knn_forecasting(aapl.test$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")
NVDAknnTest <- knn_forecasting(nvda.test$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")
GLDknnTest <- knn_forecasting(gld.test$y, h = 30, lags = 1:30, k = 50, msas = "MIMO")

spyts <- ts(SPYdf[,2],frequency=1)

aaplts <- ts(AAPLdf[,2],frequency=1)

nvdats <- ts(NVDAdf[,2],frequency=1)

gldts <- ts(GLDdf[,2],frequency=1)


autoplot(SPYknnTest)+
  autolayer(spyts)

autoplot(AAPLknnTest)+
  autolayer(aaplts)

autoplot(NVDAknnTest)+
  autolayer(nvdats)

autoplot(GLDknnTest)+
  autolayer(gldts)

autoplot(SPYpredknn)
autoplot(AAPLpredknn)
autoplot(NVDApredknn)
autoplot(GLDpredknn)

#Train set model accuracy
SPYro <- rolling_origin(SPYpredknn)
SPYrop <- print(SPYro$global_accu)
AAPLro <- rolling_origin(AAPLpredknn)
AAPLrop <- print(AAPLro$global_accu)
NVDAro <- rolling_origin(NVDApredknn)
NVDArop <- print(NVDAro$global_accu)
GLDro <- rolling_origin(GLDpredknn)
GLDrop <- print(GLDro$global_accu)

stargazer(SPYrop)
stargazer(AAPLrop)
stargazer(NVDArop)
stargazer(GLDrop)
