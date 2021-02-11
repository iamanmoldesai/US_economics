library(dplyr)
library(readr)
library(lmtest)
library(forecast)

economic <- read.csv(file.choose())
head(economic)

#Durbin-Watson

LinearMod <-  lm(uempmed ~ pop + pce, data = economic)
LinearMod

dwtest(LinearMod)

#setting a timeseries to the data set and plotting the graph

economicseries <- ts(economic$uempmed, start = c(1967), frequency = 12)
economicseries
plot.ts(economicseries)


#Difference

economicseriesdiff1 <- diff(economicseries, differences=1)
plot.ts(economicseriesdiff1)

acf(economicseriesdiff1, lag.max = 20)
acf(economicseriesdiff1, lag.max = 20, plot = FALSE)

pacf(economicseriesdiff1, lag.max=20)
pacf(economicseriesdiff1, lag.max=20, plot=FALSE)

#Testing Arima Models
economicseriesarima <- arima(economicseriesdiff1, order=c(3,0,3))
economicseriesarima

economicseriesforecasts <- forecast(economicseriesarima, h=70)
economicseriesforecasts
plot(forecast(economicseriesforecasts))

acf(economicseriesforecasts$residuals, lag.max=20)
Box.test(economicseriesforecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(economicseriesforecasts$residuals) 

#Histogram

mybinsize <- IQR(economicseriesforecasts$residuals,na.rm=TRUE)/4
mysd   <- sd(economicseriesforecasts$residuals,na.rm=TRUE)
mymin  <- min(economicseriesforecasts$residuals,na.rm=TRUE) - mysd*5
mymax  <- max(economicseriesforecasts$residuals,na.rm=TRUE) + mysd*3

mynorm <- rnorm(10000, mean=0, sd=mysd)
mymin2 <- min(mynorm)
mymax2 <- max(mynorm)
if (mymin2 < mymin) { mymin <- mymin2 }
if (mymax2 > mymax) { mymax <- mymax2 }


mybins <- seq(mymin, mymax, mybinsize)
hist(economicseriesforecasts$residuals, col="red", freq=FALSE, breaks=mybins)
myhist <- hist(mynorm, plot=FALSE, breaks=mybins)

points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)


mean(economicseriesforecasts$residuals)