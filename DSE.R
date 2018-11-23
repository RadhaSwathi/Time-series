library(readxl)
US_GDP <- read_excel("Time series/US GDP.xlsx")
View(US_GDP)
# Saving data in time series format #
USGDP <- ts(US_GDP[,2], start=c(1929,1), end=c(1991,1), frequency=1)
plot(USGDP)

Shoe_Sales <- read_excel("Time series/Shoe Sales.xlsx")

Shoe <- ts(Shoe_Sales[,3], start=c(2011,1), frequency=12)
plot(Shoe)

Quarterly_Income<-read_excel("Time series/Quarterly Income.xlsx")
Income <- ts(Quarterly_Income[,3], start=c(2000,4), frequency=4)
plot(Income)
Champagne<-read_excel("Time series/Champagne.xlsx")
Champagne <- ts(Champagne[,2], start=c(1964, 1), frequency=12)
plot(Champagne)
AirPassenger<-read_excel("Time series/AirPassenger.xlsx")
AirPax <- ts(AirPassenger[,2], start=c(1949, 1), frequency=12)
plot(AirPax)

smooth_USGDP=ses(USGDP,h=5)
smth_USGDP=holt(USGDP,h=5)
plot(smooth_USGDP)
plot(smth_USGDP)
ts.plot(USGDP,smooth_USGDP$fitted,smth_USGDP$fitted,type="l",col=c("blue","red","green"))
smooth_USGDP$model
smth_USGDP$model
# Plot for seasonality
monthplot(Champagne)
monthplot(AirPax)

monthplot(Income)

# TS decomposition 
IncDec<-stl(Income, s.window='p') #constant seasonality
plot(IncDec)
IncDec

IncDec7<-stl(Income, s.window=7) #seasonality changes
plot(IncDec7)
IncDec

DeseasonRevenue <- (IncDec7$time.series[,2]+IncDec7$time.series[,3])
ts.plot(DeseasonRevenue, Income, col=c("red", "blue"), main="Comparison of Revenue and Deseasonalized Revenue")

stl(Champagne[,1], s.window=5, robust=T)

dt<-stl(Champagne[,1], s.window=5, robust=T)
dts<-decompose(Champagne[,1],type = "additive")
#View(Champagne)
plot(dt)
plot(dts)
# Analysis of a mu ltiplicative series
logAirPax <- log(AirPax)
logAirPaxDec <- stl(logAirPax[,1], s.window="p")
plot(logAirPaxDec)
logAirPaxDec$time.series[1:12,1]
AirPaxSeason <- exp(logAirPaxDec$time.series[1:12,1])
plot(AirPaxSeason, type="l")

decompos_Airpax<-decompose(AirPax[,1],type = "multiplicative")
plot(decompos_Airpax)
plot(decompos_Airpax$figure, type="l")
sum(decompos_Airpax$figure)
sum=decompos_Airpax$trend*decompos_Airpax$seasonal  #yt=(Tt*St)*It
val=AirPax[,1]/sum #Yt/Tt*St


# Dividing a time series into train and test
IncomeTrain <- window(Income, start=c(2000,4), end=c(2012,4), frequency=4)
IncomeTest <- window(Income, start=c(2013,1), frequency=4)

# Model fit and forecast
IncTrn7<-stl(IncomeTrain, s.window=7)
fcst.Inc.stl <- forecast(IncTrn7, method="rwdrift", h=5)

Vec<- cbind(IncomeTest,fcst.Inc.stl$mean)
ts.plot(Vec, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")
MAPE <- mean(abs(Vec[,1]-Vec[,2])/Vec[,1])
MAPE

# Exponential smooting
library(fpp2)

plot(oil, type="l",col="red")
ts.plot(oil,fcoil$fitted,type="l",col=c("blue", "red"))
fcoil <- ses(oil, h=3)
plot(fcoil)
Champ1 <- window(Champagne, start=c(1964,1), end=c(1970,12))
ChampHO <- window(Champagne, start=c(1971,1), end=c(1972,9))
Champ.fc <- hw(Champ1[,1], h=21)
Champ1.stl <- stl(Champ1[,1], s.window = 5)
fcst.Champ1.stl <- forecast(Champ1.stl, method="rwdrift", h=21)
#Finding accuray
Vec1<- cbind(ChampHO,fcst.Champ1.stl$mean)
ts.plot(Vec, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")
MAPE <- mean((abs(Vec1[,1]-Vec1[,2])/Vec1[,1]),na.rm = TRUE)
MAPE

Champ1 <- window(Champagne, start=c(1964,1), end=c(1970,12))
ChampHO <- window(Champagne, start=c(1971,1), end=c(1972,9))
Champ.fc <- hw(Champ1[,1], h=21)
fcst.Champ1.hw <- forecast(Champ.fc, method="rwdrift", h=21)
Vec1<- cbind(ChampHO,fcst.Champ1.hw$mean)
ts.plot(Vec, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")
MAPE <- mean((abs(Vec1[,1]-Vec1[,2])/Vec1[,1]))
MAPE



Champ1 <- window(Champagne, start=c(1964,1), end=c(1970,12))
ChampHO <- window(Champagne, start=c(1971,1), end=c(1972,9))
Champ.fcR <- hw(Champ1[,1], h=21,alpha = 0.1,beta = 0.01,gamma = 0.85)
fcst.Champ1.hw <- forecast(Champ.fcR, method="rwdrift", h=21)
Vec1<- cbind(ChampHO,fcst.Champ1.hw$mean)
#ts.plot(Vec, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")
MAPE <- mean((abs(Vec1[,1]-Vec1[,2])/Vec1[,1]))
MAPE

Champ1 <- window(Champagne, start=c(1964,1), end=c(1970,12))
ChampHO <- window(Champagne, start=c(1971,1), end=c(1972,9))
Champ.fcD <- decompose(Champ1[,1],type = "additive")
fcst.Champ1.hw <- forecast(Champ.fcD, method="rwdrift", h=21)
Vec1<- cbind(ChampHO,fcst.Champ1.hw$mean)
#ts.plot(Vec, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")
MAPE <- mean((abs(Vec1[,1]-Vec1[,2])/Vec1[,1]))
MAPE

AirPax1 <- window(AirPax, start=c(1949,1), end=c(1958,12))
AirPaxHO <- window(AirPax, start=c(1959,1), end=c(1960,12))
AirPax.ft<-hw(AirPax1[,1], h=24,seasonal = "m")
fcst.Air.hw<-forecast(AirPax.ft,method="rwdrift", h=24)
vec<-cbind(AirPaxHO,fcst.Air.hw$mean)
MAPE <- mean((abs(vec[,1]-vec[,2])/vec[,1]))
MAPE
AirPax.fts<-hw(AirPax1[,1], h=12,alpha = 0.1,beta = 0.01,gamma = 0.85)
AirPax.ft$model
vec=cbind(AirPaxHO,fcst.Champ1.hw$mean)
ts.plot(AirPaxHO,fcst.Champ1.hw$mean,col=c("red","blue"))


# Stationarity & ARIMA
BASF <- ts(GermanMonthlyAverageStockPrice[,9], start=c(1981,1), frequency=12)
library(tseries)
adf.test(BASF) 

acf(BASF, lag = 50)

BASF.arima.fit <- arima(BASF, c(1, 1, 0))
BASF.arima.fit

Box.test(BASF.arima.fit$residuals, lag=30, type="Ljung-Box")

plot(forecast(BASF.arima.fit, h=6))

