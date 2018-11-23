library(readxl)
GermanMonthlyAverageStockPrice <- read_excel("Time series/GermanMonthlyAverageStockPrice.xlsx")
View(GermanMonthlyAverageStockPrice)
Basf<-ts(GermanMonthlyAverageStockPrice[,9],start=c(1981,1),frequency=12)
plot(Basf, col="Blue")
library(tseries)
 adf.test(Basf)
Basf.lag1= lag(Basf,-1)
Basf.lag2= lag(Basf,-2)
Basf.lag3= lag(Basf,-3)
Basf.diff=diff(Basf,lag = 1) 
plot(Basf.diff,col="red")
adf.test(Basf.diff)
acf(Basf,lag=43)
a=acf(Basf,lag=25)
a
pacf(Basf,lag=25)
a1=pacf(Basf,lag=25)
a1
a2=acf(Basf.diff,lag=1) #Pvalue must be min
a3=pacf(Basf.diff,lag=25)
Basf.arima=arima(Basf,c(1,1,0))
Basf.residual=plot(Basf.arima$residuals)
hist(Basf.arima$residuals,col = "orange")
Box.test(Basf.arima$residuals,lag = 30,type = "Ljung-Box")
library(forecast)
auto.arima(Basf)
Basf.arima.auto=arima(Basf,c(0,1,2))
plot(forecast(Basf.arima,h=3))
plot(forecast(Basf.arima.auto,h=3))

Basf.train=window(Basf,start=c(1981,1),end=c(1993,7),frequency=12)
Basf.test=window(Basf,start=c(1993,8),frequency=12) 
Basf.test.arima=arima(Basf.train,c(1,1,0))
Basf.train.auto=arima(Basf.train,c(0,1,2))
v1=forecast(Basf.test.arima,h=5)
v2=forecast(Basf.train.auto,h=5)
Vec<- cbind(Basf.test,v1$mean)
Vec1<-cbind(Basf.test,v2$mean)

MAPE <- mean(abs(Vec[,1]-Vec[,2])/Vec[,1])
MAPE

MAPE1 <- mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])
MAPE1

Bmw<-ts(GermanMonthlyAverageStockPrice[,4],start=c(1981,1),frequency=12)
plot(Bmw)
adf.test(Bmw)
bmw.diff=diff(Bmw,lag=1)
adf.test(bmw.diff)
plot(bmw.diff)
acf(bmw.diff,lag=25)
pacf(bmw.diff,lag=25)
Bmw.arima=arima(Bmw,c(1,1,0))
hist(Bmw.arima$residuals)
Box.test(Bmw.arima$residuals,lag = 25,type = "Ljung-Box")
Bmw.arima3=arima(Bmw,c(3,1,0))
hist(Bmw.arima3$residuals)
Box.test(Bmw.arima3$residuals,lag = 25,type = "Ljung-Box") #Pvalue must be more
plot(Bmw.arima3$residuals)
plot(Bmw.arima$residuals)
Bmw.auto=auto.arima(Bmw)
Bmw.auto.fit=arima(Bmw,c(1,1,4))
Bmw.train=window(Bmw,start=c(1981,1),end=c(1993,7),frequency=12)
Bmw.test=window(Bmw,start=c(1993,8),frequency=12)

Bmw.arima.train=arima(Bmw.train,c(1,1,0))
Bmw.arima3.train=arima(Bmw.train,c(3,1,0)) 
Bmw.auto.train=arima(Bmw.train,c(1,1,4))
v1=forecast(Bmw.arima.train,h=5)
v2=forecast(Bmw.arima3.train,h=5)
v3=forecast(Bmw.auto.train,h=5)

vec1=cbind(Bmw.test,v1$mean)
vec2=cbind(Bmw.test,v2$mean)
vec3=cbind(Bmw.test,v3$mean)

MAPE <- mean(abs(vec1[,1]-vec1[,2])/vec1[,1])
MAPE

MAPE1 <- mean(abs(vec2[,1]-vec2[,2])/vec2[,1])
MAPE1

MAPE2 <- mean(abs(vec3[,1]-vec3[,2])/vec3[,1])
MAPE2

Rwe<-ts(GermanMonthlyAverageStockPrice[,5],start=c(1981,1),frequency=12)
plot(Rwe)
adf.test(Rwe)
Rwe.diff=diff(Rwe,lag=1)
adf.test(Rwe.diff)
plot(Rwe.diff)
acf(Rwe.diff)
pacf(Rwe.diff)
Rwe.auto=auto.arima(Rwe.train)

Rwe.train=window(Rwe,start=c(1981,1),end=c(1993,7),frequency=12)
Rwe.test=window(Rwe,start=c(1993,8),frequency=12)

Rwe.arima.fit=arima(Rwe.train,c(1,1,1))
Rwe.auto.fit=arima(Rwe.train,c(0,1,1))

Rwe.v1=forecast(Rwe.arima.fit,h=5)
Rwe.v2=forecast(Rwe.auto,h=5)

Rw.vec1=cbind(Rwe.test,Rwe.v1$mean)
Rw.vec2=cbind(Rwe.test,Rwe.v2$mean)

MAPE <- mean(abs(Rw.vec1[,1]-Rw.vec1[,2])/Rw.vec1[,1])
MAPE

MAPE1 <- mean(abs(Rw.vec2[,1]-Rw.vec2[,2])/Rw.vec2[,1])
MAPE1

Vw<-ts(GermanMonthlyAverageStockPrice[,6],start=c(1981,1),frequency=12)
plot(Vw)
adf.test(Vw)
Vw.diff=diff(Vw,lag=1)
adf.test(Vw.diff)
plot(Vw.diff)
acf(Vw.diff)
pacf(Vw.diff)
Vw.arima=arima(Vw,c(1,1,0))
plot(Vw.arima$residuals)
Box.test(Vw.arima$residuals,lag = 60,type = "Ljung-Box") #pvalue must be high



Kar<-ts(GermanMonthlyAverageStockPrice[,7],start=c(1981,1),frequency=12)
plot(Kar)
adf.test(Kar)
Kar.diff=diff(Kar,lag=1)
adf.test(Kar.diff)
plot(Kar.diff)
acf(Kar.diff)
pacf(Kar.diff)
Kar.arima=arima(Kar,c(1,1,0))
values=seq(0,50,1)
pvalues=c()
for(i in values)
{
  temp=Box.test(Kar.arima$residuals,lag =i,type = "Ljung-Box")
  pvalues=c(pvalues,temp$p.value)
}
pvalues



Kar.auto=auto.arima(Kar.train)




Kar.train=window(Kar,start=c(1981,1),end=c(1993,7),frequency=12)
Kar.test=window(Kar,start=c(1993,8),frequency=12)

Kar.arima.fit=arima(Kar.train,c(1,1,1))
#Kar.auto.fit=arima(Kar.train,c(0,1,1))

Kar.v1=forecast(Kar.arima.fit,h=5)
Kar.v2=forecast(Kar.auto,h=5)

Kar.vec1=cbind(Kar.test,Kar.v1$mean)
Kar.vec2=cbind(Kar.test,Kar.v2$mean)

MAPE.Kar <- mean(abs(Kar.vec1[,1]-Kar.vec1[,2])/Kar.vec1[,1])
MAPE.Kar

MAPE1.Kar <- mean(abs(Kar.vec2[,1]-Kar.vec2[,2])/Kar.vec2[,1])
MAPE1.Kar

values=seq(1,50,1)
pvalues.Kar.auto=c()
pvalues.Kar.fit=c()
for(i in values)
{
  temp=Box.test(Kar.auto$residuals,lag =i,type = "Ljung-Box")
  pvalues.Kar.auto=c(pvalues.Kar.auto,temp$p.value)
  temp1=Box.test(Kar.arima.fit$residuals,lag =i,type = "Ljung-Box")
  pvalues.Kar.fit=c(pvalues.Kar.fit,temp1$p.value)
}
pvalues.Kar.auto
pvalues.Kar.fit

Seimens<-ts(GermanMonthlyAverageStockPrice[,8],start=c(1981,1),frequency=12)
plot(Seimens)
adf.test(Seimens)
Seimens.diff=diff(Seimens,lag=1)
adf.test(Seimens.diff)
plot(Seimens.diff)

Champagne<-read_excel("Time series/Champagne.xlsx")
View(Champagne)
Champng<-ts(Champagne[,2],start = c(1964,1),frequency = 12)
adf.test(Champng)
plot(Champng)
Champng.train=window(Champng, start=c(1964,1), end=c(1970,12),frequency = 12)
Champng.test=window(Champng, start=c(1971,1),frequency = 12)
chmpg.auto=auto.arima(Champng.train)
Chmp.fcst=forecast(chmpg.auto,h=21)

vec.chmp=cbind(Champng.test,Chmp.fcst$mean)

MAPE <- mean((abs(vec.chmp[,1]-vec.chmp[,2])/vec.chmp[,1]))
MAPE

Champ.fc <- hw(Champng.train[,1], h=21)
fcst.Champ1.hw <- forecast(Champ.fc, method="rwdrift", h=21)
Vec1<- cbind(Champng.test,fcst.Champ1.hw$mean)

MAPE <- mean((abs(Vec1[,1]-Vec1[,2])/Vec1[,1]))
MAPE

AirPassenger<-read_excel("Time series/AirPassenger.xlsx")
AirPax <- ts(AirPassenger[,2], start=c(1949, 1), frequency=12)
adf.test(AirPax)
