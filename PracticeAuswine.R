
Wine_sales<-read.csv('/Users/rsklanu/Time series/AusWineSales.csv',header = TRUE)
View(Wine_sales)
AWS_yearly_red<-ts(Wine_sales[,2], start=c(1980,1), end=c(1995,1), frequency=1)
AWS_quterly_red=ts(Wine_sales[,2],  start=c(1980,1),  frequency=4)
AWS_Minthly_red=ts(Wine_sales[,2],  start=c(1980,1),  frequency=12)
plot(AWS_yearly_red)
plot(AWS_quterly_red)
plot(AWS_Minthly_red)

AWS_Minthly_red_refine=ts(Wine_sales[,2],  start=c(1980,1),  frequency=12)
plot(AWS_Minthly_red_refine)
#AWS_Minthly_red_ref=window(AWS_Minthly_red_refine, start=c(1987,1))
#plot(AWS_Minthly_red_ref)
monthplot(AWS_Minthly_red_refine)
red.stl=stl(AWS_Minthly_red_refine,s.window = 12)
log.red=log(AWS_Minthly_red_refine)
red.stl.m=stl(log.red,s.window = 12)
plot(red.stl.m)
plot(red.stl)
red.decompose=decompose(AWS_Minthly_red_refine,type = "multiplicative")
plot(red.decompose)
red.decompose.a=decompose(AWS_Minthly_red_refine)
plot(red.decompose.a)

### divide into train and test
red.train=window(AWS_Minthly_red_refine,start=c(1980,1),end=c(1993,7),frequency=12)
red.test=window(AWS_Minthly_red_refine,start=c(1993,8),frequency=12)
###
red.stl.train=stl(red.train,s.window = 5)
fcst.red.stl=forecast(red.stl.train, method="rwdrift", h=24)
Vr.stl=cbind(red.test,fcst.red.stl$mean)
MAPEVr <- mean(abs(Vr.stl[,1]-Vr.stl[,2])/Vr.stl[,1])
MAPEVr

####
red.train.log=log(red.train)
red.ltrain=stl(red.train.log,s.window = 5)
fcst.red.stl=forecast(red.ltrain, method="rwdrift", h=24)
red.pred=exp(fcst.red.stl$mean)
Vre.stl=cbind(red.test,red.pred)
MAPEVre <- mean(abs(Vre.stl[,1]-Vre.stl[,2])/Vre.stl[,1])
MAPEVre
#####
red.hw.add=hw(red.train,seasonal = "additive",h=24)
fcst.red.hw <- forecast(red.hw.add, method="rwdrift", h=24)
Vrhw=cbind(red.test,fcst.red.hw$mean)
MAPEVrhw <- mean(abs(Vrhw[,1]-Vrhw[,2])/Vrhw[,1])
MAPEVrhw
#####
red.hw.addm=hw(red.train,seasonal = "multiplicative",h=24)
fcst.red.hwm <- forecast(red.hw.addm, method="rwdrift", h=24)
Vrhwm=cbind(red.test,fcst.red.hwm$mean)
MAPEVrhwm <- mean(abs(Vrhwm[,1]-Vrhwm[,2])/Vrhwm[,1])
MAPEVrhwm
####
red.train.log=log(red.train)
red.ltrain=stl(red.train.log,s.window = 5)
fcst.red.stl=forecast(red.ltrain, method="rwdrift", h=24)
red.pred=exp(fcst.red.stl$mean)
Vre.stl=cbind(red.test,red.pred)
MAPEVre <- mean(abs(Vre.stl[,1]-Vre.stl[,2])/Vre.stl[,1])
MAPEVre

###

red.log=log(AWS_Minthly_red_refine)
red.model=stl(red.log,s.window = 5)
plot(red.model)
fcst.red=forecast(red.model, method="rwdrift", h=24)
red.fpred=exp(fcst.red$mean)
Vre.stl=cbind(AWS_Minthly_red_refine,red.fpred)
ts.plot(AWS_Minthly_red_refine,red.fpred)


AWS_yearly_sparkling<-ts(Wine_sales[,3], start=c(1980,1), end=c(1995,1), frequency=1)
AWS_quterly_sparkling=ts(Wine_sales[,3],  start=c(1980,1),  frequency=4)
AWS_Minthly_sparkling=ts(Wine_sales[,3],  start=c(1980,1),  frequency=12)
plot(AWS_yearly_sparkling)
plot(AWS_quterly_sparkling)
plot(AWS_Minthly_sparkling)
AWS_Minthly_sparkling_refine=ts(Wine_sales[,3],  start=c(1980,1),  frequency=12)
#AWS_Minthly_sparkling_ref=window(AWS_Minthly_sparkling_refine, start=c(1987,1))
plot(AWS_Minthly_sparkling_refine)
monthplot(AWS_Minthly_sparkling_refine)


AWS_yearly_sw<-ts(Wine_sales[,4], start=c(1980,1), end=c(1995,1), frequency=1)
AWS_quterly_sw=ts(Wine_sales[,4],  start=c(1980,1),  frequency=4)
AWS_Minthly_sw=ts(Wine_sales[,4],  start=c(1980,1),  frequency=12)
plot(AWS_yearly_sw)
plot(AWS_quterly_sw)
plot(AWS_Minthly_sw)
AWS_Minthly_sw_refine=ts(Wine_sales[,4],  start=c(1980,1),  frequency=12)
AWS_Minthly_sw_ref=window(AWS_Minthly_sw_refine, start=c(1987,1))
plot(AWS_Minthly_sw_ref)
monthplot(AWS_Minthly_sw_ref)
