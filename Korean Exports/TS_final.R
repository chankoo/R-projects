## Constructing Data --------

data = read.csv("TS_projectData.csv",stringsAsFactors = F)
str(data)

data = data[,c(1,3,5,6,8,10)] # selected variable
str(data)
data1 = data[1:129,];tail(data1) # from 07.01 to 17.09

US_retail = data1$US_retail
US_product = data1$US_product
KR_export = data1$KR_export
KR_exportTo_US = data1$KR_export_to_US
#KR_CCI = data1$KR_CCI.
Won_Dollar = data1$Won.Dollar

## Plotting ----------
ts.plot(US_retail,col='red',gpars = list(ylab ="",ylim=c(-15,15),xlab=""))
par(new=T)
ts.plot(US_product,col='blue',gpars = list(ylab ="",ylim=c(-15,15)),xlab="")
#mtext("YOY%",side=2,line = 2,cex=0.7)
legend("topleft",legend= c("US retail","US production","(YOY%)"),col=(c("red","blue","black"))
       ,lty = c(1,1,0),cex = 0.7)
#legend("topleft",legend= c("US retail","(YOY%)"),col=(c("red","black")),lty = c(1,0),cex = 0.7)

par(new=T)
ts.plot(KR_export,col = "green", gpars = list(ylab ="",ylim=c(-55,55),xlab="Jan-07~Sep-17",axes=F))
axis(side=4)
#mtext("YOY%",side=4,line=0.3,cex=0.7)
legend("bottomright",legend= c("KR export"," ","(YOY%)"),lty = c(1,0,0),cex = 0.7,col = c("green"))
title("US sectors and KR export")

# Optional plot(KR export to US)
#par(new=T)
#ts.plot(KR_exportTo_US,col="gold",gpars=list(ylab="",ylim=c(-55,55),axes=F))
#legend("bottomright",legend = c("KR exoprt","exp to US","(YOY%)"),lty=c(1,1,0),cex=0.7,col = c("green","gold",1))
#axis(side=1)

## CCI to KR export
#ts.plot(KR_CCI,col='gold',gpars = list(ylab ="",ylim=c(-10,10)),xlab="")
#legend("topleft",legend= c("KR CCI","(YOY%)"),col=(c("gold","black")),lty = c(1,0),cex = 0.7)

## Won/Dollar
ts.plot(Won_Dollar,col='gold',gpars = list(ylab ="",ylim=c(-70,70)),xlab="")
legend("bottomleft",legend= c("Won/Dollar","(YOY%)"),col=(c("gold","black")),lty = c(1,0),cex = 0.7)
par(new=T)
ts.plot(KR_export,col = "green", gpars = list(ylab ="",ylim=c(-55,55),axes=F),xlab="Jan-07~Sep-17")
legend("bottomright",legend= c("KR export"," ","(YOY%)"),lty = c(1,0,0),cex = 0.7,col = c("green"))
axis(side=4)
title("Exrate & KR export")

## Variance fluctuation ---------
mean(US_retail)
var(US_retail)
mean(US_product)
var(US_product)
mean(KR_export)
var(KR_export)

## Testing -------
#Stationary check
library(tseries)
adf.test(US_retail) #non-stationary

adf.test(US_product) #stationary
kpss.test(US_product) # non-stationary for alpha = 0.10

adf.test(KR_export) #non-stationary
adf.test(KR_exportTo_US) #non-stationary

adf.test(KR_CCI)

#Independency check should be done for residuals
#Box.test(US_retail,lag = 1,type = "Ljung-Box") # auto correlation??
#Box.test(US_product,lag=1,type = "Ljung-Box")
#Box.test(KR_export,lag=1,type = "Ljung-Box")
#Box.test(KR_exportTo_US,lag=1,type = "Ljung-Box")

## decomposition?(But the data already seasonally adjusted!) --------
ts(data=US_retail,frequency = 12)
dUS_retail <- decompose(ts(data=US_retail,frequency = 12))
plot(dUS_retail)
adf.test(na.omit(dUS_retail$random))

ts(data=US_product,frequency = 12)
dUS_product <- decompose(ts(data=US_product,frequency = 12))
plot(dUS_product)
adf.test(na.omit(dUS_product$random))

ts(data=KR_export,frequency = 12)
dKR_export <- decompose(ts(data=KR_export,frequency = 12))
plot(dKR_export)
adf.test(na.omit(dKR_export$random))

ts(data=KR_exportTo_US,frequency = 12)
dKR_exportTo_US <- decompose(ts(data=KR_exportTo_US,frequency = 12))
plot(dKR_exportTo_US)
adf.test(na.omit(dKR_exportTo_US$random))

## 1st diff,n=128 ------
library(tseries)
US_retail1 = diff(US_retail)
adf.test(US_retail1)
US_product1 = diff(US_product)
adf.test(US_product1) # non-staionary
kpss.test(US_product1) # stationary
KR_export1 = diff(KR_export)
adf.test(KR_export1)
KR_exportTo_US1 = diff(KR_exportTo_US)
adf.test(KR_exportTo_US1)
KR_CCI1 = diff(KR_CCI)
adf.test(KR_CCI1)

library(forecast)
tsdisplay(US_retail1)
tsdisplay(US_product1)
tsdisplay(KR_export1)
tsdisplay(KR_exportTo_US1)

## cross correlation/ granger causality -----------
plot(ccf(US_retail1,US_product1),main = "US retail-US product")
plot(ccf(US_product1,KR_export1),main = "US product-KR export")
plot(ccf(US_retail1,KR_export1),main = "US retail -KR export")
plot(ccf(KR_export1,KR_exportTo_US1),main = "KR export - KR export to US")

library(lmtest)
#Veryfying connection link US retail -> US product -> KR export
grangertest(US_retail1~US_product1)
grangertest(US_product1~US_retail1) # significant US retail -> US product
grangertest(KR_export1~US_product1) # significant US product -> KR export

grangertest(KR_export1~US_retail1,order = 12) # significant US retail -> KR export
grangertest(US_retail1~KR_export1,order=12) # not significant KR export -> US retail

## Suprisingly, there' no granger cause btw KR_export1 & KR_exportTo_US1
#grangertest(KR_exportTo_US1~KR_export1)
#grangertest(KR_export1~KR_exportTo_US1) 

## Try to prove path btw US_retail1 -> US_product1 -> KR_exportTo_US1 but...
grangertest(KR_exportTo_US1~US_product1) #not significant, we losing connecting link
grangertest(KR_exportTo_US1~US_retail1) #significant US retail -> KR export to US

## CCI & export
grangertest(KR_CCI1~KR_export1)
grangertest(KR_export1~KR_CCI1) # not signifcant ....

## Exrate
adf.test(Won_Dollar1)
Won_Dollar1 = diff(Won_Dollar)
grangertest(Won_Dollar1~KR_export1,order=3)
grangertest(KR_export1~Won_Dollar1,order=4)

## AR model ------
ar(US_retail1,method = "ols")
ar(US_product1,method = "ols")
ar(KR_export1,method = "ols")

## ARIMA model ---------

library(forecast)
aUS_retail = auto.arima(US_retail[1:118],seasonal = F,max.p = 12)
aUS_retail = auto.arima(US_retail,seasonal = F,max.p = 12)
#summary(aUS_retail)
#tsdiag(aUS_retail)
#ts.plot(predict(aUS_retail,n.ahead = 10)$pred)
#accuracy(aUS_retail)
#aUS_product = auto.arima(US_product,seasonal = F,max.p = 12)
#summary(aUS_product)
#tsdiag(aUS_product)
#ts.plot(predict(aUS_product,n.ahead = 10)$pred)
#accuracy(aUS_product)

par(mfrow=c(1,2))
acf(KR_export);pacf(KR_export)
aKR_export = auto.arima(KR_export[1:118])
summary(aKR_export)
tsdiag(aKR_export)
ts.plot(predict(aKR_export,n.ahead = 12)$pred,ylab="",xlab="Oct-16 ~ Sep-17")
title("Forcasting KR export")
mtext("(%)",side=2,cex = 0.6,padj = 0,adj=1,line = 2)
accuracy(aKR_export)


## ARIMAX --------
xKR_export = auto.arima(KR_export[1:118],xreg = US_retail[1:118])
xKR_export = auto.arima(KR_export,xreg = US_retail,seasonal =F)
summary(xKR_export)
tsdiag(xKR_export)
ts.plot(predict(xKR_export,newxreg = predict(aUS_retail,n.ahead = 12)$pred 
                ,n.ahead = 12)$pred,ylab="",xlab="Oct-16 ~ Sep-17")
ts.plot(predict(xKR_export,newxreg = US_retail[118:129]
                ,n.ahead = 12)$pred,ylab="",xlab="Oct-16 ~ Sep-17")
title("Forcasting KR export with US retail")
mtext("(%)",side=2,cex = 0.6,padj = 0,adj=1,line = 2)
accuracy(xKR_export)

xKR_export2 = auto.arima(KR_export,xreg = KR_CCI)
summary(xKR_export2)
#tsdiag(xKR_export2)
aKR_CCI = auto.arima(KR_CCI)
#ts.plot(predict(xKR_export2,newxreg = predict(aKR_CCI,n.ahead = 12)$pred ,n.ahead = 12)$pred)
#accuracy(xKR_export2)

xKR_export3 = auto.arima(KR_export[1:118],xreg=Won_Dollar[1:118])
summary(xKR_export3)
aWon_Dollar = auto.arima(Won_Dollar[1:118])
ts.plot(predict(xKR_export3,newxreg = predict(aWon_Dollar,n.ahead = 12)$pred ,n.ahead = 12)$pred)


