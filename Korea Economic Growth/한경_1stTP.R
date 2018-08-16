data <-read.csv('1stTP_data4.csv',stringsAsFactors = F)
data <- data[-1,]


for(i in 1:ncol(data)){
  data[,i] <- as.numeric(data[,i])
}

str(data)
head(data)

TB <- data$관리대상수지
GDP <- data$국내총생산..명목...원화표시.
TBbyGDP <-TB/GDP
GGE <- data$일반정부.총지출
GGEbyGDP <- GGE/GDP
#RGDP <- data$X1인당.실질.국민총소득..
RGGEperC <- data$X1인당.일반정부.총지출..불변.
RGNIperC <- data$X1인당.실질.국민총소득..

TB.ts <- ts(TB,s=1970,f=1)
TBbyGDP.ts <- ts(TBbyGDP,s=1970,f=1)
GGEbyGDP.ts <- ts(GGEbyGDP,s=1970,f=1)
#RGDP.ts <- ts(RGDP,s=1970,f=1)

RGGEperC.ts <- ts(RGGEperC,s=1970,f=1)
RGNIperC.ts <- ts(RGNIperC,s=1970,f=1)


ts.plot(TB.ts)
ts.plot(TBbyGDP.ts)
ts.plot(GGEbyGDP.ts)
#ts.plot(RGDP.ts)

ts.plot(RGGEperC.ts)
ts.plot(RGNIperC.ts)
#####stationary check
library(tseries)
adf.test(TBbyGDP.ts) #non-stationary
adf.test(GGEbyGDP.ts) #non-stationary
adf.test(RGDP.ts) #non-stationary

adf.test(RGGEperC.ts) #non-stationary
adf.test(RGNIperC.ts) #non-stationary

TBbyGDP1 <-diff(TBbyGDP.ts) #1st diff
GGEbyGDP1 <- diff(GGEbyGDP.ts)
#RGDP1 <- diff(RGDP.ts)

RGGEperC1 <- diff(RGGEperC.ts)
RGNIperC1<-diff(RGNIperC.ts)

adf.test(TBbyGDP1) #stationary
adf.test(GGEbyGDP1) #stationary
adf.test(RGDP1) #stationary

adf.test(RGGEperC1) #stationary
adf.test(RGNIperC1) #stationary

library(lmtest)
#grangertest(TBbyGDP1~RGDP1) #no TBbyGDP1 <= RGDP1
#grangertest(RGDP1~TBbyGDP1) #no RGDP1 <= TBbyGDP1

#grangertest(RGDP1~GGEbyGDP1) #no GGEbyGDP1 <= RGDP1
#grangertest(GGEbyGDP1~RGDP1) #no RGDP1 <= GGEbyGDP1

grangertest(RGGEperC1~RGNIperC1,order= 2) #yes RGGEperC1 <= RGNIperC1  
grangertest(RGNIperC1~RGGEperC1,order = 2) #no RGNIperC1 <= RGGEperC1


######
NDR <- data$gdp대비.국가채무비율..기획재정부
NDR.ts <- ts(NDR,s=1970,f=1)

ts.plot(TBbyGDP.ts*100,col='black',gpars = list(ylab ="",ylim=c(-5,5),xlab="",lwd=2))
par(new=T)
ts.plot(NDR.ts,col='red',gpars = list(ylab ="",ylim=c(10,40),xlab="",axes=F,lwd=2))
axis(side=4)
#mtext("%",side=2,line = 2,cex=1)
legend("topleft",legend= c("TBbyGDP(%)","NationalDebtRatio(%)"),col=(c("black","red"))
       ,lty = c(1,1),cex = 0.5,lwd=2)

RTB <- data$관리대상수지..불변.
RTB.ts <- ts(RTB,s=1970,f=1)

ts.plot(RTB.ts,col='black',gpars = list(ylab ="",ylim=c(-50000,50000),xlab="",lwd=2))
par(new=T)
ts.plot(NDR.ts,col='red',gpars = list(ylab ="",ylim=c(10,40),xlab="",axes=F,lwd=2))
axis(side=4,col="red")
#mtext("%",side=2,line = 2,cex=1)
legend("topleft",legend= c("관리대상수지(불변,십억원)","GDP 대비 국가채무비율(%)"),col=(c("black","red"))
       ,lty = c(1,1),cex = 1,lwd=2)
