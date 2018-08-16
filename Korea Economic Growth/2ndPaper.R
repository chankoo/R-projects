Data <- read.csv('2ndData.csv',stringsAsFactors = F)
str(Data)
attach(Data)

##plotting
time<-c()
class(time)
for(i in paste("Jan", 1988:2018)){
  i<-paste(substr(i,1,4),substr(i,7,8),sep='')
  time<-c(time,i)
}

ts.plot(WTI_yoy,gpars = list(main="유가, 수출 그리고 한국경기 시계열 ",ylab ="",ylim=c(-50,100),xlab="",lwd=2,axe=F))
axis(2)
axis(1,at=seq(from=1,to=364,by=12), label=time,las=2,lwd=0.1)  
box()
par(new=T)
ts.plot(Export_yoy,col='red',gpars = list(ylab ="",ylim=c(-50,100),xlab="",lwd=2,axe=F))
par(new=T)
ts.plot(CCI_yoy,col='blue',gpars = list(ylim=c(-12.5,25),axes=F,xlab="",ylab='',lwd=2.2))
axis(4)
legend("topleft",legend= c("WTI","Export","(YOY%)"),col=(c("black","red","black"))
       ,lty = c(1,1,0),lwd=c(2,2,0),cex = 0.8)
legend("topright",legend= c("CCI","(YOY%)"),col=(c("blue","black"))
       ,lty = c(1,0),lwd=c(2,0),cex = 0.8)

##statinary check
library(tseries)
WTI_yoy_89 <- WTI_yoy[13:length(WTI_yoy)]
adf.test(WTI_yoy_89) #stationary

Export_yoy_omitted <-na.omit(Export_yoy)
adf.test(Export_yoy_omitted) #stationary

CCI_yoy_89 <- CCI_yoy[13:length(WTI_yoy)]
adf.test(CCI_yoy_89) #stationary

## grangerTest
library(lmtest)
grangertest(WTI_yoy_89~Export_yoy_omitted) # not significant Export -> WTI
grangertest(Export_yoy_omitted~WTI_yoy_89,order=1) # significant WTI -> Export

grangertest(CCI_yoy_89~Export_yoy_omitted,order=1) # significant Export -> CCI

##reStructuring
d1<- cbind(WTI_yoy_89,Export_yoy_omitted)
d1<-as.data.frame(cbind(d1,CCI_yoy_89))
d1$Date <-date[13:length(date)]
colnames(d1)<-c('WTI','Export','CCI','Date')
str(d1)


# ##ggplotting
# library(ggplot2)
# 
# 
# p<-ggplot(data = d1,aes(x=Date,y=WTI,group=1))+scale_x_discrete(c(1,2))
# 
# p1<-p+geom_line(lwd=0.8)
# p2<-p1+geom_line(aes(x=Date,y=Export,group=1),color='red',lwd=0.8)
# p3<-p2+geom_line(aes(x=Date,y=CCI,group=1),color='blue',lwd=0.8)
# p3+scale_fill_manual()
