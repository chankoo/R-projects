d = read.csv('./CaseStudyData/7pronto.csv')
getwd()

d$Day = as.factor(d$Day)
d$Hour = as.factor(d$Hour)
str(d)

boxplot(d[,-c(1,2)]) #wait.time에서 문제(이상치)발생

boxplot(d$Wait.Time~d$Day) # day 5,6
boxplot(d$Wait.Time~d$Hour) # hour 5,7


#변수선택(종속: Total)
##0.PrepT, WaitT, TravelT 다 넣으면 안됨(since (Total = PrepT +WaitT +travelT) ==완전공선성  )
##1.PrepT > 거의일정해서 제외
##2.WatiT > 중요변수
##3.TrvT & Distance > 매우 높은상관관계 > Distance만 사용
cor(d$Travel.Time,d$Distance)
##4.dummy var 코딩 > 바쁜날,시간과 안바쁜날,시간 구분위해 
summary(lm(Wait.Time~Day+Hour,data=d)) # day 5,6 hour 5,7 >>specially busy days&hours
####let day5,6 =1, hour5,7 =1 & 나머지=0

d1 = read.table('clipboard',header = F) #let day5,6 =1, hour5,7 =1
colnames(d1)=c('Day','Hour','PrepTime','WaitTime','TravelTime','Distance','Total')
d1$Day = as.factor(d1$Day)
d1$Hour = as.factor(d1$Hour)
str(d1)
m1=lm(Total~WaitTime*Distance*Day*Hour,data = d1) #full model
step(m1,direction = 'both') 
summary(lm(formula = Total ~ WaitTime * Distance * Day * Hour, data = d1)) #prediction

