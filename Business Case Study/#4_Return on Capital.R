roc=read.csv("./CaseStudyData/4ROC_4Sectors.csv")
str(roc)


#데이터 정리
attach(roc)
B=data.frame(BANKS,RETURN,SALES,MARGIN,DEBTTOCAPITAL)
COM=data.frame(COMPUTERS,RETURN.1,SALES.1,MARGIN.1,DEBTTOCAPITAL.1)
CON=data.frame(CONSTRUCTION,RETURN.2,SALES.2,MARGIN.2,DEBTTOCAPITAL.2)
E=data.frame(ENERGY,RETURN.3,SALES.3,MARGIN.3,DEBTTOCAPITAL.3)

B=na.omit(B) #Bank
COM=na.omit(COM) #Computer
CON=na.omit(CON) #Construction
E=na.omit(E) #Energy


#SECTOR 변수 추가
B$SECTOR="BANK"
COM$SECTOR="COM"
CON$SECTOR="CON"
E$SECTOR="E"

B$SECTOR=as.factor(B$SECTOR)
COM$SECTOR=as.factor(COM$SECTOR)
CON$SECTOR=as.factor(CON$SECTOR)
E$SECTOR=as.factor(E$SECTOR)

#COL이름 통일
names(B)=c("NAME","RETURN","SALES","MARGIN","DEBTTOCAPITAL","SECTOR")
names(COM)=c("NAME","RETURN","SALES","MARGIN","DEBTTOCAPITAL","SECTOR")
names(CON)=c("NAME","RETURN","SALES","MARGIN","DEBTTOCAPITAL","SECTOR")
names(E)=c("NAME","RETURN","SALES","MARGIN","DEBTTOCAPITAL","SECTOR")

#아웃라이어 제거
#B=B[B$NAME!="Zions Bancorp",] #by BOXPLOT
#CON=CON[CON$NAME!="MDC Holdings" & CON$NAME!="NVR", ]#by BOXPLOT

#B=B[B$NAME!="Synovus FinI",]#by sudentized r

#rbind
data <- rbind(B,COM)
data <- rbind(data,CON)
data <- rbind(data,E)

#feature scaling
# data$RETURN = (data$RETURN-mean(data$RETURN))/sd(data$RETURN)
# data$SALES = (data$SALES -mean(data$SALES))/sd(data$SALES)
# data$MARGIN = (data$MARGIN-mean(data$MARGIN))/sd(data$MARGIN)
# data$DEBTTOCAPITAL = (data$DEBTTOCAPITAL-mean(data$DEBTTOCAPITAL))/sd(data$DEBTTOCAPITAL)

str(data)

#full_model
full_model<-lm(RETURN~SALES*MARGIN*DEBTTOCAPITAL*SECTOR,data = data)
summary(full_model)

anova(full_model,data=data) #분산분석 결과는 결국 평균의 차이 유무

selected_form <- step(full_model,direction = "both")
selected_model<-lm(formula = selected_form,data=data)
summary(selected_model)

plot(selected_model) #outlier 56,43,30,37로 판단

data1 <-data[-c(56,43,30,37),] ;nrow(data1) #outlier deleted -> 제거가능한 이유 밝혀야함 
selected_model1 <-lm(formula = selected_form,data=data1)
summary(selected_model1) #Adjusted R-squared:  0.5979 
plot(selected_model1) #비교적 잔차 일정, 등분산 


##selected_model1
# Call:
#   lm(formula = selected_form, data = data1)
# 
# Coefficients:
#   (Intercept)                MARGIN         DEBTTOCAPITAL             SECTORCOM             SECTORCON  
# 19.142366             -0.130604             -0.193883              0.300801              4.421621  
# SECTORE  MARGIN:DEBTTOCAPITAL      MARGIN:SECTORCOM      MARGIN:SECTORCON        MARGIN:SECTORE  
# 0.162399              0.005562              0.406917             -0.256639             -0.088740  


# prediction 
predict(selected_model1,newdata=data.frame(SALES=c(2000),
                              MARGIN=c(3.5),
                              DEBTTOCAPITAL=c(50),
                              SECTOR="BANK"),interval = "confidence")

predict(selected_model1,newdata=data.frame(SALES=c(2000),
                              MARGIN=c(3.5),
                              DEBTTOCAPITAL=c(50),
                              SECTOR="COM"),interval = "confidence")

predict(selected_model1,newdata=data.frame(SALES=c(2000),
                              MARGIN=c(3.5),
                              DEBTTOCAPITAL=c(50),
                              SECTOR="CON"),interval = "confidence")

predict(selected_model1,newdata=data.frame(SALES=c(2000),
                              MARGIN=c(3.5),
                              DEBTTOCAPITAL=c(50),
                              SECTOR="E"),interval = "confidence")







