#Mimi Trinh
#Unit 2 Bingo Bonus
library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(rJava)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(moments)
library(plotROC)
library(pROC)
library(stats4)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

setwd('/Users/mimitrinh/Desktop')
data=read.csv('logit_insurance.csv')

#data exploration
str(data)
summary(data) #need to fix the following variables to the correct format in R
#need to fix missing values in section 2: AGE, YOJ, INCOME, HOME_VAL, JOB, CAR_AGE
data$TARGET_FLAG=factor(data$TARGET_FLAG) #convert 2-level numeric to 2-level nominal variable
summary(data$TARGET_FLAG)
data$KIDSDRIV=factor(ifelse(data$KIDSDRIV>0,1,0)) #convert 5-level numeric to 2-level nominal
summary(data$KIDSDRIV)
data$HOMEKIDS=factor(ifelse(data$HOMEKIDS>0,1,0)) #convert 6-level numeric to 2-level nominal
summary(data$HOMEKIDS)
data$INCOME=suppressWarnings(as.numeric(gsub('[^0-9.]','',data$INCOME))) #convert nominal to numeric 
summary(data$INCOME)
data$HOME_VAL=suppressWarnings(as.numeric(gsub('[^0-9.]','',data$HOME_VAL))) #convert nominal to numeric 
summary(data$HOME_VAL)
data$BLUEBOOK=suppressWarnings(as.numeric(gsub('[^0-9.]','',data$BLUEBOOK))) #convert nominal to numeric 
summary(data$BLUEBOOK)
data$OLDCLAIM=suppressWarnings(as.numeric(gsub('[^0-9.]','',data$OLDCLAIM))) #convert nominal to numeric 
summary(data$OLDCLAIM)
data$CAR_AGE[data$CAR_AGE<0]=NA #can't have negative car age, so change that value to NA
summary(data$CAR_AGE)
str(data) #good format now
summary(data)

#decision tree
#variables with missing values: AGE, YOJ, INCOME, HOME_VAL, CAR_AGE, JOB
complete=data.frame(c(data[4:4],data[6:6],data[9:9],data[11:13],data[15:24],data[26:26]))
str(complete)
summary(complete)

#AGE
tree1=data.frame(c(data[5:5],complete))
str(tree1)
summary(tree1)
fit1=rpart(tree1$AGE~.,method='anova',data=tree1)
par(mfrow=c(1,1))
rpart.plot(fit1,main='AGE Decision Tree')

#YOJ
tree2=data.frame(c(data[7:7],complete))
str(tree2)
summary(tree2)
fit2=rpart(tree2$YOJ~.,method='anova',data=tree2)
rpart.plot(fit2,main='YOJ Decision Tree')

#INCOME
tree3=data.frame(c(data[8:8],complete))
str(tree3)
summary(tree3)
fit3=rpart(tree3$INCOME~.,method='anova',data=tree3)
rpart.plot(fit3,main='INCOME Decision Tree')

#HOME_VAL
tree4=data.frame(c(data[10:10],complete))
str(tree4)
summary(tree4)
fit4=rpart(tree4$HOME_VAL~.,method='anova',data=tree4)
rpart.plot(fit4,main='HOME_VAL Decision Tree')

#CAR_AGE
tree5=data.frame(c(data[25:25],complete))
str(tree5)
summary(tree5)
fit5=rpart(tree5$CAR_AGE~.,method='anova',data=tree5)
rpart.plot(fit5,main='CAR_AGE Decision Tree')

#JOB
tree6=data.frame(c(data[14:14],complete))
str(tree6)
summary(tree6)
fit6=rpart(tree6$JOB~.,method='class',data=tree6) 
#by using method='class' force R to give exact predictions instead of probability
rpart.plot(fit6,main='JOB Decision Tree')

#TARGET_FLAG
tree7=data.frame(c(data[2:2],data[4:26]))
fit=rpart(tree7$TARGET_FLAG~.,method='class',data=tree7)
#by using method='class' force R to give exact predictions instead of probability
rpart.plot(fit,main='TARGET_FLAG Decision Tree')

#this concludes the decision tree bingo bonus
#to continue with the next bingo bonus, run ALL train and test code
#roll the dice - severity model
subdata=subset(newdata,newdata$TARGET_FLAG=='1')
str(subdata)
summary(subdata)
submodel=lm(subdata$TARGET_AMT~subdata$IMP_AGE+subdata$IMP_YOJ+subdata$IMP_INCOME+subdata$IMP_HOME_VAL+subdata$TRAVTIME+subdata$BLUEBOOK+subdata$TIF+subdata$OLDCLAIM+subdata$CLM_FREQ+subdata$MVR_PTS+subdata$IMP_CAR_AGE)
summary(submodel)
submodel=stepAIC(submodel,direction='both')
summary(submodel)
subpredict=3923.11001+subdata$BLUEBOOK*0.12830+subdata$MVR_PTS*151.4049+subdata$IMP_CAR_AGE*-49.46889
str(subpredict)
summary(subpredict)
mean(subdata$TARGET_AMT)
subpredict1=3923.11001+newtest$BLUEBOOK*0.12830+newtest$MVR_PTS*151.4049+newtest$IMP_CAR_AGE*-49.46889
str(subpredict1)
summary(subpredict1)

#probit model
str(newdata)
newdata=data.frame(c(newdata[2:2],newdata[4:37]))
str(newdata)
probit=glm(TARGET_FLAG~.,family=binomial(link='probit'),data=newdata)
summary(probit)
probit=glm(TARGET_FLAG~KIDSDRIV+HOMEKIDS+  PARENT1+  MSTATUS+EDUCATION+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+OLDCLAIM+  REVOKED+MVR_PTS+URBANICITY+M_AGE+  IMP_INCOME+IMP_JOB+Z_INCOME+  Z_HOME_VAL+  Z_CLM_FREQ,family=binomial(link='probit'),data=newdata)
summary(probit)







