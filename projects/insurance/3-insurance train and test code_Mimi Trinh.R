#Mimi Trinh
#Unit 2 Insurance Project Train (line 1-460) and Test Code (line 461-end)
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

setwd('/Users/mimitrinh/Desktop')
data=read.csv('logit_insurance.csv')

#section 1: data exploration
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

#y-variables
par(mfrow=c(1,1))
plot(data$TARGET_FLAG,col='blue',main='TARGET_FLAG Distribution',xlab='0 = No Car Crash, 1 = Car Crash')
summary(data$TARGET_FLAG)
par(mfrow=c(1,2))
hist(data$TARGET_AMT,col='blue',main='TARGET_AMT Histogram',xlab='TARGET_AMT')
boxplot(data$TARGET_AMT,col='red',main='TARGET_AMT Boxplot')
summary(data$TARGET_AMT)

#x-variables categorical
par(mfrow=c(1,4))
plot(data$KIDSDRIV,col='blue',main='KIDSDRIV Distribution',xlab='0 = teenage drivers, 1 = adult drivers')
plot(data$HOMEKIDS,col='red',main='HOMEKIDS Distribution',xlab='0 = no children at home, 1 = children at home')
plot(data$PARENT1,col='green',main='PARENTS1 Distribution',xlab='No = not single parent, Yes = single parent')
plot(data$MSTATUS,col='yellow',main='MSTATUS Distribution',xlab='Yes = married, No = single')
summary(data$KIDSDRIV)
summary(data$HOMEKIDS)
summary(data$PARENT1)
summary(data$MSTATUS)
par(mfrow=c(2,1))
plot(data$SEX,col='blue',main='SEX Distribution')
plot(data$EDUCATION,col='red',main='EDUCATION Distribution')
summary(data$SEX)
summary(data$EDUCATION)
par(mfrow=c(1,1))
plot(data$JOB,col='blue',main='JOB Distribution')
summary(data$JOB)
par(mfrow=c(2,1))
plot(data$CAR_USE,col='blue',main='CAR_USE Distribution')
plot(data$CAR_TYPE,col='red',main='CAR_TYPE Distribution')
summary(data$CAR_USE)
summary(data$CAR_TYPE)
par(mfrow=c(1,3))
plot(data$RED_CAR,col='blue',main='RED_CAR Distribution')
plot(data$REVOKED,col='red',main='REVOKED Distribution')
plot(data$URBANICITY,col='green',main='URBANICITY Distribution')
summary(data$RED_CAR)
summary(data$REVOKED)
summary(data$URBANICITY)

#x-variables numeric
skewness(data$AGE,na.exclude(data$AGE)) #no outlier issue
skewness(data$YOJ,na.exclude(data$YOJ)) #small outlier issue
skewness(data$INCOME,na.exclude(data$INCOME)) #small outlier issue
skewness(data$HOME_VAL,na.exclude(data$HOME_VAL)) 
skewness(data$TRAVTIME) #no outlier issue
skewness(data$BLUEBOOK) #no outlier issue
skewness(data$TIF) #no outlier issue
skewness(data$OLDCLAIM) #big outlier issue
skewness(data$CLM_FREQ) #small outlier issue
skewness(data$MVR_PTS) #small outlier issue
skewness(data$CAR_AGE,na.exclude(data$CAR_AGE)) #no outlier issue
#small outlier issues: YOJ, INCOME, HOME_VAL (charts below), TRAVTIME (charts below) 
#BLUEBOOK (charts below), TIF(charts below), CLM_FREQ, MVR_PTS
#big outlier issue: OLDCLAIM
par(mfrow=c(1,4))
hist(data$AGE,col='blue',main='AGE Histogram',xlab='AGE')
boxplot(data$AGE,col='red',main='AGE Boxplot')
hist(data$YOJ,col='green',main='YOJ Histogram',xlab='YOJ')
boxplot(data$YOJ,col='yellow',main='YOJ Boxplot')
par(mfrow=c(1,4))
hist(data$INCOME,col='blue',main='INCOME Histogram',xlab='INCOME')
boxplot(data$INCOME,col='red',main='INCOME Boxplot')
hist(data$HOME_VAL,col='green',main='HOME_VAL Histogram',xlab='HOME_VAL')
boxplot(data$HOME_VAL,col='yellow',main='HOME_VAL Boxplot')
par(mfrow=c(1,4))
hist(data$TRAVTIME,col='blue',main='TRAVTIME Histogram',xlab='TRAVTIME')
boxplot(data$TRAVTIME,col='red',main='TRAVTIME Boxplot')
hist(data$BLUEBOOK,col='green',main='BLUEBOOK Histogram',xlab='BLUEBOOK')
boxplot(data$BLUEBOOK,col='yellow',main='BLUEBOOK Boxplot')
par(mfrow=c(1,4))
hist(data$TIF,col='blue',main='TIF Histogram',xlab='TIF')
boxplot(data$TIF,col='red',main='TIF Boxplot')
hist(data$OLDCLAIM,col='green',main='OLDCLAIM Histogram',xlab='OLDCLAIM')
boxplot(data$OLDCLAIM,col='yellow',main='OLDCLAIM Boxplot')
par(mfrow=c(1,4))
hist(data$CLM_FREQ,col='blue',main='CLM_FREQ Histogram',xlab='CLM_FREQ')
boxplot(data$CLM_FREQ,col='red',main='CLM_FREQ Boxplot')
hist(data$MVR_PTS,col='green',main='MVR_PTS Histogram',xlab='MVR_PTS')
boxplot(data$MVR_PTS,col='yellow',main='MVR_PTS Boxplot')
par(mfrow=c(1,2))
hist(data$CAR_AGE,col='blue',main='CAR_AGE Histogram',xlab='CAR_AGE')
boxplot(data$CAR_AGE,col='red',main='CAR_AGE Boxplot')
#from histograms and boxplots, AGE and CAR_AGE don't have outlier issues
#OLDCLAIM has serious outlier issue
#remaining x-variables numeric have moderate outlier issues

#correlation
par(mfrow=c(1,1))
numeric=subset(data,select=c(TARGET_AMT,AGE,YOJ,INCOME,HOME_VAL,TRAVTIME,BLUEBOOK,TIF,OLDCLAIM,CLM_FREQ,MVR_PTS,CAR_AGE),na.rm=TRUE)
numeric=numeric[complete.cases(numeric),]
correl=cor(numeric)
corrplot(correl,method='square')

#section 2: data preparation
#variables with missing values: AGE, YOJ, INCOME, HOME_VAL, JOB, CAR_AGE
#missing value AGE
#replace missing values with mean b/c no outlier issue
mean_AGE=mean(data$AGE,na.rm=TRUE)
mean_AGE
data$IMP_AGE=data$AGE
data$IMP_AGE[is.na(data$IMP_AGE)]=mean_AGE
str(data$IMP_AGE)
summary(data$IMP_AGE)
data$M_AGE=data$AGE
data$M_AGE[data$M_AGE>=0]=0
data$M_AGE[is.na(data$M_AGE)]=1
str(data$M_AGE)
summary(data$M_AGE)

#missing value YOJ
#replace missing values with median b/c outlier issue
median_YOJ=median(data$YOJ,na.rm=TRUE)
median_YOJ
data$IMP_YOJ=data$YOJ
data$IMP_YOJ[is.na(data$IMP_YOJ)]=median_YOJ
str(data$IMP_YOJ)
summary(data$IMP_YOJ)
data$M_YOJ=data$YOJ
data$M_YOJ[data$M_YOJ>=0]=0
data$M_YOJ[is.na(data$M_YOJ)]=1
str(data$M_YOJ)
summary(data$M_YOJ)

#missing value INCOME
#replace missing values with median b/c outlier issue
median_INCOME=median(data$INCOME,na.rm=TRUE)
median_INCOME
data$IMP_INCOME=data$INCOME
data$IMP_INCOME[is.na(data$IMP_INCOME)]=median_INCOME
str(data$IMP_INCOME)
summary(data$IMP_INCOME)
data$M_INCOME=data$INCOME
data$M_INCOME[data$M_INCOME>=0]=0
data$M_INCOME[is.na(data$M_INCOME)]=1
str(data$M_INCOME)
summary(data$M_INCOME)

#missing value HOME_VAL
#replace missing values with median b/c outlier issue
median_HOME_VAL=median(data$HOME_VAL,na.rm=TRUE)
median_HOME_VAL
data$IMP_HOME_VAL=data$HOME_VAL
data$IMP_HOME_VAL[is.na(data$IMP_HOME_VAL)]=median_HOME_VAL
str(data$IMP_HOME_VAL)
summary(data$IMP_HOME_VAL)
data$M_HOME_VAL=data$HOME_VAL
data$M_HOME_VAL[data$M_HOME_VAL>=0]=0
data$M_HOME_VAL[is.na(data$M_HOME_VAL)]=1
str(data$M_HOME_VAL)
summary(data$M_HOME_VAL)

#missing value JOB
#replace missing value with "Unknown" level
data$IMP_JOB=data$JOB
levels(data$IMP_JOB)=c(levels(data$IMP_JOB),'Unknown')
data$IMP_JOB[data$IMP_JOB=='']="Unknown"
data$IMP_JOB=as.factor(as.character(data$IMP_JOB))
summary(data$IMP_JOB)

#missing value CAR_AGE
#replace missing values with mean b/c no outlier issue
mean_CAR_AGE=mean(data$CAR_AGE,na.rm=TRUE)
mean_CAR_AGE
data$IMP_CAR_AGE=data$CAR_AGE
data$IMP_CAR_AGE[is.na(data$IMP_CAR_AGE)]=mean_CAR_AGE
str(data$IMP_CAR_AGE)
summary(data$IMP_CAR_AGE)
data$M_CAR_AGE=data$CAR_AGE
data$M_CAR_AGE[data$M_CAR_AGE>=0]=0
data$M_CAR_AGE[is.na(data$M_CAR_AGE)]=1
str(data$M_CAR_AGE)
summary(data$M_CAR_AGE)

#putting all new variables together in data frame 
newdata=data.frame(data[1:4],data[6:6],data[9:9],data[11:13],data[15:24],data[26:37])
str(newdata)
summary(newdata)

#variables with moderate outlier issues: YOJ, INCOME, HOME_VAL, TRAVTIME
#BLUEBOOK, TIF, CLM_FREQ, MVR_PTS
#variables with serious outlier issue: OLDCLAIM
#important to know variables with 0 value to predict TARGET_FLAG, so create new variables for this
#outlier IMP_YOJ
#trim 90% lower level b/c still have outliers after 95% trim lower level
newdata$Z_YOJ=newdata$IMP_YOJ
newdata$Z_YOJ=ifelse(newdata$Z_YOJ==0,0,1)
newdata$Z_YOJ=factor(newdata$Z_YOJ)
str(newdata$Z_YOJ)
summary(newdata$Z_YOJ)
per90lower_IMP_YOJ=quantile(newdata$IMP_YOJ,0.10)
per90lower_IMP_YOJ
newdata$IMP_YOJ[newdata$IMP_YOJ<per90lower_IMP_YOJ]=per90lower_IMP_YOJ
skewness(newdata$IMP_YOJ)
skewness(data$IMP_YOJ)
par(mfrow=c(1,2))
boxplot(data$IMP_YOJ,col='blue',main='IMP_YOJ before 90% trim')
boxplot(newdata$IMP_YOJ,col='red',main='IMP_YOJ after 90% trim')

#outlier IMP_INCOME
#trim 95% upper level
newdata$Z_INCOME=newdata$IMP_INCOME
newdata$Z_INCOME=ifelse(newdata$Z_INCOME==0,0,1)
newdata$Z_INCOME=factor(newdata$Z_INCOME)
str(newdata$Z_INCOME)
summary(newdata$Z_INCOME)
per95upper_IMP_INCOME=quantile(newdata$IMP_INCOME,0.95)
per95upper_IMP_INCOME
newdata$IMP_INCOME[newdata$IMP_INCOME>per95upper_IMP_INCOME]=per95upper_IMP_INCOME
skewness(newdata$IMP_INCOME)
skewness(data$IMP_INCOME)
par(mfrow=c(1,2))
boxplot(data$IMP_INCOME,col='blue',main='IMP_INCOME before 95% trim')
boxplot(newdata$IMP_INCOME,col='red',main='IMP_INCOME after 95% trim')

#outlier IMP_HOME_VAL
#trim 99% upper level
newdata$Z_HOME_VAL=newdata$IMP_HOME_VAL
newdata$Z_HOME_VAL=ifelse(newdata$Z_HOME_VAL==0,0,1)
newdata$Z_HOME_VAL=factor(newdata$Z_HOME_VAL)
str(newdata$Z_HOME_VAL)
summary(newdata$Z_HOME_VAL)
per99upper_IMP_HOME_VAL=quantile(newdata$IMP_HOME_VAL,0.99)
per99upper_IMP_HOME_VAL
newdata$IMP_HOME_VAL[newdata$IMP_HOME_VAL>per99upper_IMP_HOME_VAL]=per99upper_IMP_HOME_VAL
skewness(newdata$IMP_HOME_VAL)
skewness(data$IMP_HOME_VAL)
par(mfrow=c(1,2))
boxplot(data$IMP_HOME_VAL,col='blue',main='IMP_HOME_VAL before 99% trim')
boxplot(newdata$IMP_HOME_VAL,col='red',main='IMP_HOME_VAL after 99% trim')

#outlier TRAVTIME
#trim 99% upper level
per99upper_TRAVTIME=quantile(newdata$TRAVTIME,0.99)
per99upper_TRAVTIME
newdata$TRAVTIME[newdata$TRAVTIME>per99upper_TRAVTIME]=per99upper_TRAVTIME
skewness(newdata$TRAVTIME)
skewness(data$TRAVTIME)
par(mfrow=c(1,2))
boxplot(data$TRAVTIME,col='blue',main='TRAVTIME before 99% trim')
boxplot(newdata$TRAVTIME,col='red',main='TRAVTIME after 99% trim')

#outlier BLUEBOOK
#trim 95% upper level
per95upper_BLUEBOOK=quantile(newdata$BLUEBOOK,0.95)
per95upper_BLUEBOOK
newdata$BLUEBOOK[newdata$BLUEBOOK>per95upper_BLUEBOOK]=per95upper_BLUEBOOK
skewness(newdata$BLUEBOOK)
skewness(data$BLUEBOOK)
par(mfrow=c(1,2))
boxplot(data$BLUEBOOK,col='blue',main='BLUEBOOK before 95% trim')
boxplot(newdata$BLUEBOOK,col='red',main='BLUEBOOK after 95% trim')

#outlier TIF
#trim 95% upper level
per95upper_TIF=quantile(newdata$TIF,0.95)
per95upper_TIF
newdata$TIF[newdata$TIF>per95upper_TIF]=per95upper_TIF
skewness(newdata$TIF)
skewness(data$TIF)
par(mfrow=c(1,2))
boxplot(data$TIF,col='blue',main='TIF before 95% trim')
boxplot(newdata$TIF,col='red',main='TIF after 95% trim')

#outlier CLM_FREQ
#trim 95% upper level
newdata$Z_CLM_FREQ=newdata$CLM_FREQ
newdata$Z_CLM_FREQ=ifelse(newdata$Z_CLM_FREQ==0,0,1)
newdata$Z_CLM_FREQ=factor(newdata$Z_CLM_FREQ)
str(newdata$Z_CLM_FREQ)
summary(newdata$Z_CLM_FREQ)
per95upper_CLM_FREQ=quantile(newdata$CLM_FREQ,0.95)
per95upper_CLM_FREQ
newdata$CLM_FREQ[newdata$CLM_FREQ>per95upper_CLM_FREQ]=per95upper_CLM_FREQ
skewness(newdata$CLM_FREQ)
skewness(data$CLM_FREQ)
par(mfrow=c(1,2))
boxplot(data$CLM_FREQ,col='blue',main='CLM_FREQ before 95% trim')
boxplot(newdata$CLM_FREQ,col='red',main='CLM_FREQ after 95% trim')
#there's no outier in before trim and value ranges from 0 to 5, so keep this variable the way it is
newdata$CLM_FREQ=data$CLM_FREQ

#outlier MVR_PTS
#trim 95% upper level
newdata$Z_MVR_PTS=newdata$MVR_PTS
newdata$Z_MVR_PTS=ifelse(newdata$Z_MVR_PTS==0,0,1)
newdata$Z_MVR_PTS=factor(newdata$Z_MVR_PTS)
str(newdata$Z_MVR_PTS)
summary(newdata$Z_MVR_PTS)
per95upper_MVR_PTS=quantile(newdata$MVR_PTS,0.95)
per95upper_MVR_PTS
newdata$MVR_PTS[newdata$MVR_PTS>per95upper_MVR_PTS]=per95upper_MVR_PTS
skewness(newdata$MVR_PTS)
skewness(data$MVR_PTS)
par(mfrow=c(1,2))
boxplot(data$MVR_PTS,col='blue',main='MVR_PTS before 95% trim')
boxplot(newdata$MVR_PTS,col='red',main='MVR_PTS after 95% trim')

#outlier OLDCLAIM
#trim 90% upper level
newdata$Z_OLDCLAIM=newdata$OLDCLAIM
newdata$Z_OLDCLAIM=ifelse(newdata$Z_OLDCLAIM==0,0,1)
newdata$Z_OLDCLAIM=factor(newdata$Z_OLDCLAIM)
str(newdata$Z_OLDCLAIM)
summary(newdata$Z_OLDCLAIM)
per90upper_OLDCLAIM=quantile(newdata$OLDCLAIM,0.90)
per90upper_OLDCLAIM
newdata$OLDCLAIM[newdata$OLDCLAIM>per90upper_OLDCLAIM]=per90upper_OLDCLAIM
skewness(newdata$OLDCLAIM)
skewness(data$OLDCLAIM)
par(mfrow=c(1,2))
boxplot(data$OLDCLAIM,col='blue',main='OLDCLAIM beofre 90% trim')
boxplot(newdata$OLDCLAIM,col='red',main='OLDCLAIM after 90% trim')

str(newdata)
summary(newdata)

#section 3: model development
#model 1: full model
model1=glm(newdata$TARGET_FLAG~.,data=newdata,family=binomial())
summary(model1)
newdata$predict1=predict(model1,type='response')
str(newdata$predict1)
summary(newdata$predict1)
#doesn't have any good results here, so need to break out the model in smaller models

#model 2: model with only original x-var
names(newdata)
model2=glm(newdata$TARGET_FLAG~KIDSDRIV+HOMEKIDS+PARENT1+MSTATUS+SEX+EDUCATION+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+RED_CAR+OLDCLAIM+CLM_FREQ+REVOKED+MVR_PTS+URBANICITY,data=newdata,family=binomial())
summary(model2)
#these x-vars not 90% significant, so remove from model: SEX, RED_CAR, OLDCLAIM
model2=glm(newdata$TARGET_FLAG~KIDSDRIV+HOMEKIDS+PARENT1+MSTATUS+EDUCATION+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+CLM_FREQ+REVOKED+MVR_PTS+URBANICITY,data=newdata,family=binomial())
summary(model2)
newdata$predict2=predict(model2,type='response')
str(newdata$predict2)
summary(newdata$predict2)

#model 3: model with remaining variables
names(newdata)
model3=glm(newdata$TARGET_FLAG~IMP_AGE+M_AGE+IMP_YOJ+M_YOJ+IMP_INCOME+M_INCOME+IMP_HOME_VAL+M_HOME_VAL+IMP_JOB+IMP_CAR_AGE+M_CAR_AGE+Z_YOJ+Z_INCOME+Z_HOME_VAL+Z_CLM_FREQ+Z_MVR_PTS+Z_OLDCLAIM,data=newdata,family=binomial())
summary(model3)
#remove x-vars not 90% significant, we have the following model
model3=glm(newdata$TARGET_FLAG~IMP_AGE+M_AGE+IMP_INCOME+IMP_JOB+Z_HOME_VAL+Z_CLM_FREQ+Z_MVR_PTS+Z_OLDCLAIM,data=newdata,family=binomial())
summary(model3)
#Z_OLDCLAIM has NA in model 3 b/c it has multicollinearity with other x-vars, so remove it and get this
model3=glm(newdata$TARGET_FLAG~IMP_AGE+M_AGE+IMP_INCOME+IMP_JOB+Z_HOME_VAL+Z_CLM_FREQ+Z_MVR_PTS,data=newdata,family=binomial())
summary(model3)
newdata$predict3=predict(model3,type='response')
str(newdata$predict3)
summary(newdata$predict3)

#model 4: put model #2 and #3 together
model4=glm(TARGET_FLAG~KIDSDRIV+HOMEKIDS+PARENT1+MSTATUS+EDUCATION+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+CLM_FREQ+REVOKED+MVR_PTS+URBANICITY+IMP_AGE+M_AGE+IMP_INCOME+IMP_JOB+Z_HOME_VAL+Z_CLM_FREQ+Z_MVR_PTS,data=newdata,family=binomial())
summary(model4)
#some x-var no longer significant, probably due to multicollinearity
#remove x-var not 90% significant, have the following model: CLM_FREQ,IMP_AGE,Z_MVR_PTS
model4=glm(TARGET_FLAG~KIDSDRIV+HOMEKIDS+PARENT1+MSTATUS+EDUCATION+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+REVOKED+MVR_PTS+URBANICITY+M_AGE+IMP_INCOME+IMP_JOB+Z_HOME_VAL+Z_CLM_FREQ,data=newdata,family=binomial())
summary(model4)
newdata$predict4=predict(model4,type='response')
str(newdata$predict4)
summary(newdata$predict4)

#section 4: model selection
AIC1=AIC(model1)
AIC2=AIC(model2)
AIC3=AIC(model3)
AIC4=AIC(model4)
BIC1=BIC(model1)
BIC2=BIC(model2)
BIC3=BIC(model3)
BIC4=BIC(model4)   
LL1=-2*logLik(model1,REML=TRUE)
LL2=-2*logLik(model2,REML=TRUE)
LL3=-2*logLik(model3,REML=TRUE)
LL4=-2*logLik(model4,REML=TRUE)
KS1=ks_stat(actuals=newdata$TARGET_FLAG,predictedScores=newdata$predict1)
KS2=ks_stat(actuals=newdata$TARGET_FLAG,predictedScores=newdata$predict2)
KS3=ks_stat(actuals=newdata$TARGET_FLAG,predictedScores=newdata$predict3)
KS4=ks_stat(actuals=newdata$TARGET_FLAG,predictedScores=newdata$predict4)
AIC=round(c(AIC1,AIC2,AIC3,AIC4),2)
BIC=round(c(BIC1,BIC2,BIC3,BIC4),2)
LL=round(c(LL1,LL2,LL3,LL4),2)
KS=round(c(KS1,KS2,KS3,KS4),2)
table=rbind(AIC,BIC,LL,KS)
colnames(table)=c('model 1','model 2','model 3','model 4')
table
par(mfrow=c(1,1))
plot(roc(newdata$TARGET_FLAG,newdata$predict1),col='blue',main='ROC Curve Model 1',ylim=c(0,1),print.auc=TRUE)
par(mfrow=c(1,1))
plot(roc(newdata$TARGET_FLAG,newdata$predict2),col='red',main='ROC Curve Model 2',ylim=c(0,1),print.auc=TRUE)
par(mfrow=c(1,1))
plot(roc(newdata$TARGET_FLAG,newdata$predict3),col='brown',main='ROC Curve Model 3',ylim=c(0,1),print.auc=TRUE)
par(mfrow=c(1,1))
plot(roc(newdata$TARGET_FLAG,newdata$predict4),col='purple',main='ROC Curve Model 4',ylim=c(0,1),print.auc=TRUE)
#choose model 4

#section 5: predictive model to forecast TARGET_AMT 
str(newdata)
summary(newdata)
ols=lm(newdata$TARGET_AMT~newdata$TRAVTIME+newdata$BLUEBOOK+newdata$TIF+newdata$OLDCLAIM+newdata$CLM_FREQ+newdata$MVR_PTS+newdata$IMP_AGE+newdata$IMP_YOJ+newdata$IMP_INCOME+newdata$IMP_HOME_VAL)
summary(ols)
ols=stepAIC(ols,direction='both')
summary(ols)
formula=1.715e+03+
  newdata$TRAVTIME*7.609e+00+  
  newdata$BLUEBOOK*1.498e-02+  
  newdata$TIF*-4.998e+01+
  newdata$CLM_FREQ*2.933e+02+
  newdata$MVR_PTS*2.229e+02+
  newdata$IMP_AGE*-1.202e+01+  
  newdata$IMP_INCOME*-2.385e-03+ 
  newdata$IMP_HOME_VAL*-2.271e-03
str(formula)
summary(formula)

#Unit 2 Insurance Project Test Code

setwd('/Users/mimitrinh/Desktop')
test=read.csv('logit_insurance_test.csv')

#section 1: test preparation
str(test)
summary(test) #need to fix the following variables to the correct format in R
#need to fix missing values in section 2: AGE, YOJ, INCOME, HOME_VAL, JOB, CAR_AGE
test$TARGET_FLAG=factor(test$TARGET_FLAG) #convert 2-level numeric to 2-level nominal variable
summary(test$TARGET_FLAG)
test$KIDSDRIV=factor(ifelse(test$KIDSDRIV>0,1,0)) #convert 5-level numeric to 2-level nominal
summary(test$KIDSDRIV)
test$HOMEKIDS=factor(ifelse(test$HOMEKIDS>0,1,0)) #convert 6-level numeric to 2-level nominal
summary(test$HOMEKIDS)
test$INCOME=suppressWarnings(as.numeric(gsub('[^0-9.]','',test$INCOME))) #convert nominal to numeric 
summary(test$INCOME)
test$HOME_VAL=suppressWarnings(as.numeric(gsub('[^0-9.]','',test$HOME_VAL))) #convert nominal to numeric 
summary(test$HOME_VAL)
test$BLUEBOOK=suppressWarnings(as.numeric(gsub('[^0-9.]','',test$BLUEBOOK))) #convert nominal to numeric 
summary(test$BLUEBOOK)
test$OLDCLAIM=suppressWarnings(as.numeric(gsub('[^0-9.]','',test$OLDCLAIM))) #convert nominal to numeric 
summary(test$OLDCLAIM)
test$CAR_AGE[test$CAR_AGE<0]=NA #can't have negative car age, so change that value to NA
summary(test$CAR_AGE)
str(test) #good format now
summary(test)

#section 2: model
#variables with missing values: AGE, YOJ, INCOME, HOME_VAL, JOB, CAR_AGE
#missing value AGE
#replace missing values with mean b/c no outlier issue
mean_AGE=mean(test$AGE,na.rm=TRUE)
mean_AGE
test$IMP_AGE=test$AGE
test$IMP_AGE[is.na(test$IMP_AGE)]=mean_AGE
str(test$IMP_AGE)
summary(test$IMP_AGE)
test$M_AGE=test$AGE
test$M_AGE[test$M_AGE>=0]=0
test$M_AGE[is.na(test$M_AGE)]=1
str(test$M_AGE)
summary(test$M_AGE)

#missing value YOJ
#replace missing values with median b/c outlier issue
median_YOJ=median(test$YOJ,na.rm=TRUE)
median_YOJ
test$IMP_YOJ=test$YOJ
test$IMP_YOJ[is.na(test$IMP_YOJ)]=median_YOJ
str(test$IMP_YOJ)
summary(test$IMP_YOJ)
test$M_YOJ=test$YOJ
test$M_YOJ[test$M_YOJ>=0]=0
test$M_YOJ[is.na(test$M_YOJ)]=1
str(test$M_YOJ)
summary(test$M_YOJ)

#missing value INCOME
#replace missing values with median b/c outlier issue
median_INCOME=median(test$INCOME,na.rm=TRUE)
median_INCOME
test$IMP_INCOME=test$INCOME
test$IMP_INCOME[is.na(test$IMP_INCOME)]=median_INCOME
str(test$IMP_INCOME)
summary(test$IMP_INCOME)
test$M_INCOME=test$INCOME
test$M_INCOME[test$M_INCOME>=0]=0
test$M_INCOME[is.na(test$M_INCOME)]=1
str(test$M_INCOME)
summary(test$M_INCOME)

#missing value HOME_VAL
#replace missing values with median b/c outlier issue
median_HOME_VAL=median(test$HOME_VAL,na.rm=TRUE)
median_HOME_VAL
test$IMP_HOME_VAL=test$HOME_VAL
test$IMP_HOME_VAL[is.na(test$IMP_HOME_VAL)]=median_HOME_VAL
str(test$IMP_HOME_VAL)
summary(test$IMP_HOME_VAL)
test$M_HOME_VAL=test$HOME_VAL
test$M_HOME_VAL[test$M_HOME_VAL>=0]=0
test$M_HOME_VAL[is.na(test$M_HOME_VAL)]=1
str(test$M_HOME_VAL)
summary(test$M_HOME_VAL)

#missing value JOB
#replace missing value with "Unknown" level
test$IMP_JOB=test$JOB
levels(test$IMP_JOB)=c(levels(test$IMP_JOB),'Unknown')
test$IMP_JOB[test$IMP_JOB=='']="Unknown"
test$IMP_JOB=as.factor(as.character(test$IMP_JOB))
summary(test$IMP_JOB)

#missing value CAR_AGE
#replace missing values with mean b/c no outlier issue
mean_CAR_AGE=mean(test$CAR_AGE,na.rm=TRUE)
mean_CAR_AGE
test$IMP_CAR_AGE=test$CAR_AGE
test$IMP_CAR_AGE[is.na(test$IMP_CAR_AGE)]=mean_CAR_AGE
str(test$IMP_CAR_AGE)
summary(test$IMP_CAR_AGE)
test$M_CAR_AGE=test$CAR_AGE
test$M_CAR_AGE[test$M_CAR_AGE>=0]=0
test$M_CAR_AGE[is.na(test$M_CAR_AGE)]=1
str(test$M_CAR_AGE)
summary(test$M_CAR_AGE)

#putting all new variables together in test frame 
newtest=data.frame(test[1:4],test[6:6],test[9:9],test[11:13],test[15:24],test[26:37])
str(newtest)
summary(newtest)

#variables with moderate outlier issues: YOJ, INCOME, HOME_VAL, TRAVTIME
#BLUEBOOK, TIF, CLM_FREQ, MVR_PTS
#variables with serious outlier issue: OLDCLAIM
#important to know variables with 0 value to predict TARGET_FLAG, so create new variables for this
#outlier IMP_YOJ
#trim 90% lower level b/c still have outliers after 95% trim lower level
newtest$Z_YOJ=newtest$IMP_YOJ
newtest$Z_YOJ=ifelse(newtest$Z_YOJ==0,0,1)
newtest$Z_YOJ=factor(newtest$Z_YOJ)
str(newtest$Z_YOJ)
summary(newtest$Z_YOJ)
per90lower_IMP_YOJ=quantile(newtest$IMP_YOJ,0.10)
per90lower_IMP_YOJ
newtest$IMP_YOJ[newtest$IMP_YOJ<per90lower_IMP_YOJ]=per90lower_IMP_YOJ
skewness(newtest$IMP_YOJ)
skewness(test$IMP_YOJ)
par(mfrow=c(1,2))
boxplot(test$IMP_YOJ,col='blue',main='IMP_YOJ before 90% trim')
boxplot(newtest$IMP_YOJ,col='red',main='IMP_YOJ after 90% trim')

#outlier IMP_INCOME
#trim 95% upper level
newtest$Z_INCOME=newtest$IMP_INCOME
newtest$Z_INCOME=ifelse(newtest$Z_INCOME==0,0,1)
newtest$Z_INCOME=factor(newtest$Z_INCOME)
str(newtest$Z_INCOME)
summary(newtest$Z_INCOME)
per95upper_IMP_INCOME=quantile(newtest$IMP_INCOME,0.95)
per95upper_IMP_INCOME
newtest$IMP_INCOME[newtest$IMP_INCOME>per95upper_IMP_INCOME]=per95upper_IMP_INCOME
skewness(newtest$IMP_INCOME)
skewness(test$IMP_INCOME)
par(mfrow=c(1,2))
boxplot(test$IMP_INCOME,col='blue',main='IMP_INCOME before 95% trim')
boxplot(newtest$IMP_INCOME,col='red',main='IMP_INCOME after 95% trim')

#outlier IMP_HOME_VAL
#trim 99% upper level
newtest$Z_HOME_VAL=newtest$IMP_HOME_VAL
newtest$Z_HOME_VAL=ifelse(newtest$Z_HOME_VAL==0,0,1)
newtest$Z_HOME_VAL=factor(newtest$Z_HOME_VAL)
str(newtest$Z_HOME_VAL)
summary(newtest$Z_HOME_VAL)
per99upper_IMP_HOME_VAL=quantile(newtest$IMP_HOME_VAL,0.99)
per99upper_IMP_HOME_VAL
newtest$IMP_HOME_VAL[newtest$IMP_HOME_VAL>per99upper_IMP_HOME_VAL]=per99upper_IMP_HOME_VAL
skewness(newtest$IMP_HOME_VAL)
skewness(test$IMP_HOME_VAL)
par(mfrow=c(1,2))
boxplot(test$IMP_HOME_VAL,col='blue',main='IMP_HOME_VAL before 99% trim')
boxplot(newtest$IMP_HOME_VAL,col='red',main='IMP_HOME_VAL after 99% trim')

#outlier TRAVTIME
#trim 99% upper level
per99upper_TRAVTIME=quantile(newtest$TRAVTIME,0.99)
per99upper_TRAVTIME
newtest$TRAVTIME[newtest$TRAVTIME>per99upper_TRAVTIME]=per99upper_TRAVTIME
skewness(newtest$TRAVTIME)
skewness(test$TRAVTIME)
par(mfrow=c(1,2))
boxplot(test$TRAVTIME,col='blue',main='TRAVTIME before 99% trim')
boxplot(newtest$TRAVTIME,col='red',main='TRAVTIME after 99% trim')

#outlier BLUEBOOK
#trim 95% upper level
per95upper_BLUEBOOK=quantile(newtest$BLUEBOOK,0.95)
per95upper_BLUEBOOK
newtest$BLUEBOOK[newtest$BLUEBOOK>per95upper_BLUEBOOK]=per95upper_BLUEBOOK
skewness(newtest$BLUEBOOK)
skewness(test$BLUEBOOK)
par(mfrow=c(1,2))
boxplot(test$BLUEBOOK,col='blue',main='BLUEBOOK before 95% trim')
boxplot(newtest$BLUEBOOK,col='red',main='BLUEBOOK after 95% trim')

#outlier TIF
#trim 95% upper level
per95upper_TIF=quantile(newtest$TIF,0.95)
per95upper_TIF
newtest$TIF[newtest$TIF>per95upper_TIF]=per95upper_TIF
skewness(newtest$TIF)
skewness(test$TIF)
par(mfrow=c(1,2))
boxplot(test$TIF,col='blue',main='TIF before 95% trim')
boxplot(newtest$TIF,col='red',main='TIF after 95% trim')

#outlier CLM_FREQ
#trim 95% upper level
newtest$Z_CLM_FREQ=newtest$CLM_FREQ
newtest$Z_CLM_FREQ=ifelse(newtest$Z_CLM_FREQ==0,0,1)
newtest$Z_CLM_FREQ=factor(newtest$Z_CLM_FREQ)
str(newtest$Z_CLM_FREQ)
summary(newtest$Z_CLM_FREQ)
per95upper_CLM_FREQ=quantile(newtest$CLM_FREQ,0.95)
per95upper_CLM_FREQ
newtest$CLM_FREQ[newtest$CLM_FREQ>per95upper_CLM_FREQ]=per95upper_CLM_FREQ
skewness(newtest$CLM_FREQ)
skewness(test$CLM_FREQ)
par(mfrow=c(1,2))
boxplot(test$CLM_FREQ,col='blue',main='CLM_FREQ before 95% trim')
boxplot(newtest$CLM_FREQ,col='red',main='CLM_FREQ after 95% trim')
#there's no outier in before trim and value ranges from 0 to 5, so keep this variable the way it is
newtest$CLM_FREQ=test$CLM_FREQ

#outlier MVR_PTS
#trim 95% upper level
newtest$Z_MVR_PTS=newtest$MVR_PTS
newtest$Z_MVR_PTS=ifelse(newtest$Z_MVR_PTS==0,0,1)
newtest$Z_MVR_PTS=factor(newtest$Z_MVR_PTS)
str(newtest$Z_MVR_PTS)
summary(newtest$Z_MVR_PTS)
per95upper_MVR_PTS=quantile(newtest$MVR_PTS,0.95)
per95upper_MVR_PTS
newtest$MVR_PTS[newtest$MVR_PTS>per95upper_MVR_PTS]=per95upper_MVR_PTS
skewness(newtest$MVR_PTS)
skewness(test$MVR_PTS)
par(mfrow=c(1,2))
boxplot(test$MVR_PTS,col='blue',main='MVR_PTS before 95% trim')
boxplot(newtest$MVR_PTS,col='red',main='MVR_PTS after 95% trim')

#outlier OLDCLAIM
#trim 90% upper level
newtest$Z_OLDCLAIM=newtest$OLDCLAIM
newtest$Z_OLDCLAIM=ifelse(newtest$Z_OLDCLAIM==0,0,1)
newtest$Z_OLDCLAIM=factor(newtest$Z_OLDCLAIM)
str(newtest$Z_OLDCLAIM)
summary(newtest$Z_OLDCLAIM)
per90upper_OLDCLAIM=quantile(newtest$OLDCLAIM,0.90)
per90upper_OLDCLAIM
newtest$OLDCLAIM[newtest$OLDCLAIM>per90upper_OLDCLAIM]=per90upper_OLDCLAIM
skewness(newtest$OLDCLAIM)
skewness(test$OLDCLAIM)
par(mfrow=c(1,2))
boxplot(test$OLDCLAIM,col='blue',main='OLDCLAIM beofre 90% trim')
boxplot(newtest$OLDCLAIM,col='red',main='OLDCLAIM after 90% trim')

str(newtest)
summary(newtest)

newtest$P_TARGET_FLAG=predict(model4,newdata=newtest,type='response')
str(newtest$P_TARGET_FLAG)
summary(newtest$P_TARGET_FLAG)
newtest$P_TARGET_AMT=1.715e+03+
  newtest$TRAVTIME*7.609e+00+  
  newtest$BLUEBOOK*1.498e-02+  
  newtest$TIF*-4.998e+01+
  newtest$CLM_FREQ*2.933e+02+
  newtest$MVR_PTS*2.229e+02+
  newtest$IMP_AGE*-1.202e+01+  
  newtest$IMP_INCOME*-2.385e-03+ 
  newtest$IMP_HOME_VAL*-2.271e-03
str(newtest$P_TARGET_AMT)
summary(newtest$P_TARGET_AMT)
scores=newtest[c('INDEX','P_TARGET_FLAG','P_TARGET_AMT')]
write.csv(scores,file='CI_Scored_Mimi_Trinh.csv')

