#Mimi Trinh
#unit 3 wine sales project
#train code ends in line 290, test code starts in line 291
setwd('/Users/mimitrinh/Desktop')
wine=read.csv("WINE.csv")

library(ggplot2) 
library(MASS) 
library(pscl) 
library(dplyr) 
library(readr)
library(corrplot)
library(zoo)
library(psych)
library(ROCR)
library(car)
library(InformationValue)
library(rJava)
library(pbkrtest)
library(car)
library(leaps)
library(glm2)
library(aod)
library(moments)

#part 1: data exploration
str(wine)
summary(wine)
#missing value: ResidualSugar, Chlorides, FreeSulfurDioxide
#TotalSulfurDioxide, pH, Sulphates, Alcohol, STARS 

skewness(wine$TARGET)
skewness(wine$FixedAcidity)
skewness(wine$VolatileAcidity)
skewness(wine$CitricAcid)
skewness(wine$ResidualSugar,na.exclude(wine$ResidualSugar))
skewness(wine$Chlorides,na.exclude(wine$Chlorides))
skewness(wine$FreeSulfurDioxide,na.exclude(wine$FreeSulfurDioxide))
skewness(wine$TotalSulfurDioxide,na.exclude(wine$TotalSulfurDioxide))
skewness(wine$Density)
skewness(wine$pH,na.exclude(wine$pH))
skewness(wine$Sulphates,na.exclude(wine$Sulphates))
skewness(wine$Alcohol,na.exclude(wine$Alcohol))
skewness(wine$LabelAppeal)
skewness(wine$AcidIndex)
skewness(wine$STARS,na.exclude(wine$STARS))
#outlier issue (skewness > 1 or < -1): AcidIndex

par(mfrow=c(1,2))
hist(wine$TARGET,col='red',main='TARGET Histogram',xlab='TARGET')
boxplot(wine$TARGET,col='blue',main='TARGET Boxplot')

par(mfrow=c(2,2))
hist(wine$FixedAcidity,col='red',main='FixedAcidity Histogram',xlab='FixedAcidity')
boxplot(wine$FixedAcidity,col='blue',main='FixedAcidity Boxplot')
hist(wine$VolatileAcidity,col='red',main='VolatileAcidity Histogram',xlab='VolatileAcidity')
boxplot(wine$VolatileAcidity,col='blue',main='VolatileAcidity Boxplot')

par(mfrow=c(2,2))
hist(wine$CitricAcid,col='red',main='CitricAcid Histogram',xlab='CitricAcid')
boxplot(wine$CitricAcid,col='blue',main='CitricAcid Boxplot')
hist(wine$ResidualSugar,col='red',main='ResidualSugar Histogram',xlab='ResidualSugar')
boxplot(wine$ResidualSugar,col='blue',main='ResidualSugar Boxplot')

par(mfrow=c(2,2))
hist(wine$Chlorides,col='red',main='Chlorides Histogram',xlab='Chlorides')
boxplot(wine$Chlorides,col='blue',main='Chlorides Boxplot')
hist(wine$FreeSulfurDioxide,col='red',main='FreeSulfurDioxide Histogram',xlab='FreeSulfurDioxide')
boxplot(wine$FreeSulfurDioxide,col='blue',main='FreeSulfurDioxide Boxplot')

par(mfrow=c(2,2))
hist(wine$TotalSulfurDioxide,col='red',main='TotalSulfurDioxide Histogram',xlab='TotalSulfurDioxide')
boxplot(wine$TotalSulfurDioxide,col='blue',main='TotalSulfurDioxide Boxplot')
hist(wine$Density,col='red',main='Density Histogram',xlab='Density')
boxplot(wine$Density,col='blue',main='Density Boxplot')

par(mfrow=c(1,3))
plot(wine$pH,col='green',main='pH',ylab='pH')
hist(wine$Sulphates,col='red',main='Sulphates Histogram',xlab='Sulphates')
boxplot(wine$Sulphates,col='blue',main='Sulphates Boxplot')

par(mfrow=c(2,2))
hist(wine$Alcohol,col='red',main='Alcohol Histogram',xlab='Alcohol')
boxplot(wine$Alcohol,col='blue',main='Alcohol Boxplot')
hist(wine$LabelAppeal,col='red',main='LabelAppeal Histogram',xlab='LabelAppeal')
boxplot(wine$LabelAppeal,col='blue',main='LabelAppeal Boxplot')

par(mfrow=c(2,2))
hist(wine$AcidIndex,col='red',main='AcidIndex Histogram',xlab='AcidIndex')
boxplot(wine$AcidIndex,col='blue',main='AcidIndex Boxplot')
hist(wine$STARS,col='red',main='STARS Histogram',xlab='STARS')
boxplot(wine$STARS,col='blue',main='STARS Boxplot')

par(mfrow=c(1,1))
wine_clean=na.omit(wine)
wine_correl=cor(wine_clean)
corrplot(wine_correl,method='square')

#part 2: data preparation
#missing value: ResidualSugar, Chlorides, FreeSulfurDioxide
#TotalSulfurDioxide, pH, Sulphates, Alcohol, STARS 
#replace all with mean instead of median b/c no outlier issue

wine$M_ResidualSugar=wine$ResidualSugar
wine$IMP_ResidualSugar=wine$ResidualSugar
wine$M_ResidualSugar=ifelse(is.na(wine$M_ResidualSugar)==TRUE,1,0)
wine$IMP_ResidualSugar[which(is.na(wine$IMP_ResidualSugar))]=mean(wine$IMP_ResidualSugar,na.rm=TRUE)
str(wine$M_ResidualSugar)
str(wine$IMP_ResidualSugar)
summary(wine$M_ResidualSugar)
summary(wine$IMP_ResidualSugar)
summary(wine$ResidualSugar)

wine$M_Chlorides=wine$Chlorides
wine$IMP_Chlorides=wine$Chlorides
wine$M_Chlorides=ifelse(is.na(wine$M_Chlorides)==TRUE,1,0)
wine$IMP_Chlorides[which(is.na(wine$IMP_Chlorides))]=mean(wine$IMP_Chlorides,na.rm=TRUE)
str(wine$M_Chlorides)
str(wine$IMP_Chlorides)
summary(wine$M_Chlorides)
summary(wine$IMP_Chlorides)
summary(wine$Chlorides)

wine$M_FreeSulfurDioxide=wine$FreeSulfurDioxide
wine$IMP_FreeSulfurDioxide=wine$FreeSulfurDioxide
wine$M_FreeSulfurDioxide=ifelse(is.na(wine$M_FreeSulfurDioxide)==TRUE,1,0)
wine$IMP_FreeSulfurDioxide[which(is.na(wine$IMP_FreeSulfurDioxide))]=mean(wine$IMP_FreeSulfurDioxide,na.rm=TRUE)
str(wine$M_FreeSulfurDioxide)
str(wine$IMP_FreeSulfurDioxide)
summary(wine$M_FreeSulfurDioxide)
summary(wine$IMP_FreeSulfurDioxide)
summary(wine$FreeSulfurDioxide)

wine$M_TotalSulfurDioxide=wine$TotalSulfurDioxide
wine$IMP_TotalSulfurDioxide=wine$TotalSulfurDioxide
wine$M_TotalSulfurDioxide=ifelse(is.na(wine$M_TotalSulfurDioxide)==TRUE,1,0)
wine$IMP_TotalSulfurDioxide[which(is.na(wine$IMP_TotalSulfurDioxide))]=mean(wine$IMP_TotalSulfurDioxide,na.rm=TRUE)
str(wine$M_TotalSulfurDioxide)
str(wine$IMP_TotalSulfurDioxide)
summary(wine$M_TotalSulfurDioxide)
summary(wine$IMP_TotalSulfurDioxide)
summary(wine$TotalSulfurDioxide)

wine$M_pH=wine$pH
wine$IMP_pH=wine$pH
wine$M_pH=ifelse(is.na(wine$M_pH)==TRUE,1,0)
wine$IMP_pH[which(is.na(wine$IMP_pH))]=mean(wine$IMP_pH,na.rm=TRUE)
str(wine$M_pH)
str(wine$IMP_pH)
summary(wine$M_pH)
summary(wine$IMP_pH)
summary(wine$pH)

wine$M_Sulphates=wine$Sulphates
wine$IMP_Sulphates=wine$Sulphates
wine$M_Sulphates=ifelse(is.na(wine$M_Sulphates)==TRUE,1,0)
wine$IMP_Sulphates[which(is.na(wine$IMP_Sulphates))]=mean(wine$IMP_Sulphates,na.rm=TRUE)
str(wine$M_Sulphates)
str(wine$IMP_Sulphates)
summary(wine$M_Sulphates)
summary(wine$IMP_Sulphates)
summary(wine$Sulphates)

wine$M_Alcohol=wine$Alcohol
wine$IMP_Alcohol=wine$Alcohol
wine$M_Alcohol=ifelse(is.na(wine$M_Alcohol)==TRUE,1,0)
wine$IMP_Alcohol[which(is.na(wine$IMP_Alcohol))]=mean(wine$IMP_Alcohol,na.rm=TRUE)
str(wine$M_Alcohol)
str(wine$IMP_Alcohol)
summary(wine$M_Alcohol)
summary(wine$IMP_Alcohol)
summary(wine$Alcohol)

wine$M_STARS=wine$STARS
wine$IMP_STARS=wine$STARS
wine$M_STARS=ifelse(is.na(wine$M_STARS)==TRUE,1,0)
wine$IMP_STARS[which(is.na(wine$IMP_STARS))]=mean(wine$IMP_STARS,na.rm=TRUE)
str(wine$M_STARS)
str(wine$IMP_STARS)
summary(wine$M_STARS)
summary(wine$IMP_STARS)
summary(wine$STARS)

#outlier: AcidIndex - 95% trim
per95lower=quantile(wine$AcidIndex,0.05)
per95upper=quantile(wine$AcidIndex,0.95)
per95lower
per95upper
skewness(wine$AcidIndex)
summary(wine$AcidIndex)
wine$AcidIndex[wine$AcidIndex<per95lower]=per95lower
wine$AcidIndex[wine$AcidIndex>per95upper]=per95upper
skewness(wine$AcidIndex)
summary(wine$AcidIndex)
#though other variables don't have skewness >1 or <-1, from boxplots,
#some variables have outlier issues on both tails
#this is NOT a big violation assumption, but these outliers may affect
#regression formulas, so we won't trim them now but wait till see 
#results of models and regression formulas and will come back to 
#trim these outliers if necessary

str(wine)
summary(wine)

#putting all new variables in a dataframe to prepare for model building
wine=data.frame(wine[1:5],wine[10:10],wine[14:15],wine[17:32])
str(wine)
summary(wine)

#part 3: model development
#model 1: stepwise multiple linear regression model
model1=lm(wine$TARGET~.,data=wine)
model1=stepAIC(model1,direction='both')
summary(model1)
AIC(model1)

#model 2: poisson regression
model2=glm(wine$TARGET~.,family='poisson'(link='log'),data=wine)
summary(model2)
#eliminate insignificant x-vars
model2=glm(wine$TARGET~wine$VolatileAcidity+wine$LabelAppeal+wine$AcidIndex+wine$IMP_Chlorides+wine$IMP_FreeSulfurDioxide+wine$IMP_TotalSulfurDioxide+wine$IMP_Sulphates+wine$IMP_Alcohol+wine$M_STARS+wine$IMP_STARS,family='poisson'(link='log'),data=wine)
summary(model2)
AIC(model2)

#model 3: negative binomial regression
model3=glm.nb(wine$TARGET~.,data=wine)
summary(model3)
#eliminate insignificant x-vars
model3=glm.nb(wine$TARGET~wine$VolatileAcidity+wine$LabelAppeal+wine$AcidIndex+wine$IMP_Chlorides+wine$IMP_FreeSulfurDioxide+wine$IMP_TotalSulfurDioxide+wine$IMP_Sulphates+wine$IMP_Alcohol+wine$M_STARS+wine$IMP_STARS,data=wine)
summary(model3)
AIC(model3)
#model 3 has same x-vars as model 2 but different models b/c different AIC

#model 4: zero-inflated poisson regression (ZIP)
model4=zeroinfl(wine$TARGET~FixedAcidity+VolatileAcidity+CitricAcid+Density+LabelAppeal+AcidIndex+M_ResidualSugar+IMP_ResidualSugar+M_Chlorides+IMP_Chlorides+M_FreeSulfurDioxide+IMP_FreeSulfurDioxide+M_TotalSulfurDioxide+IMP_TotalSulfurDioxide+M_pH+IMP_pH+M_Sulphates+IMP_Sulphates+M_Alcohol+IMP_Alcohol+M_STARS+IMP_STARS,data=wine)
summary(model4)
#eliminate insignficant x-vars in both table results
model4=zeroinfl(wine$TARGET~wine$VolatileAcidity+wine$LabelAppeal+wine$AcidIndex+wine$IMP_Alcohol+wine$M_STARS+wine$IMP_STARS+wine$IMP_FreeSulfurDioxide+wine$IMP_TotalSulfurDioxide+wine$IMP_pH+wine$IMP_Sulphates,data=wine)
summary(model4)
AIC(model4)

#model 5: zero-inflated negative binomial regression (ZNIB)
model5=zeroinfl(wine$TARGET~FixedAcidity+VolatileAcidity+CitricAcid+Density+LabelAppeal+AcidIndex+M_ResidualSugar+IMP_ResidualSugar+M_Chlorides+IMP_Chlorides+M_FreeSulfurDioxide+IMP_FreeSulfurDioxide+M_TotalSulfurDioxide+IMP_TotalSulfurDioxide+M_pH+IMP_pH+M_Sulphates+IMP_Sulphates+M_Alcohol+IMP_Alcohol+M_STARS+IMP_STARS,data=wine,dist='negbin',EM=TRUE)
summary(model5)
#eliminate insignificant x-vars in both table results
model5=zeroinfl(wine$TARGET~wine$VolatileAcidity+wine$LabelAppeal+wine$AcidIndex+wine$IMP_Alcohol+wine$M_STARS+wine$IMP_STARS+wine$IMP_FreeSulfurDioxide+wine$IMP_TotalSulfurDioxide+wine$IMP_pH+wine$IMP_Sulphates,data=wine,dist='negbin',EM=TRUE)
summary(model5)
AIC(model5)
#model 5 has same x-vars as model 4 but different models b/c different AIC

#models 2 and 3, 4 and 5 have same x-vars with similar results b/c Poisson similar to NB 
#if mean~var, which is true in this case
mean(wine$TARGET)
var(wine$TARGET)

wine$predict1=fitted(model1)
wine$predict2=predict(model2,newdata=wine,type='response')
wine$predict3=predict(model3,newdata=wine,type='response')
wine$predict4=predict(model4,newdata=wine,type='response')
wine$predict5=predict(model5,newdata=wine,type='response')
str(wine)

#part 4: model evaluation
AIC1=AIC(model1)
AIC2=AIC(model2)
AIC3=AIC(model3)
AIC4=AIC(model4)
AIC5=AIC(model5)
MSE1=mean((wine$TARGET-wine$predict1)^2)
MSE2=mean((wine$TARGET-wine$predict2)^2)
MSE3=mean((wine$TARGET-wine$predict3)^2)
MSE4=mean((wine$TARGET-wine$predict4)^2)
MSE5=mean((wine$TARGET-wine$predict5)^2)
SSE1=sum(model1$residuals^2)
SSE2=sum(model2$residuals^2)
SSE3=sum(model3$residuals^2)
SSE4=sum(model4$residuals^2)
SSE5=sum(model5$residuals^2)
AIC=round(c(AIC1,AIC2,AIC3,AIC4,AIC5),2)
MSE=round(c(MSE1,MSE2,MSE3,MSE4,MSE5),4)
SSE=round(c(SSE1,SSE2,SSE3,SSE4,SSE5),2)
table=cbind(AIC,MSE,SSE)
rownames(table)=c('model 1','model 2','model 3','model 4','model 5')
table
#using AIC, model 4 is winner
#using MSE, model 4 and 5 tie
#using SSE, model 2 and 3 tie 
#since analyst doesn't have any industry knowledge, will choose purely based on metrics
#model 4 ZIP is winner

#test code starts here
wine_test=read.csv('wine_test.csv')
str(wine_test)

wine_test$M_ResidualSugar=wine_test$ResidualSugar
wine_test$IMP_ResidualSugar=wine_test$ResidualSugar
wine_test$M_ResidualSugar=ifelse(is.na(wine_test$M_ResidualSugar)==TRUE,1,0)
wine_test$IMP_ResidualSugar[which(is.na(wine_test$IMP_ResidualSugar))]=mean(wine_test$IMP_ResidualSugar,na.rm=TRUE)
str(wine_test$M_ResidualSugar)
str(wine_test$IMP_ResidualSugar)
summary(wine_test$M_ResidualSugar)
summary(wine_test$IMP_ResidualSugar)
summary(wine_test$ResidualSugar)

wine_test$M_Chlorides=wine_test$Chlorides
wine_test$IMP_Chlorides=wine_test$Chlorides
wine_test$M_Chlorides=ifelse(is.na(wine_test$M_Chlorides)==TRUE,1,0)
wine_test$IMP_Chlorides[which(is.na(wine_test$IMP_Chlorides))]=mean(wine_test$IMP_Chlorides,na.rm=TRUE)
str(wine_test$M_Chlorides)
str(wine_test$IMP_Chlorides)
summary(wine_test$M_Chlorides)
summary(wine_test$IMP_Chlorides)
summary(wine_test$Chlorides)

wine_test$M_FreeSulfurDioxide=wine_test$FreeSulfurDioxide
wine_test$IMP_FreeSulfurDioxide=wine_test$FreeSulfurDioxide
wine_test$M_FreeSulfurDioxide=ifelse(is.na(wine_test$M_FreeSulfurDioxide)==TRUE,1,0)
wine_test$IMP_FreeSulfurDioxide[which(is.na(wine_test$IMP_FreeSulfurDioxide))]=mean(wine_test$IMP_FreeSulfurDioxide,na.rm=TRUE)
str(wine_test$M_FreeSulfurDioxide)
str(wine_test$IMP_FreeSulfurDioxide)
summary(wine_test$M_FreeSulfurDioxide)
summary(wine_test$IMP_FreeSulfurDioxide)
summary(wine_test$FreeSulfurDioxide)

wine_test$M_TotalSulfurDioxide=wine_test$TotalSulfurDioxide
wine_test$IMP_TotalSulfurDioxide=wine_test$TotalSulfurDioxide
wine_test$M_TotalSulfurDioxide=ifelse(is.na(wine_test$M_TotalSulfurDioxide)==TRUE,1,0)
wine_test$IMP_TotalSulfurDioxide[which(is.na(wine_test$IMP_TotalSulfurDioxide))]=mean(wine_test$IMP_TotalSulfurDioxide,na.rm=TRUE)
str(wine_test$M_TotalSulfurDioxide)
str(wine_test$IMP_TotalSulfurDioxide)
summary(wine_test$M_TotalSulfurDioxide)
summary(wine_test$IMP_TotalSulfurDioxide)
summary(wine_test$TotalSulfurDioxide)

wine_test$M_pH=wine_test$pH
wine_test$IMP_pH=wine_test$pH
wine_test$M_pH=ifelse(is.na(wine_test$M_pH)==TRUE,1,0)
wine_test$IMP_pH[which(is.na(wine_test$IMP_pH))]=mean(wine_test$IMP_pH,na.rm=TRUE)
str(wine_test$M_pH)
str(wine_test$IMP_pH)
summary(wine_test$M_pH)
summary(wine_test$IMP_pH)
summary(wine_test$pH)

wine_test$M_Sulphates=wine_test$Sulphates
wine_test$IMP_Sulphates=wine_test$Sulphates
wine_test$M_Sulphates=ifelse(is.na(wine_test$M_Sulphates)==TRUE,1,0)
wine_test$IMP_Sulphates[which(is.na(wine_test$IMP_Sulphates))]=mean(wine_test$IMP_Sulphates,na.rm=TRUE)
str(wine_test$M_Sulphates)
str(wine_test$IMP_Sulphates)
summary(wine_test$M_Sulphates)
summary(wine_test$IMP_Sulphates)
summary(wine_test$Sulphates)

wine_test$M_Alcohol=wine_test$Alcohol
wine_test$IMP_Alcohol=wine_test$Alcohol
wine_test$M_Alcohol=ifelse(is.na(wine_test$M_Alcohol)==TRUE,1,0)
wine_test$IMP_Alcohol[which(is.na(wine_test$IMP_Alcohol))]=mean(wine_test$IMP_Alcohol,na.rm=TRUE)
str(wine_test$M_Alcohol)
str(wine_test$IMP_Alcohol)
summary(wine_test$M_Alcohol)
summary(wine_test$IMP_Alcohol)
summary(wine_test$Alcohol)

wine_test$M_STARS=wine_test$STARS
wine_test$IMP_STARS=wine_test$STARS
wine_test$M_STARS=ifelse(is.na(wine_test$M_STARS)==TRUE,1,0)
wine_test$IMP_STARS[which(is.na(wine_test$IMP_STARS))]=mean(wine_test$IMP_STARS,na.rm=TRUE)
str(wine_test$M_STARS)
str(wine_test$IMP_STARS)
summary(wine_test$M_STARS)
summary(wine_test$IMP_STARS)
summary(wine_test$STARS)

#outlier: AcidIndex - 95% trim
per95lower=quantile(wine_test$AcidIndex,0.05)
per95upper=quantile(wine_test$AcidIndex,0.95)
per95lower
per95upper
skewness(wine_test$AcidIndex)
summary(wine_test$AcidIndex)
wine_test$AcidIndex[wine_test$AcidIndex<per95lower]=per95lower
wine_test$AcidIndex[wine_test$AcidIndex>per95upper]=per95upper
skewness(wine_test$AcidIndex)
summary(wine_test$AcidIndex)

str(wine_test)
summary(wine_test)

#putting all new variables in a dataframe to prepare for model building
wine_test=data.frame(wine_test[1:5],wine_test[10:10],wine_test[14:15],wine_test[17:32])
str(wine_test)
summary(wine_test)
model=zeroinfl(TARGET~VolatileAcidity+LabelAppeal+AcidIndex+IMP_Alcohol+M_STARS+IMP_STARS+IMP_FreeSulfurDioxide+IMP_TotalSulfurDioxide+IMP_pH+IMP_Sulphates,data=wine)
#the code here works b/c use (TARGET~x1+x2+x3, data=wine) to develop model
#but if do it differently using (wine$TARGET~wine$x1+wine$x2+wine$x3, data=wine) will get an error with predict() function later
wine_test$TARGET=predict(model,newdata=wine_test,type='response')
select=dplyr::select
scores=wine_test[c('INDEX','TARGET')]
write.csv(scores,file='U3_Scored_Mimi_Trinh.csv',row.names=FALSE)







