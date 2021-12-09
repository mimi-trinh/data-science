#Mimi Trinh
#unit 3 wine sales project - bingo bonus
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
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

str(wine)
summary(wine)
#missing value: ResidualSugar, Chlorides, FreeSulfurDioxide
#TotalSulfurDioxide, pH, Sulphates, Alcohol, STARS 

complete=data.frame(c(wine[3:5],wine[10:10],wine[14:15]))
predictor=data.frame(c(wine[3:16]))
str(complete)
str(predictor)

#TARGET
tree1=data.frame(c(wine[2:2],predictor))
str(tree1)
fit1=rpart(tree1$TARGET~.,method='anova',data=tree1)
par(mfrow=c(1,1))
rpart.plot(fit1,main='Target Decision Tree')

#Residual Sugar
tree2=data.frame(c(wine[6:6],complete))
str(tree2)
fit2=rpart(tree2$ResidualSugar~.,method='anova',data=tree2)
rpart.plot(fit2,main='Residual Sugar Decision Tree')
#this doesn't work, probably due to lack of enough variables, so redo it
fit2=rpart(predictor$ResidualSugar~.,method='anova',data=predictor)
rpart.plot(fit2,main='Residual Sugar Decision Tree')
#can't use decision tree to impute missing values b/c of lack of variables  




