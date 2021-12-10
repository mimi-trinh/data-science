# MSDS 498 Capstone Project R Code by Mimi Trinh
# install R packages
library(matrixStats)
library(ggplot2)
library(rpart)				        
library(rattle)				
library(rpart.plot)				
library(RColorBrewer)	
library(randomForest)
library(xgboost)
library(gbm)
library(glm2)
library(plotROC)
library(pROC)
library(MASS)
library(partykit)
library(e1071)
library(woeBinning)
library(OneR)
set.seed(1234)

# read_credit_card_default_data.R
credit_card_default=readRDS(file.path('/Users/mimitrinh/Desktop','credit_card_default.RData'))

# Show dataframe structure;
str(credit_card_default)
# Note: data.group values are constructed to partition the data set in a single dimension;
# data.group <- 1*my.data$train + 2*my.data$test + 3*my.data$validate;
# Use the train, test, and validate flags to define the train, test, and validate data sets;

# Here is the observation count in each data set;
table(credit_card_default$data.group)

# Show top of data frame with some values;
head(credit_card_default)

# section 2: the data
# table with # of observations in train, test, validate dataset
table1=table(credit_card_default$data.group)
rownames(table1)=c('train','test','validate')
table1=addmargins(table1)
table1

# divide into train, test, validate datasets
train=subset(credit_card_default,credit_card_default$train==1)
test=subset(credit_card_default,credit_card_default$test==1)
validate=subset(credit_card_default,credit_card_default$validate==1)
str(train)
str(test)
str(validate)

# data quality check
summary(train)
# no missing value in any column but data has integrity issues
# predictors with data integrity issues: EDUCATION, MARRIAGE, PAY 0-6, BILL_AMT 1-6
# dependent variable
train$DEFAULT=as.factor(train$DEFAULT)
summary(train$DEFAULT)
table2=table(train$DEFAULT)
rownames(table2)=c('no default','default on payment')
table2=addmargins(table2)
table2
# predictor variables 
train$SEX=as.factor(train$SEX)
summary(train$SEX)
# convert numeric values in nominal variable like SEX to factor values
train$EDUCATION=as.factor(train$EDUCATION)
summary(train$EDUCATION)
# EDUCATION data integrity issue: data dictionary only has 1-4 vs. dataset has 0-6 
train$MARRIAGE=as.factor(train$MARRIAGE)
summary(train$MARRIAGE)
# MARRIAGE data integrity issue: data dictionary only has 1-3 vs. dataset has 0-3
train$PAY_0=as.factor(train$PAY_0)
train$PAY_2=as.factor(train$PAY_2)
train$PAY_3=as.factor(train$PAY_3)
train$PAY_4=as.factor(train$PAY_4)
train$PAY_5=as.factor(train$PAY_5)
train$PAY_6=as.factor(train$PAY_6)
# PAY_0 to PAY_6 data integrity issue: data dictionary only has -1 and positive values vs.
# dataset has -2 and 0 and positive values
# BILL_AMT1 to BILL_AMT6 data integrity issue: what do negative value means?

# EDA to determine how to handle predictors with data integrity issues
# EDUCATION: what do 0, 5, 6 value mean? same as 4 value?
summary(train$EDUCATION)
summary(subset(train$DEFAULT,train$EDUCATION=='0')) # 0% default
summary(subset(train$DEFAULT,train$EDUCATION=='1')) # 20% default
summary(subset(train$DEFAULT,train$EDUCATION=='2')) # 24% default
summary(subset(train$DEFAULT,train$EDUCATION=='3')) # 26% default
summary(subset(train$DEFAULT,train$EDUCATION=='4')) # 8% default
summary(subset(train$DEFAULT,train$EDUCATION=='5')) # 9% default
summary(subset(train$DEFAULT,train$EDUCATION=='6')) # 9% default
# values 5 and 6 behave similar to value 4 with similar default rate
# value 0 (default 0%) behave more similar to value 4 (default 8%) than values 1, 2, 3 (default 20%-26%)
# value 0 also has small sample size with only 7 observations
# so map values 0, 5, 6 to value 4 (others category)
summary(train$EDUCATION)
train$EDUCATION[train$EDUCATION=='0']='4'
train$EDUCATION[train$EDUCATION=='5']='4'
train$EDUCATION[train$EDUCATION=='6']='4'
summary(train$EDUCATION)
train$EDUCATION=droplevels(train$EDUCATION)
summary(train$EDUCATION)

# MARRIAGE data integrity issue: what does 0 value mean? same as 3 value?
summary(train$MARRIAGE)
summary(subset(train$DEFAULT,train$MARRIAGE=='0')) # 3% default
summary(subset(train$DEFAULT,train$MARRIAGE=='1')) # 24% default
summary(subset(train$DEFAULT,train$MARRIAGE=='2')) # 21% default 
summary(subset(train$DEFAULT,train$MARRIAGE=='3')) # 25% default
# value 0 behave different from values 1, 2, 3 while values 1, 2, 3 behave in similar ways
# so will leave 0 the way it is and not map it into 3 (others category)
# but in real life, need to check with industry expert what value 0 mean

# PAY 0-6 data integrity issue: what do -2 and 0 values mean? same as -1 value? 
# need to change name PAY_0 to PAY_1 b/c BILL_AMT and PAY_AMT columns are 1-6 
# whereas PAY columns go from 0 to 2
summary(train)
colnames(train)[7]='PAY_1'
summary(train)
summary(train$PAY_1)
summary(train$PAY_2)
summary(train$PAY_3)
summary(train$PAY_4)
summary(train$PAY_5)
summary(train$PAY_6)
# majority of observations fall in category of -2, -1, and 0 values
# only do EDA for PAY_1 to decide how to handle values -2 and 0 in PAY 1-6 
# b/c breakdown of observations btw -2 to 8 category is same for PAY 1-6
# in this aspect, -2 and 0 values behave similar to -1 value
summary(subset(train$DEFAULT,train$PAY_1=='-2')) # 13% default
summary(subset(train$DEFAULT,train$PAY_1=='0')) # 14% default
summary(subset(train$DEFAULT,train$PAY_1=='-1')) # 17% default
summary(subset(train$DEFAULT,train$PAY_1=='1')) # 34% default
summary(subset(train$DEFAULT,train$PAY_1=='2')) # 70% default
# stop here and not keep going till PAY_1 = 8 
# b/c sample size drops significantly from PAY_1 = 2 to PAY_1 = 3
# values -2 and 0 behave similar to value -1 with similar default rate, which is very different
# from values 1 and 2
# so map values -2 and 0 into value -1, which means pay duly
train$PAY_1[train$PAY_1=='-2']='-1'
train$PAY_1[train$PAY_1=='0']='-1'
train$PAY_1=droplevels(train$PAY_1)
train$PAY_2[train$PAY_2=='-2']='-1'
train$PAY_2[train$PAY_2=='0']='-1'
train$PAY_2=droplevels(train$PAY_2)
train$PAY_3[train$PAY_3=='-2']='-1'
train$PAY_3[train$PAY_3=='0']='-1'
train$PAY_3=droplevels(train$PAY_3)
train$PAY_4[train$PAY_4=='-2']='-1'
train$PAY_4[train$PAY_4=='0']='-1'
train$PAY_4=droplevels(train$PAY_4)
train$PAY_5[train$PAY_5=='-2']='-1'
train$PAY_5[train$PAY_5=='0']='-1'
train$PAY_5=droplevels(train$PAY_5)
train$PAY_6[train$PAY_6=='-2']='-1'
train$PAY_6[train$PAY_6=='0']='-1'
train$PAY_6=droplevels(train$PAY_6)
summary(train$PAY_1)
summary(train$PAY_2)
summary(train$PAY_3)
summary(train$PAY_4)
summary(train$PAY_5)
summary(train$PAY_6)

# BILL_AMT 1-6 data integrity issue: what do negative values mean?
# upon research, if you owe $ to CC Co, have (+) balance
# if you overpay the bill or if CC Co issue a credit back to the account after paying the bill, 
# have (-) balance, so these negative values in BILL_AMT are ok, leave them the way they are
str(train)
summary(train)

# section 3: feature engineering
# AGE binning
summary(train$AGE)
str(train$AGE)
# b/c min = 21 and max = 75, will have 4 bins
# bin 1 young adults: <= 25
# bin 2 adults: 26 - 40
# bin 3 middle age: 41-64
# bin 4 elderly: >= 65
nrow(train[train$AGE<=25,]) # bin 1
nrow(train[train$AGE>25 & train$AGE<=40,]) # bin 2
nrow(train[train$AGE>40 & train$AGE<=64,]) # bin 3
nrow(train[train$AGE>=65,]) # bin 4
train$AGE[train$AGE<=25]=1
train$AGE[train$AGE>25 & train$AGE<=40]=2
train$AGE[train$AGE>40 & train$AGE<=64]=3
train$AGE[train$AGE>=65]=4
train$AGE=as.factor(train$AGE)
summary(train$AGE)
str(train$AGE)

# AVG_BILL_AMT or average bill amount
train$AVG_BILL_AMT=(train$BILL_AMT1+train$BILL_AMT2+train$BILL_AMT3+train$BILL_AMT4+
                      train$BILL_AMT5+train$BILL_AMT6)/6

# AVG_PAY_AMT or average payment amount
train$AVG_PAY_AMT=(train$PAY_AMT1+train$PAY_AMT2+train$PAY_AMT3+train$PAY_AMT4+
                     train$PAY_AMT5+train$PAY_AMT6)/6

# PAY_RATIO 1-6 or payment ratio months 1-6
# there's a time delay here so PAY_AMT 2 is equivalent of BILL_AMT3
train$PAY_RATIO1=train$PAY_AMT1/train$BILL_AMT2
train$PAY_RATIO2=train$PAY_AMT2/train$BILL_AMT3
train$PAY_RATIO3=train$PAY_AMT3/train$BILL_AMT4
train$PAY_RATIO4=train$PAY_AMT4/train$BILL_AMT5
train$PAY_RATIO5=train$PAY_AMT5/train$BILL_AMT6
head(train)
# definte a 0 payment / 0 bill as 100, these are n/a values in five columns above
train$PAY_RATIO1[is.na(train$PAY_RATIO1)]=100
train$PAY_RATIO2[is.na(train$PAY_RATIO2)]=100
train$PAY_RATIO3[is.na(train$PAY_RATIO3)]=100
train$PAY_RATIO4[is.na(train$PAY_RATIO4)]=100
train$PAY_RATIO5[is.na(train$PAY_RATIO5)]=100
head(train)

# AVG_PAY_RATIO or average payment ratio
train$AVG_PAY_RATIO=(train$PAY_RATIO1+train$PAY_RATIO2+train$PAY_RATIO3+train$PAY_RATIO4+
                       train$PAY_RATIO5)/5

# UTIL or utilization: how much of credit line is the customer using
train$UTIL1=train$BILL_AMT1/train$LIMIT_BAL
train$UTIL2=train$BILL_AMT2/train$LIMIT_BAL
train$UTIL3=train$BILL_AMT3/train$LIMIT_BAL
train$UTIL4=train$BILL_AMT4/train$LIMIT_BAL
train$UTIL5=train$BILL_AMT5/train$LIMIT_BAL
train$UTIL6=train$BILL_AMT6/train$LIMIT_BAL

# AVG_UTIL or average utilization
train$AVG_UTIL=(train$UTIL1+train$UTIL2+train$UTIL3+train$UTIL4+
                  train$UTIL5+train$UTIL6)/6

# BILL_GROWTH 1-6 or balance growth months 1-6
train$BILL_GROWTH2=train$BILL_AMT2-train$BILL_AMT1
train$BILL_GROWTH3=train$BILL_AMT3-train$BILL_AMT2
train$BILL_GROWTH4=train$BILL_AMT4-train$BILL_AMT3
train$BILL_GROWTH5=train$BILL_AMT5-train$BILL_AMT4
train$BILL_GROWTH6=train$BILL_AMT6-train$BILL_AMT5

# UTIL_GROWTH 1-6 or utilization growth months 1-6
train$UTIL_GROWTH2=train$UTIL2-train$UTIL1
train$UTIL_GROWTH3=train$UTIL3-train$UTIL2
train$UTIL_GROWTH4=train$UTIL4-train$UTIL3
train$UTIL_GROWTH5=train$UTIL5-train$UTIL4
train$UTIL_GROWTH6=train$UTIL6-train$UTIL5

# MAX_BILL_AMT or max bill amount 
head(train)
matrix1=matrix(c(train$BILL_AMT1,train$BILL_AMT2,train$BILL_AMT3,
                     train$BILL_AMT4,train$BILL_AMT5,train$BILL_AMT6),ncol=6)
train$MAX_BILL_AMT=rowMaxs(matrix1)
head(train$MAX_BILL_AMT)

# MAX_PAY_AMT or max payment amount
head(train)
matrix2=matrix(c(train$PAY_AMT1,train$PAY_AMT2,train$PAY_AMT3,
                 train$PAY_AMT4,train$PAY_AMT5,train$PAY_AMT6),ncol=6)
train$MAX_PAY_AMT=rowMaxs(matrix2)
head(train$MAX_PAY_AMT)

# DLQ 1-5 or delinquency months 1-5, negative value means owe $, positive value means overpay 
train$DLQ1=train$PAY_AMT1-train$BILL_AMT2
train$DLQ2=train$PAY_AMT2-train$BILL_AMT3
train$DLQ3=train$PAY_AMT3-train$BILL_AMT4
train$DLQ4=train$PAY_AMT4-train$BILL_AMT5
train$DLQ5=train$PAY_AMT5-train$BILL_AMT6

# MAX_DLQ or max delinquency amount
head(train)
matrix3=matrix(c(train$DLQ1,train$DLQ2,train$DLQ3,train$DLQ4,train$DLQ5),ncol=5)
train$MAX_DLQ=rowMins(matrix3)
head(train$MAX_DLQ)

# section 4a: traditional EDA
summary(train)
str(train)

# LIMIT_BAL
plot(train$DEFAULT,train$LIMIT_BAL,col='red',main='Default by Limit Balance',
     ylab='Limit Balance',xlab='0: no default          1: default payment')
mean(train$LIMIT_BAL[train$DEFAULT=='0'])
mean(train$LIMIT_BAL[train$DEFAULT=='1'])
quantile(train$LIMIT_BAL[train$DEFAULT=='0'],c(0.25,0.50,0.75))
quantile(train$LIMIT_BAL[train$DEFAULT=='1'],c(0.25,0.50,0.75))

# SEX
ggplot(train,aes(x=DEFAULT,fill=SEX)) + geom_bar() + ggtitle('Default by Sex') +
  xlab('0: no default          1: default payment') +
  ylab('1: male           2: female')
table3=table(train$DEFAULT,train$SEX)
colnames(table3)=c('male','female')
rownames(table3)=c('no default','default payment')
table3=addmargins(table3)
table3

# EDUCATION
ggplot(train,aes(x=DEFAULT,fill=EDUCATION)) + geom_bar() + ggtitle('Default by Education') +
  xlab('0: no default          1: default payment') +
  ylab('1: graduate school      2: university      3: high school      4: others')
table4=table(train$DEFAULT,train$EDUCATION)
colnames(table4)=c('graduate school','university','high school','others')
rownames(table4)=c('no default','default payment')
table4=addmargins(table4)
table4

# MARRIAGE
ggplot(train,aes(x=DEFAULT,fill=MARRIAGE)) + geom_bar() + ggtitle('Default by Marriage') +
  xlab('0: no default          1: default payment') +
  ylab('0: unknown     1: married     2: single     3: others')
table5=table(train$DEFAULT,train$MARRIAGE)
colnames(table5)=c('unknown','married','single','others')
rownames(table5)=c('no default','default payment')
table5=addmargins(table5)
table5

# AGE
ggplot(train,aes(x=DEFAULT,fill=AGE)) + geom_bar() + ggtitle('Default by Age Group') +
  xlab('0: no default     1: default payment') +
  ylab('1: 25 or less     2: between 26-40     3: between 41-64     4: 65 or more')

# AVG_BILL_AMT
plot(train$DEFAULT,train$AVG_BILL_AMT,col='blue',main='Default by Average Balance')
mean(train$AVG_BILL_AMT[train$DEFAULT=='0'])
mean(train$AVG_BILL_AMT[train$DEFAULT=='1'])
quantile(train$AVG_BILL_AMT[train$DEFAULT=='0'],c(0.25,0.50,0.75))
quantile(train$AVG_BILL_AMT[train$DEFAULT=='1'],c(0.25,0.50,0.75))

# AVG_PAY_AMT
par(mfrow=c(1,2))
plot(train$DEFAULT,train$AVG_PAY_AMT,col='red',main='Default by Average Payment Amount',
     ylab=c('Average Payment Amount'),xlab=c('0: no default          1: default payment'))
plot(train$DEFAULT,train$AVG_PAY_AMT,col='red',main='Default by Average Payment Amount with No Outlier',
     outline=FALSE,ylab=c('Average Payment Amount excluding Outlier'),
     xlab=c('0: no default          1: default payment'))
par(mfrow=c(1,1))
mean(train$AVG_PAY_AMT[train$DEFAULT=='0'])
mean(train$AVG_PAY_AMT[train$DEFAULT=='1'])
quantile(train$AVG_PAY_AMT[train$DEFAULT=='0'],c(0.25,0.50,0.75))
quantile(train$AVG_PAY_AMT[train$DEFAULT=='1'],c(0.25,0.50,0.75))

# AVG_UTIL
plot(train$DEFAULT,train$AVG_UTIL,col='red',main='Default by Average Utilization',
     xlab=c('0: no default          1: default payment'),
     ylab=c('Average Utilization'))
mean(train$AVG_UTIL[train$DEFAULT=='0'])
mean(train$AVG_UTIL[train$DEFAULT=='1'])
round(quantile(train$AVG_UTIL[train$DEFAULT=='0'],c(0.25,0.50,0.75)),4)
round(quantile(train$AVG_UTIL[train$DEFAULT=='1'],c(0.25,0.50,0.75)),4)

# section 4b: model based EDA
# put model data together
str(train)
model_train=data.frame(c(train[25:25],train[2:12],train[31:32],train[38:38],train[45:57],train[63:63]))
str(model_train)
tree1=rpart(DEFAULT~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
              AVG_BILL_AMT+AVG_PAY_AMT+AVG_PAY_RATIO+AVG_UTIL+
              MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,
              method='class',data=model_train)
rpart.plot(tree1,main='Decision Tree')
tree2=rpart(DEFAULT~.,method='class',data=model_train)
rpart.plot(tree2,main='Decision Tree')
# decision tree result keys on one variable PAY_1
# so go back and plot this variable to see a separation btw two classes 
ggplot(train,aes(x=DEFAULT,fill=PAY_1)) + geom_bar() + ggtitle('Default by Payment Status Sept') +
  xlab('0: no default          1: default payment') +
  ylab('Payment Status')
table6=table(train$PAY_1,train$DEFAULT)
colnames(table6)=c('no default','default payment')
rownames(table6)=c('pay in full','1 month late','2 month late','3 month late','4 month late',
                   '5 month late','6 month late','7 month late','8 month late')
table6=addmargins(table6)
table6
# among those with no default, 86% paid in full in Sept
# among those that default on payment, only 49% paid in full in Sept 
# however, this is NOT very useful
# so can't rely on decision tree to build model in this case
# need to build other predictive models: random forest, gradient boosting, logistic, neural networks
tree3=OneR(DEFAULT~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
             AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
             MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=model_train,verbose=TRUE)
summary(tree3)
plot(tree3)
eval_model(prediction=predict(tree3,newdata=model_train),actual=model_train$DEFAULT)
# using OneR, classification matrix produces poor results b/c predict all to be no default 

# section 5: predictive modeling: methods and results 
# clean test dataset, similar to train dataset 
test$DEFAULT=as.factor(test$DEFAULT)
test$SEX=as.factor(test$SEX)
test$EDUCATION=as.factor(test$EDUCATION)
test$MARRIAGE=as.factor(test$MARRIAGE)
test$PAY_0=as.factor(test$PAY_0)
test$PAY_2=as.factor(test$PAY_2)
test$PAY_3=as.factor(test$PAY_3)
test$PAY_4=as.factor(test$PAY_4)
test$PAY_5=as.factor(test$PAY_5)
test$PAY_6=as.factor(test$PAY_6)
test$EDUCATION[test$EDUCATION=='0']='4'
test$EDUCATION[test$EDUCATION=='5']='4'
test$EDUCATION[test$EDUCATION=='6']='4'
test$EDUCATION=droplevels(test$EDUCATION)
test$PAY_1[test$PAY_1=='-2']='-1'
test$PAY_1[test$PAY_1=='0']='-1'
test$PAY_1=droplevels(test$PAY_1)
test$PAY_2[test$PAY_2=='-2']='-1'
test$PAY_2[test$PAY_2=='0']='-1'
test$PAY_2=droplevels(test$PAY_2)
test$PAY_3[test$PAY_3=='-2']='-1'
test$PAY_3[test$PAY_3=='0']='-1'
test$PAY_3=droplevels(test$PAY_3)
test$PAY_4[test$PAY_4=='-2']='-1'
test$PAY_4[test$PAY_4=='0']='-1'
test$PAY_4=droplevels(test$PAY_4)
test$PAY_5[test$PAY_5=='-2']='-1'
test$PAY_5[test$PAY_5=='0']='-1'
test$PAY_5=droplevels(test$PAY_5)
test$PAY_6[test$PAY_6=='-2']='-1'
test$PAY_6[test$PAY_6=='0']='-1'
test$PAY_6=droplevels(test$PAY_6)
nrow(test[test$AGE<=25,]) # bin 1
nrow(test[test$AGE>25 & test$AGE<=40,]) # bin 2
nrow(test[test$AGE>40 & test$AGE<=64,]) # bin 3
nrow(test[test$AGE>=65,]) # bin 4
test$AGE[test$AGE<=25]=1
test$AGE[test$AGE>25 & test$AGE<=40]=2
test$AGE[test$AGE>40 & test$AGE<=64]=3
test$AGE[test$AGE>=65]=4
test$AGE=as.factor(test$AGE)
test$AVG_BILL_AMT=(test$BILL_AMT1+test$BILL_AMT2+test$BILL_AMT3+test$BILL_AMT4+
                     test$BILL_AMT5+test$BILL_AMT6)/6
test$AVG_PAY_AMT=(test$PAY_AMT1+test$PAY_AMT2+test$PAY_AMT3+test$PAY_AMT4+
                    test$PAY_AMT5+test$PAY_AMT6)/6
test$PAY_RATIO1=test$PAY_AMT1/test$BILL_AMT2
test$PAY_RATIO2=test$PAY_AMT2/test$BILL_AMT3
test$PAY_RATIO3=test$PAY_AMT3/test$BILL_AMT4
test$PAY_RATIO4=test$PAY_AMT4/test$BILL_AMT5
test$PAY_RATIO5=test$PAY_AMT5/test$BILL_AMT6
test$PAY_RATIO1[is.na(test$PAY_RATIO1)]=100
test$PAY_RATIO2[is.na(test$PAY_RATIO2)]=100
test$PAY_RATIO3[is.na(test$PAY_RATIO3)]=100
test$PAY_RATIO4[is.na(test$PAY_RATIO4)]=100
test$PAY_RATIO5[is.na(test$PAY_RATIO5)]=100
test$AVG_PAY_RATIO=(test$PAY_RATIO1+test$PAY_RATIO2+test$PAY_RATIO3+test$PAY_RATIO4+
                       test$PAY_RATIO5)/5
test$UTIL1=test$BILL_AMT1/test$LIMIT_BAL
test$UTIL2=test$BILL_AMT2/test$LIMIT_BAL
test$UTIL3=test$BILL_AMT3/test$LIMIT_BAL
test$UTIL4=test$BILL_AMT4/test$LIMIT_BAL
test$UTIL5=test$BILL_AMT5/test$LIMIT_BAL
test$UTIL6=test$BILL_AMT6/test$LIMIT_BAL
test$AVG_UTIL=(test$UTIL1+test$UTIL2+test$UTIL3+test$UTIL4+
                 test$UTIL5+test$UTIL6)/6
test$BILL_GROWTH2=test$BILL_AMT2-test$BILL_AMT1
test$BILL_GROWTH3=test$BILL_AMT3-test$BILL_AMT2
test$BILL_GROWTH4=test$BILL_AMT4-test$BILL_AMT3
test$BILL_GROWTH5=test$BILL_AMT5-test$BILL_AMT4
test$BILL_GROWTH6=test$BILL_AMT6-test$BILL_AMT5
test$UTIL_GROWTH2=test$UTIL2-test$UTIL1
test$UTIL_GROWTH3=test$UTIL3-test$UTIL2
test$UTIL_GROWTH4=test$UTIL4-test$UTIL3
test$UTIL_GROWTH5=test$UTIL5-test$UTIL4
test$UTIL_GROWTH6=test$UTIL6-test$UTIL5
matrix1=matrix(c(test$BILL_AMT1,test$BILL_AMT2,test$BILL_AMT3,
                 test$BILL_AMT4,test$BILL_AMT5,test$BILL_AMT6),ncol=6)
test$MAX_BILL_AMT=rowMaxs(matrix1)
matrix2=matrix(c(test$PAY_AMT1,test$PAY_AMT2,test$PAY_AMT3,
                 test$PAY_AMT4,test$PAY_AMT5,test$PAY_AMT6),ncol=6)
test$MAX_PAY_AMT=rowMaxs(matrix2)
test$DLQ1=test$PAY_AMT1-test$BILL_AMT2
test$DLQ2=test$PAY_AMT2-test$BILL_AMT3
test$DLQ3=test$PAY_AMT3-test$BILL_AMT4
test$DLQ4=test$PAY_AMT4-test$BILL_AMT5
test$DLQ5=test$PAY_AMT5-test$BILL_AMT6
matrix3=matrix(c(test$DLQ1,test$DLQ2,test$DLQ3,test$DLQ4,test$DLQ5),ncol=5)
test$MAX_DLQ=rowMins(matrix3)
model_test=data.frame(c(test[25:25],test[2:12],test[31:32],test[38:38],test[45:57],test[63:63]))

# true positive rate (TPR) = TP / actual yes
# false positive rate (FPR) = FP / actual no
# accuracy = (TP + TN) / total

# section 5a model 1: random forest
fit1=randomForest(DEFAULT~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
                    AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
                    MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=model_train,importance=TRUE,ntree=300)
varImpPlot(fit1,main='Variable Importance Plot - Random Forest Model',col='blue') 
# 2 types of measurement in Variable Importance Plot
# Accuracy: tests to see how worse the model performs without each variable, 
# so a high decrease in accuracy would be expected for very predictive variables
# Gini: digs into the mathematics behind decision trees, 
# but essentially measures how pure the nodes are at the end of the tree
# it tests to see the result if each variable is taken out and 
# a high score means the variable was important
# Both measurements show AVG_PAY_AMT as the most significant predictors 
predict1train=predict(fit1,model_train)
summary(predict1train)
table7=table(predict1train,model_train$DEFAULT)
colnames(table7)=c('No Default Actual','Default Actual')
rownames(table7)=c('No Default Predicted','Default Predicted')
table7=addmargins(table7)
table7
# classification matrix for train dataset
# TPR = 3328 / 3423 = 97.22%
# FPR = 11 / 11757 = 0.0936%
# Accuracy = (11746 + 3328) / 15180 = 99.30%
predict1test=predict(fit1,model_test)
summary(predict1test)
table8=table(predict1test,model_test$DEFAULT)
colnames(table8)=c('No Default Actual','Default Actual')
rownames(table8)=c('No Default Predicted','Default Predicted')
table8=addmargins(table8)
table8
# classification matrix for test dataset
# TPR = 216 / 1557 = 13.87%
# FPR = 225 / 5766 = 3.9022%
# Accuracy = (5541 + 216) / 7323 = 78.62%
# train set measurements are much better than test sets, so model 1 overfit

# section 5b model 2: gradient boosting 
gbm_train=model_train
gbm_train$DEFAULT=as.character(gbm_train$DEFAULT)
fit2=gbm(DEFAULT~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
           AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
           MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=gbm_train,distribution = "bernoulli",n.trees=500)
predict2train=predict(fit2,gbm_train,n.trees=500,type='response')
summary(predict2train)
str(predict2train)
cutoff=InformationValue::optimalCutoff(gbm_train$DEFAULT,predict2train,optimiseFor='misclasserror',returnDiagnostics =T)
cutoff=cutoff$optimalCutoff
cutoff
predict2train[predict2train > cutoff] = 1
predict2train[predict2train <= cutoff] = 0
predict2train=as.factor(predict2train)
summary(predict2train)
table13=table(predict2train,model_train$DEFAULT)
colnames(table13)=c('No Default Actual','Default Actual')
rownames(table13)=c('No Default Predicted','Default Predicted')
table13=addmargins(table13)
table13
# classification matrix for train dataset
# TPR = 253 / 3423 = 7.39%
# FPR = 180 / 11757 = 1.53%
# Accuracy = (11577 + 253) / 15180 = 77.93%
gbm_test=model_test
gbm_test$DEFAULT=as.character(gbm_test$DEFAULT)
predict2test=predict(fit2,gbm_test,n.trees=500,type='response')
summary(predict2test)
str(predict2test)
predict2test[predict2test > cutoff] = 1
predict2test[predict2test <= cutoff] = 0
predict2test = as.factor(predict2test)
summary(predict2test)
table14=table(predict2test,model_test$DEFAULT)
colnames(table14)=c('No Default Actual','Default Actual')
rownames(table14)=c('No Default Predicted','Default Predicted')
table14=addmargins(table14)
table14
# classification matrix for test dataset
# TPR = 115 / 1557 = 7.39%
# FPR = 94 / 5766 = 1.63%
# Accuracy = (5672 + 115) / 7323 = 79.02%

# section 5c model 3: logistic regression with variable selection
# using section 5a model 1: random forest, narrow down to 7 significant predictors
# exclude the following variables in logistic model b/c they're not significant in random forest model
# MARRIAGE, AGE, SEX, EDUCATION
fit3=glm(DEFAULT~LIMIT_BAL+
           AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
           MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=model_train,family=binomial())
summary(fit3)
# use stepwise variable selection method 
fit3=stepAIC(fit3,direction=c('both')) 
summary(fit3)
# all predictors are significant with p-values < 0.05
predict3train=predict(fit3,model_train)
summary(predict3train)
predict3test=predict(fit3,model_test)
summary(predict3test)
score_train=fit3$fitted.values
roc_train=roc(response=model_train$DEFAULT,predictor=score_train)
print(roc_train)
auc_train=auc(roc_train)
auc_train
specs_train=coords(roc=roc_train,x=c('best'),input=c('threshold','specificity','sensitivity'),
                   ret=c('threshold','specificity','sensitivity'),as.list=TRUE)
model_train$SCORE=fit3$fitted.values
model_train$CLASS=ifelse(model_train$SCORE>specs_train$threshold,1,0)
table11=table(model_train$CLASS,model_train$DEFAULT)
colnames(table11)=c('No Default Actual','Default Actual')
rownames(table11)=c('No Default Predicted','Default Predicted')
table11=addmargins(table11)
table11
# classification for train dataset
# TPR = 2091 / 3423 = 61.09%
# FPR = 4314 / 11757 = 36.69%
# Accuracy = (7443 + 2091) / 15180 = 62.81%
fit3test=glm(DEFAULT~LIMIT_BAL+
           AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
           MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=model_test,family=binomial())
score_test=fit3test$fitted.values
roc_test=roc(response=model_test$DEFAULT,predictor=score_test)
print(roc_test)
auc_test=auc(roc_test)
auc_test
specs_test=coords(roc=roc_test,x=c('best'),input=c('threshold','specificity','sensitivity'),
                  ret=c('threshold','specificity','sensitivity'),as.list=TRUE)
model_test$SCORE=fit3test$fitted.values
model_test$CLASS=ifelse(model_test$SCORE>specs_test$threshold,1,0)
table12=table(model_test$CLASS,model_test$DEFAULT)
colnames(table12)=c('No Default Actual','Default Actual')
rownames(table12)=c('No Default Predicted','Default Predicted')
table12=addmargins(table12)
table12
# classification for test dataset
# TPR = 1051 / 1557 = 67.50%
# FPR = 2296 / 5766 = 39.82%
# Accuracy = (3470 + 1051) / 7323 = 61.74%
# performance of train set < test set but still very close, so no overfit issue 
par(mfrow=c(1,2))
plot(roc_train,main='ROC Curve of Train Set',col='blue',print.auc=TRUE)
plot(roc_test,main='ROC Curve of Test Set',col='red',print.auc=TRUE)
par(mfrow=c(1,1))
# AUC for train set = 0.667 vs. test set = 0.682, not too different
# confirm no overfit issue b/c performance train model similar to test model 

# section 5d model 4: SVM
fit4=svm(DEFAULT~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
           AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
           MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=model_train,
           type = 'C-classification',kernel = 'linear')
predict4train=predict(fit4,model_train)
summary(predict4train)
table9=table(predict4train,model_train$DEFAULT)
colnames(table9)=c('No Default Actual','Default Actual')
rownames(table9)=c('No Default Predicted','Default Predicted')
table9=addmargins(table9)
table9
# classification matrix for train dataset
# TPR = 0 / 3423 = 0%
# FPR = 0 / 11757 = 0&
# Accuracy = (11757 + 0) / 15180 = 77.45%
predict4test=predict(fit4,model_test)
summary(predict4test)
table10=table(predict4test,model_test$DEFAULT)
colnames(table10)=c('No Default Actual','Default Actual')
rownames(table10)=c('No Default Predicted','Default Predicted')
table10=addmargins(table10)
table10
# classification matrix for test dataset
# TPR = 0 / 1557 = 0%
# FPR = 0 / 5766 = 0%
# Accuracy = (5766 + 0) / 7323 = 78.74%
svm_train1=data.frame(model_train$DEFAULT,model_train$AVG_PAY_AMT,model_train$AVG_UTIL)
colnames(svm_train1)=c('DEFAULT','AVG_PAY_AMT','AVG_UTIL')
str(svm_train1)
fit_svm1=svm(DEFAULT~.,svm_train1,kernel='linear')
plot(fit_svm1,svm_train1)
svm_train2=data.frame(model_train$DEFAULT,model_train$MAX_PAY_AMT,model_train$MAX_BILL_AMT)
colnames(svm_train2)=c('DEFAULT','MAX_PAY_AMT','MAX_BILL_AMT')
fit_svm2=svm(DEFAULT~.,svm_train2,kernel='linear')
plot(fit_svm2,svm_train2)
svm_train3=data.frame(model_train$DEFAULT,model_train$AVG_BILL_AMT,model_train$MAX_DLQ)
colnames(svm_train3)=c('DEFAULT','AVG_BILL_AMT','MAX_DLQ')
fit_svm3=svm(DEFAULT~.,svm_train3,kernel='linear')
plot(fit_svm3,svm_train3)


# Performance Monitoring Guide
table(model_train$CLASS)
table(model_test$CLASS)
df_train=as.data.frame(cbind(model_train$SCORE,model_train$DEFAULT))
head(df_train)
df_test=as.data.frame(cbind(model_test$SCORE,model_test$DEFAULT))
head(df_test)
decile_train=quantile(df_train$V1,probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,
                                          0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95))
decile_test=quantile(df_test$V1,probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,
                                          0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95))
df_train$model.decile=cut(df_train$V1,breaks=c(0,decile_train,1),
                          labels=rev(c('0.5','1','1.5','2','2.5','3','3.5',
                                       '4','4.5','5','5.5','6','6.5',
                                       '7','7.5','8','8.5','9','9.5','10')))
df_test$model.decile=cut(df_test$V1,breaks=c(0,decile_test,1),
                          labels=rev(c('0.5','1','1.5','2','2.5','3','3.5',
                                       '4','4.5','5','5.5','6','6.5',
                                       '7','7.5','8','8.5','9','9.5','10')))
head(df_train)
head(df_test)
aggregate(df_train$V1,by=list(Decile=df_train$model.decile),FUN=min)
aggregate(df_test$V1,by=list(Decile=df_test$model.decile),FUN=min)
table(df_train$model.decile)
table(df_test$model.decile)
table(df_train$model.decile,df_train$V2)
table(df_test$model.decile,df_test$V2)
ks_train=as.data.frame(list(Y0=table(df_train$model.decile,df_train$V2)[,1],
                            Y1=table(df_train$model.decile,df_train$V2)[,2],
                            Decile=rev(c('0.5','1','1.5','2','2.5','3','3.5',
                                         '4','4.5','5','5.5','6','6.5',
                                         '7','7.5','8','8.5','9','9.5','10'))))
ks_test=as.data.frame(list(Y0=table(df_test$model.decile,df_test$V2)[,1],
                            Y1=table(df_test$model.decile,df_test$V2)[,2],
                            Decile=rev(c('0.5','1','1.5','2','2.5','3','3.5',
                                         '4','4.5','5','5.5','6','6.5',
                                         '7','7.5','8','8.5','9','9.5','10'))))
ks_train[order(ks_train$Decile),]
ks_test[order(ks_test$Decile),]


# Performance Validation Guide
validate$DEFAULT=as.factor(validate$DEFAULT)
validate$SEX=as.factor(validate$SEX)
validate$EDUCATION=as.factor(validate$EDUCATION)
validate$MARRIAGE=as.factor(validate$MARRIAGE)
validate$PAY_0=as.factor(validate$PAY_0)
validate$PAY_2=as.factor(validate$PAY_2)
validate$PAY_3=as.factor(validate$PAY_3)
validate$PAY_4=as.factor(validate$PAY_4)
validate$PAY_5=as.factor(validate$PAY_5)
validate$PAY_6=as.factor(validate$PAY_6)
validate$EDUCATION[validate$EDUCATION=='0']='4'
validate$EDUCATION[validate$EDUCATION=='5']='4'
validate$EDUCATION[validate$EDUCATION=='6']='4'
validate$EDUCATION=droplevels(validate$EDUCATION)
validate$PAY_1[validate$PAY_1=='-2']='-1'
validate$PAY_1[validate$PAY_1=='0']='-1'
validate$PAY_1=droplevels(validate$PAY_1)
validate$PAY_2[validate$PAY_2=='-2']='-1'
validate$PAY_2[validate$PAY_2=='0']='-1'
validate$PAY_2=droplevels(validate$PAY_2)
validate$PAY_3[validate$PAY_3=='-2']='-1'
validate$PAY_3[validate$PAY_3=='0']='-1'
validate$PAY_3=droplevels(validate$PAY_3)
validate$PAY_4[validate$PAY_4=='-2']='-1'
validate$PAY_4[validate$PAY_4=='0']='-1'
validate$PAY_4=droplevels(validate$PAY_4)
validate$PAY_5[validate$PAY_5=='-2']='-1'
validate$PAY_5[validate$PAY_5=='0']='-1'
validate$PAY_5=droplevels(validate$PAY_5)
validate$PAY_6[validate$PAY_6=='-2']='-1'
validate$PAY_6[validate$PAY_6=='0']='-1'
validate$PAY_6=droplevels(validate$PAY_6)
nrow(validate[validate$AGE<=25,]) # bin 1
nrow(validate[validate$AGE>25 & validate$AGE<=40,]) # bin 2
nrow(validate[validate$AGE>40 & validate$AGE<=64,]) # bin 3
nrow(validate[validate$AGE>=65,]) # bin 4
validate$AGE[validate$AGE<=25]=1
validate$AGE[validate$AGE>25 & validate$AGE<=40]=2
validate$AGE[validate$AGE>40 & validate$AGE<=64]=3
validate$AGE[validate$AGE>=65]=4
validate$AGE=as.factor(validate$AGE)
validate$AVG_BILL_AMT=(validate$BILL_AMT1+validate$BILL_AMT2+validate$BILL_AMT3+validate$BILL_AMT4+
                         validate$BILL_AMT5+validate$BILL_AMT6)/6
validate$AVG_PAY_AMT=(validate$PAY_AMT1+validate$PAY_AMT2+validate$PAY_AMT3+validate$PAY_AMT4+
                        validate$PAY_AMT5+validate$PAY_AMT6)/6
validate$PAY_RATIO1=validate$PAY_AMT1/validate$BILL_AMT2
validate$PAY_RATIO2=validate$PAY_AMT2/validate$BILL_AMT3
validate$PAY_RATIO3=validate$PAY_AMT3/validate$BILL_AMT4
validate$PAY_RATIO4=validate$PAY_AMT4/validate$BILL_AMT5
validate$PAY_RATIO5=validate$PAY_AMT5/validate$BILL_AMT6
validate$PAY_RATIO1[is.na(validate$PAY_RATIO1)]=100
validate$PAY_RATIO2[is.na(validate$PAY_RATIO2)]=100
validate$PAY_RATIO3[is.na(validate$PAY_RATIO3)]=100
validate$PAY_RATIO4[is.na(validate$PAY_RATIO4)]=100
validate$PAY_RATIO5[is.na(validate$PAY_RATIO5)]=100
validate$AVG_PAY_RATIO=(validate$PAY_RATIO1+validate$PAY_RATIO2+validate$PAY_RATIO3+validate$PAY_RATIO4+
                          validate$PAY_RATIO5)/5
validate$UTIL1=validate$BILL_AMT1/validate$LIMIT_BAL
validate$UTIL2=validate$BILL_AMT2/validate$LIMIT_BAL
validate$UTIL3=validate$BILL_AMT3/validate$LIMIT_BAL
validate$UTIL4=validate$BILL_AMT4/validate$LIMIT_BAL
validate$UTIL5=validate$BILL_AMT5/validate$LIMIT_BAL
validate$UTIL6=validate$BILL_AMT6/validate$LIMIT_BAL
validate$AVG_UTIL=(validate$UTIL1+validate$UTIL2+validate$UTIL3+validate$UTIL4+
                     validate$UTIL5+validate$UTIL6)/6
validate$BILL_GROWTH2=validate$BILL_AMT2-validate$BILL_AMT1
validate$BILL_GROWTH3=validate$BILL_AMT3-validate$BILL_AMT2
validate$BILL_GROWTH4=validate$BILL_AMT4-validate$BILL_AMT3
validate$BILL_GROWTH5=validate$BILL_AMT5-validate$BILL_AMT4
validate$BILL_GROWTH6=validate$BILL_AMT6-validate$BILL_AMT5
validate$UTIL_GROWTH2=validate$UTIL2-validate$UTIL1
validate$UTIL_GROWTH3=validate$UTIL3-validate$UTIL2
validate$UTIL_GROWTH4=validate$UTIL4-validate$UTIL3
validate$UTIL_GROWTH5=validate$UTIL5-validate$UTIL4
validate$UTIL_GROWTH6=validate$UTIL6-validate$UTIL5
matrix1=matrix(c(validate$BILL_AMT1,validate$BILL_AMT2,validate$BILL_AMT3,
                 validate$BILL_AMT4,validate$BILL_AMT5,validate$BILL_AMT6),ncol=6)
validate$MAX_BILL_AMT=rowMaxs(matrix1)
matrix2=matrix(c(validate$PAY_AMT1,validate$PAY_AMT2,validate$PAY_AMT3,
                 validate$PAY_AMT4,validate$PAY_AMT5,validate$PAY_AMT6),ncol=6)
validate$MAX_PAY_AMT=rowMaxs(matrix2)
validate$DLQ1=validate$PAY_AMT1-validate$BILL_AMT2
validate$DLQ2=validate$PAY_AMT2-validate$BILL_AMT3
validate$DLQ3=validate$PAY_AMT3-validate$BILL_AMT4
validate$DLQ4=validate$PAY_AMT4-validate$BILL_AMT5
validate$DLQ5=validate$PAY_AMT5-validate$BILL_AMT6
matrix3=matrix(c(validate$DLQ1,validate$DLQ2,validate$DLQ3,validate$DLQ4,validate$DLQ5),ncol=5)
validate$MAX_DLQ=rowMins(matrix3)
model_validate=data.frame(c(validate[25:25],validate[2:12],validate[31:32],validate[38:38],validate[45:57],validate[63:63]))

fit3validate=glm(DEFAULT~LIMIT_BAL+
                   AVG_BILL_AMT+AVG_PAY_AMT+AVG_UTIL+
                   MAX_BILL_AMT+MAX_PAY_AMT+MAX_DLQ,data=model_validate,family=binomial())
score_validate=fit3validate$fitted.values
roc_validate=roc(response=model_validate$DEFAULT,predictor=score_validate)
print(roc_validate)
auc_validate=auc(roc_validate)
auc_validate
specs_validate=coords(roc=roc_validate,x=c('best'),input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),as.list=TRUE)
model_validate$SCORE=fit3validate$fitted.values
model_validate$CLASS=ifelse(model_validate$SCORE>specs_validate$threshold,1,0)

table(model_validate$CLASS)
df_validate=as.data.frame(cbind(model_validate$SCORE,model_validate$DEFAULT))
head(df_validate)
decile_validate=quantile(df_validate$V1,probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,
                                                0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95))
df_validate$model.decile=cut(df_validate$V1,breaks=c(0,decile_validate,1),
                             labels=rev(c('0.5','1','1.5','2','2.5','3','3.5',
                                          '4','4.5','5','5.5','6','6.5',
                                          '7','7.5','8','8.5','9','9.5','10')))
head(df_validate)
aggregate(df_validate$V1,by=list(Decile=df_validate$model.decile),FUN=min)
table(df_validate$model.decile)
table(df_validate$model.decile,df_validate$V2)
ks_validate=as.data.frame(list(Y0=table(df_validate$model.decile,df_validate$V2)[,1],
                               Y1=table(df_validate$model.decile,df_validate$V2)[,2],
                               Decile=rev(c('0.5','1','1.5','2','2.5','3','3.5',
                                            '4','4.5','5','5.5','6','6.5',
                                            '7','7.5','8','8.5','9','9.5','10'))))
ks_validate[order(ks_validate$Decile),]





