#Mimi Trinh
library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(xlsxjars)
library(xlsx)
library(corrplot)
library(moments)
library(stats4)
setwd('/Users/mimitrinh/Desktop')
moneyball=read.csv('moneyball.csv',header=T)

#part 1: data exploration

head(moneyball)
str(moneyball)
skewness(moneyball)
skewness(moneyball$TEAM_BATTING_SO,na.exclude(moneyball$TEAM_BATTING_SO))
skewness(moneyball$TEAM_BASERUN_SB,na.exclude(moneyball$TEAM_BASERUN_SB))
skewness(moneyball$TEAM_BASERUN_CS,na.exclude(moneyball$TEAM_BASERUN_CS))
skewness(moneyball$TEAM_BATTING_HBP,na.exclude(moneyball$TEAM_BATTING_HBP))
skewness(moneyball$TEAM_PITCHING_SO,na.exclude(moneyball$TEAM_PITCHING_SO))
skewness(moneyball$TEAM_FIELDING_DP,na.exclude(moneyball$TEAM_FIELDING_DP))
summary(moneyball)
moneyball$TARGET_WINS[moneyball$TARGET_WINS<30]
moneyball$TARGET_WINS[moneyball$TARGET_WINS>120]
#there are teams with less than 30 and more than 120 wins
#but it's possible to do so, no need to remove these out of dataset
#no team has negative win or over 162 win, so no error in y-var

par(mfrow=c(1,2))
hist(moneyball$TARGET_WINS,col='blue',main='Target Wins Histogram',xlab='Target Wins')
boxplot(moneyball$TARGET_WINS,col='red',main='Target Wins Boxplot')

par(mfrow=c(1,3))
hist(moneyball$TEAM_BATTING_H,col='blue',main='Team Batting H Histogram',xlab='Team Batting H')
hist(moneyball$TEAM_BATTING_2B,col='red',main='Team Batting 2B Histogram',xlab='Team Batting 2B')
hist(moneyball$TEAM_BATTING_3B,col='green',main='Team Batting 3B Histogram',xlab='Team Batting 3B')

par(mfrow=c(1,3))
boxplot(moneyball$TEAM_BATTING_H,col='blue',main='Team Batting H Boxplot')
boxplot(moneyball$TEAM_BATTING_2B,col='red',main='Team Batting 2B Boxplot')
boxplot(moneyball$TEAM_BATTING_3B,col='green',main='Team Batting 3B Boxplot')

par(mfrow=c(1,3))
hist(moneyball$TEAM_BATTING_HR,col='blue',main='Team Batting HR Histogram',xlab='Team Batting HR')
hist(moneyball$TEAM_BATTING_BB,col='red',main='Team Batting BB Histogram',xlab='Team Batting BB')
hist(moneyball$TEAM_BATTING_HBP,col='green',main='Team Batting HBP Histogram',xlab='Team Batting HBP')

par(mfrow=c(1,3))
boxplot(moneyball$TEAM_BATTING_HR,col='blue',main='Team Batting HR Boxplot')
boxplot(moneyball$TEAM_BATTING_BB,col='red',main='Team Batting BB Boxplot')
boxplot(moneyball$TEAM_BATTING_HBP,col='green',main='Team Batting HBP Boxplot')

par(mfrow=c(1,3))
hist(moneyball$TEAM_BASERUN_SB,col='blue',main='Team Baserun SB Histogram',xlab='Team Baserun SB')
hist(moneyball$TEAM_FIELDING_DP,col='red',main='Team Fielding DP Histogram',xlab='Team Fielding DP')
hist(moneyball$TEAM_PITCHING_SO,col='green',main='Team Pitching SO Histogram',xlab='Team Pitching SO')

par(mfrow=c(1,3))
boxplot(moneyball$TEAM_BASERUN_SB,col='blue',main='Team Baserun SB Boxplot')
boxplot(moneyball$TEAM_FIELDING_DP,col='red',main='Team Fielding DP Boxplot')
boxplot(moneyball$TEAM_PITCHING_SO,col='green',main='Team Pitching SO Boxplot')

par(mfrow=c(1,3))
hist(moneyball$TEAM_BATTING_SO,col='blue',main='Team Batting SO Histogram',xlab='Team Batting SO')
hist(moneyball$TEAM_BASERUN_CS,col='red',main='Team Baserun CS Histogram',xlab='Team Baserun CS')
hist(moneyball$TEAM_FIELDING_E,col='green',main='Team Fielding E Histogram',xlab='Team Fielding E')

par(mfrow=c(1,3))
boxplot(moneyball$TEAM_BATTING_SO,col='blue',main='Team Batting SO Boxplot')
boxplot(moneyball$TEAM_BASERUN_CS,col='red',main='Team Baserun CS Boxplot')
boxplot(moneyball$TEAM_FIELDING_E,col='green',main='Team Fielding E Boxplot')

par(mfrow=c(1,3))
hist(moneyball$TEAM_PITCHING_BB,col='blue',main='Team Pitching BB Histogram',xlab='Team Pitching BB')
hist(moneyball$TEAM_PITCHING_H,col='red',main='Team Pitching H Histogram',xlab='Team Pitching H')
hist(moneyball$TEAM_PITCHING_HR,col='green',main='Team Pitching HR Histogram',xlab='Team Pitching HR')

par(mfrow=c(1,3))
boxplot(moneyball$TEAM_PITCHING_BB,col='blue',main='Team Pitching BB Boxplot')
boxplot(moneyball$TEAM_PITCHING_H,col='red',main='Team Pitching H Boxplot')
boxplot(moneyball$TEAM_PITCHING_HR,col='green',main='Team Pitching HR Boxplot')

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex.cor * r)}
str(moneyball)
pairs(moneyball[2:8],lower.panel=panel.smooth, upper.panel=panel.cor)
pairs(~moneyball$TARGET_WINS+moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS+moneyball$TEAM_BATTING_HBP,lower.panel=panel.smooth,upper.panel=panel.cor)
pairs(~moneyball$TARGET_WINS+moneyball$TEAM_PITCHING_H+moneyball$TEAM_PITCHING_HR+moneyball$TEAM_PITCHING_BB+moneyball$TEAM_PITCHING_SO+moneyball$TEAM_FIELDING_E+moneyball$TEAM_FIELDING_DP,lower.panel=panel.smooth,upper.panel=panel.cor)

#part 2: data preparation

summary(moneyball)
sum(is.na(moneyball))
missing=c(sum(is.na(moneyball$TEAM_BATTING_SO)),sum(is.na(moneyball$TEAM_BASERUN_SB)),sum(is.na(moneyball$TEAM_BASERUN_CS)),sum(is.na(moneyball$TEAM_BATTING_HBP)),sum(is.na(moneyball$TEAM_PITCHING_SO)),sum(is.na(moneyball$TEAM_FIELDING_DP)))
missing
missingvar=c('TEAM_BATTING_SO','TEAM_BASERUN_SB','TEAM_BASERUN_CS','TEAM_BATTING_HDP','TEAM_PITCHING_SO','TEAM_FIELD_DP')
missingvar
missingtable=cbind(missingvar,missing)
missingtable

#5 ways to deal with missing value: slide 12 missing value PPT week 2

#missing value TEAM_BATTING_HDP
par(mfrow=c(1,1))
plot(moneyball$TEAM_BATTING_HBP,moneyball$TARGET_WINS,col='blue',main='Team Batting HBP vs. Target Wins',xlab='Team Batting HBP',ylab='Target Wins')
#won't fix missing value in this x-var b/c won't use this in model b/c 
#1) x-var doesn't correlate with y-var 
#2) out of 2276 records, there are 2085 missing values => 92% missing => useless x-var 

#missing value TEAM_BASERUN_CS
skewness(moneyball$TEAM_BASERUN_CS,na.exclude(moneyball$TEAM_BASERUN_CS))
par(mfrow=c(1,2))
hist(moneyball$TEAM_BASERUN_CS,col='blue',main='Team Baserun CS Histogram',xlab='Team Baserun CS')
boxplot(moneyball$TEAM_BASERUN_CS,col='red',main='Team Baserun CS Boxplot')
#replace missing value with median b/c so many outliers in x-var
median_TEAM_BASERUN_CS=median(moneyball$TEAM_BASERUN_CS,na.exclude(moneyball$TEAM_BASERUN_CS))
median_TEAM_BASERUN_CS
moneyball$IMP_TEAM_BASERUN_CS=moneyball$TEAM_BASERUN_CS
moneyball$IMP_TEAM_BASERUN_CS[is.na(moneyball$IMP_TEAM_BASERUN_CS)]=median_TEAM_BASERUN_CS
str(moneyball$IMP_TEAM_BASERUN_CS)
summary(moneyball$IMP_TEAM_BASERUN_CS)
moneyball$M_TEAM_BASERUN_CS=moneyball$TEAM_BASERUN_CS
moneyball$M_TEAM_BASERUN_CS[moneyball$M_TEAM_BASERUN_CS>=0]=0
moneyball$M_TEAM_BASERUN_CS[is.na(moneyball$M_TEAM_BASERUN_CS)]=1
str(moneyball$M_TEAM_BASERUN_CS)
summary(moneyball$M_TEAM_BASERUN_CS)

#mising value TEAM_FIELDING_DP
skewness(moneyball$TEAM_FIELDING_DP,na.exclude(moneyball$TEAM_FIELDING_DP))
par(mfrow=c(1,2))
hist(moneyball$TEAM_FIELDING_DP,col='blue',main='Team Fielding DP Histogram',xlab='Team Field DP')
boxplot(moneyball$TEAM_FIELDING_DP,col='red',main='Team Fielding DP Boxplot')
#replace missing value with mean b/c don't have many outliers in x-var
mean_TEAM_FIELDING_DP=mean(moneyball$TEAM_FIELDING_DP,na.rm=TRUE)
mean_TEAM_FIELDING_DP
moneyball$IMP_TEAM_FIELDING_DP=moneyball$TEAM_FIELDING_DP
moneyball$IMP_TEAM_FIELDING_DP[is.na(moneyball$IMP_TEAM_FIELDING_DP)]=mean_TEAM_FIELDING_DP
str(moneyball$IMP_TEAM_FIELDING_DP)
summary(moneyball$IMP_TEAM_FIELDING_DP)
moneyball$M_TEAM_FIELDING_DP=moneyball$TEAM_FIELDING_DP
moneyball$M_TEAM_FIELDING_DP[moneyball$M_TEAM_FIELDING_DP>=0]=0
moneyball$M_TEAM_FIELDING_DP[is.na(moneyball$M_TEAM_FIELDING_DP)]=1
str(moneyball$M_TEAM_FIELDING_DP)
summary(moneyball$M_TEAM_FIELDING_DP)

#missing value TEAM_BASERUN_SB
skewness(moneyball$TEAM_BASERUN_SB,na.exclude(moneyball$TEAM_BASERUN_SB))
par(mfrow=c(1,2))
hist(moneyball$TEAM_BASERUN_SB,col='blue',main='Team Baserun SB Histogram',xlab='Team Baserun SB')
boxplot(moneyball$TEAM_BASERUN_SB,col='red',main='Team Baserun SB Boxplot')
#replace missing values with median b/c so many outliers in x-var
median_TEAM_BASERUN_SB=median(moneyball$TEAM_BASERUN_SB,na.exclude(moneyball$TEAM_BASERUN_SB))
median_TEAM_BASERUN_SB
moneyball$IMP_TEAM_BASERUN_SB=moneyball$TEAM_BASERUN_SB
moneyball$IMP_TEAM_BASERUN_SB[is.na(moneyball$IMP_TEAM_BASERUN_SB)]=median_TEAM_BASERUN_SB
str(moneyball$IMP_TEAM_BASERUN_SB)
summary(moneyball$IMP_TEAM_BASERUN_SB)
moneyball$M_TEAM_BASERUN_SB=moneyball$TEAM_BASERUN_SB
moneyball$M_TEAM_BASERUN_SB[moneyball$M_TEAM_BASERUN_SB>=0]=0
moneyball$M_TEAM_BASERUN_SB[is.na(moneyball$M_TEAM_BASERUN_SB)]=1
str(moneyball$M_TEAM_BASERUN_SB)
summary(moneyball$M_TEAM_BASERUN_SB)

#missing value TEAM_BATTING_SO
skewness(moneyball$TEAM_BATTING_SO,na.exclude(moneyball$TEAM_BATTING_SO))
par(mfrow=c(1,2))
hist(moneyball$TEAM_BATTING_SO,col='blue',main='Team Batting SO Histogram',xlab='Team Batting SO')
boxplot(moneyball$TEAM_BATTING_SO,col='red',main='Team Batting SO Boxplot')
#replace missing value with mean b/c don't have many outliers in x-var
mean_TEAM_BATTING_SO=mean(moneyball$TEAM_BATTING_SO,na.rm=TRUE)
mean_TEAM_BATTING_SO
moneyball$IMP_TEAM_BATTING_SO=moneyball$TEAM_BATTING_SO
moneyball$IMP_TEAM_BATTING_SO[is.na(moneyball$IMP_TEAM_BATTING_SO)]=mean_TEAM_BATTING_SO
str(moneyball$IMP_TEAM_BATTING_SO)
summary(moneyball$IMP_TEAM_BATTING_SO)
moneyball$M_TEAM_BATTING_SO=moneyball$TEAM_BATTING_SO
moneyball$M_TEAM_BATTING_SO[moneyball$M_TEAM_BATTING_SO>=0]=0
moneyball$M_TEAM_BATTING_SO[is.na(moneyball$M_TEAM_BATTING_SO)]=1
str(moneyball$M_TEAM_BATTING_SO)
summary(moneyball$M_TEAM_BATTING_SO)

#missing value TEAM_PITCHING_SO
skewness(moneyball$TEAM_PITCHING_SO,na.exclude(moneyball$TEAM_PITCHING_SO))
par(mfrow=c(1,2))
hist(moneyball$TEAM_PITCHING_SO,col='blue',main='Team Pitching SO Histogram',xlab='Team Pitching SO')
boxplot(moneyball$TEAM_PITCHING_SO,col='red',main='Team Pitching SO Boxplot')
#replace missing values with median b/c so many outliers in x-var
median_TEAM_PITCHING_SO=median(moneyball$TEAM_PITCHING_SO,na.exclude(moneyball$TEAM_PITCHING_SO))
median_TEAM_PITCHING_SO
moneyball$IMP_TEAM_PITCHING_SO=moneyball$TEAM_PITCHING_SO
moneyball$IMP_TEAM_PITCHING_SO[is.na(moneyball$IMP_TEAM_PITCHING_SO)]=median_TEAM_PITCHING_SO
str(moneyball$IMP_TEAM_PITCHING_SO)
summary(moneyball$IMP_TEAM_PITCHING_SO)
moneyball$M_TEAM_PITCHING_SO=moneyball$TEAM_PITCHING_SO
moneyball$M_TEAM_PITCHING_SO[moneyball$M_TEAM_PITCHING_SO>=0]=0
moneyball$M_TEAM_PITCHING_SO[is.na(moneyball$M_TEAM_PITCHING_SO)]=1
str(moneyball$M_TEAM_PITCHING_SO)
summary(moneyball$M_TEAM_PITCHING_SO)

#outlier issues
skewness(moneyball[2:7])
skewness(moneyball[12:14])
skewness(moneyball[16:16])
skewness(moneyball[18:18])
skewness(moneyball[20:20])
skewness(moneyball[22:22])
skewness(moneyball[24:24])
skewness(moneyball[26:26])
#small outlier issues: TEAM_BATTING_H, TEAM_BATTING_3B, TEAM_BATTING_BB
#moderate outlier issues: TEAM_FIELDING_E, IMP_TEAM_BASERUN_CS, IMP_TEAM_BASERUN_SB
#serious outlier issues:TEAM_PITCHING_H, TEAM_PITCHING_BB, IMP_TEAM_PITCHING_SO

#outlier TEAM_BATTING_H
moneyball$TRIM_TEAM_BATTING_H=moneyball$TEAM_BATTING_H
per99_TEAM_BATTING_H=quantile(moneyball$TRIM_TEAM_BATTING_H,0.99)
per99_TEAM_BATTING_H
par(mfrow=c(1,1))
plot(moneyball$TRIM_TEAM_BATTING_H,col='blue',main='Team Batting H at 99% percentile',ylab='Team Batting H')
abline(h=per99_TEAM_BATTING_H,col='red')
moneyball$TRIM_TEAM_BATTING_H[moneyball$TRIM_TEAM_BATTING_H>per99_TEAM_BATTING_H]=per99_TEAM_BATTING_H
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_BATTING_H,col='blue',main='Team Batting H original')
boxplot(moneyball$TRIM_TEAM_BATTING_H,col='red',main='Team Batting H trimmed')
skewness(moneyball$TEAM_BATTING_H)
skewness(moneyball$TRIM_TEAM_BATTING_H)

#outlier TEAM_BATTING_3B
moneyball$TRIM_TEAM_BATTING_3B=moneyball$TEAM_BATTING_3B
per99_TEAM_BATTING_3B=quantile(moneyball$TEAM_BATTING_3B,0.99)
per99_TEAM_BATTING_3B
par(mfrow=c(1,1))
plot(moneyball$TRIM_TEAM_BATTING_3B,col='blue',main='Team Batting 3B at 99% percentile',ylab='Team Batting 3B')
abline(h=per99_TEAM_BATTING_3B,col='red')
moneyball$TRIM_TEAM_BATTING_3B[moneyball$TRIM_TEAM_BATTING_3B>per99_TEAM_BATTING_3B]=per99_TEAM_BATTING_3B
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_BATTING_3B,col='blue',main='Team Batting 3B original')
boxplot(moneyball$TRIM_TEAM_BATTING_3B,col='red',main='Team Batting 3B trimmed')
skewness(moneyball$TEAM_BATTING_3B)
skewness(moneyball$TRIM_TEAM_BATTING_3B)

#outlier TEAM_BATTING_BB
moneyball$TRIM_TEAM_BATTING_BB=moneyball$TEAM_BATTING_BB
per99upper_TEAM_BATTING_BB=quantile(moneyball$TEAM_BATTING_BB,0.99)
per99upper_TEAM_BATTING_BB
per99lower_TEAM_BATTING_BB=quantile(moneyball$TEAM_BATTING_BB,0.01)
per99lower_TEAM_BATTING_BB
par(mfrow=c(1,1))
plot(moneyball$TRIM_TEAM_BATTING_BB,col='blue',main='Team Batting BB at 99% percentile',ylab='Team Batting BB')
abline(h=per99upper_TEAM_BATTING_BB,col='red')
abline(h=per99lower_TEAM_BATTING_BB,col='red')
moneyball$TRIM_TEAM_BATTING_BB[moneyball$TRIM_TEAM_BATTING_BB>per99upper_TEAM_BATTING_BB]=per99upper_TEAM_BATTING_BB
moneyball$TRIM_TEAM_BATTING_BB[moneyball$TRIM_TEAM_BATTING_BB<per99lower_TEAM_BATTING_BB]=per99lower_TEAM_BATTING_BB
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_BATTING_BB,col='blue',main='Team Batting BB original')
boxplot(moneyball$TRIM_TEAM_BATTING_BB,col='red',main='Team Batting BB trimmed')
skewness(moneyball$TEAM_BATTING_BB)
skewness(moneyball$TRIM_TEAM_BATTING_BB)
#the 99% percentile trim still doesn't fix the problem, so will try another outlier technique: z-standardization
sd3upper_TEAM_BATTING_BB=mean(moneyball$TEAM_BATTING_BB)+3*sd(moneyball$TEAM_BATTING_BB)
sd3upper_TEAM_BATTING_BB
sd3lower_TEAM_BATTING_BB=mean(moneyball$TEAM_BATTING_BB)-3*sd(moneyball$TEAM_BATTING_BB)
sd3lower_TEAM_BATTING_BB
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_TEAM_BATTING_BB,col='blue',main='Team Batting BB at 3 standard deviations')
abline(h=sd3lower_TEAM_BATTING_BB,col='red')
abline(h=sd3upper_TEAM_BATTING_BB,col='red')
moneyball$TRIM_TEAM_BATTING_BB[moneyball$TRIM_TEAM_BATTING_BB>sd3upper_TEAM_BATTING_BB]=sd3upper_TEAM_BATTING_BB
moneyball$TRIM_TEAM_BATTING_BB[moneyball$TRIM_TEAM_BATTING_BB<sd3lower_TEAM_BATTING_BB]=sd3lower_TEAM_BATTING_BB
summary(moneyball$TRIM_TEAM_BATTING_BB)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_BATTING_BB,col='blue',main='Team Batting BB original')
boxplot(moneyball$TRIM_TEAM_BATTING_BB,col='red',main='Team Batting BB trimmed')
skewness(moneyball$TEAM_BATTING_BB)
skewness(moneyball$TRIM_TEAM_BATTING_BB)

#outlier TEAM_FIELDING_E
moneyball$TRIM_TEAM_FIELDING_E=moneyball$TEAM_FIELDING_E
sd3upper_TEAM_FIELDING_E=mean(moneyball$TEAM_FIELDING_E)+3*sd(moneyball$TEAM_FIELDING_E)
sd3upper_TEAM_FIELDING_E
sd3lower_TEAM_FIELDING_E=mean(moneyball$TEAM_FIELDING_E)-3*sd(moneyball$TEAM_FIELDING_E)
sd3lower_TEAM_FIELDING_E
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_TEAM_FIELDING_E,col='blue',main='Team Fielding E at 3 standard deviations')
abline(h=sd3lower_TEAM_FIELDING_E,col='red')
abline(h=sd3upper_TEAM_FIELDING_E,col='red')
moneyball$TRIM_TEAM_FIELDING_E[moneyball$TRIM_TEAM_FIELDING_E>sd3upper_TEAM_FIELDING_E]=sd3upper_TEAM_FIELDING_E
moneyball$TRIM_TEAM_FIELDING_E[moneyball$TRIM_TEAM_FIELDING_E<sd3lower_TEAM_FIELDING_E]=sd3lower_TEAM_FIELDING_E
summary(moneyball$TRIM_TEAM_FIELDING_E)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_FIELDING_E,col='blue',main='Team Fielding E original')
boxplot(moneyball$TRIM_TEAM_FIELDING_E,col='red',main='Team Fielding E trimmed')
skewness(moneyball$TEAM_FIELDING_E)
skewness(moneyball$TRIM_TEAM_FIELDING_E)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_TEAM_FIELDING_E=quantile(moneyball$TRIM_TEAM_FIELDING_E,0.95)
per95upper_TEAM_FIELDING_E
per95lower_TEAM_FIELDING_E=quantile(moneyball$TRIM_TEAM_FIELDING_E,0.05)
per95lower_TEAM_FIELDING_E
moneyball$TRIM_TEAM_FIELDING_E[moneyball$TRIM_TEAM_FIELDING_E<per95lower_TEAM_FIELDING_E]=per95lower_TEAM_FIELDING_E
moneyball$TRIM_TEAM_FIELDING_E[moneyball$TRIM_TEAM_FIELDING_E>per95upper_TEAM_FIELDING_E]=per95upper_TEAM_FIELDING_E
summary(moneyball$TRIM_TEAM_FIELDING_E)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_FIELDING_E,col='blue',main='Team Fielding E original')
boxplot(moneyball$TRIM_TEAM_FIELDING_E,col='red',main='Team Fielding E trimmed')
skewness(moneyball$TEAM_FIELDING_E)
skewness(moneyball$TRIM_TEAM_FIELDING_E)

#outlier IMP_TEAM_BASERUN_CS
moneyball$TRIM_IMP_TEAM_BASERUN_CS=moneyball$IMP_TEAM_BASERUN_CS
sd3upper_IMP_TEAM_BASERUN_CS=mean(moneyball$IMP_TEAM_BASERUN_CS)+3*sd(moneyball$IMP_TEAM_BASERUN_CS)
sd3upper_IMP_TEAM_BASERUN_CS
sd3lower_IMP_TEAM_BASERUN_CS=mean(moneyball$IMP_TEAM_BASERUN_CS)-3*sd(moneyball$IMP_TEAM_BASERUN_CS)
sd3lower_IMP_TEAM_BASERUN_CS
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_IMP_TEAM_BASERUN_CS,col='blue',main='IMP Team Baserun CS at 3 standard deviations')
abline(h=sd3lower_IMP_TEAM_BASERUN_CS,col='red')
abline(h=sd3upper_IMP_TEAM_BASERUN_CS,col='red')
moneyball$TRIM_IMP_TEAM_BASERUN_CS[moneyball$TRIM_IMP_TEAM_BASERUN_CS>sd3upper_IMP_TEAM_BASERUN_CS]=sd3upper_IMP_TEAM_BASERUN_CS
moneyball$TRIM_IMP_TEAM_BASERUN_CS[moneyball$TRIM_IMP_TEAM_BASERUN_CS<sd3lower_IMP_TEAM_BASERUN_CS]=sd3lower_IMP_TEAM_BASERUN_CS
summary(moneyball$TRIM_IMP_TEAM_BASERUN_CS)
par(mfrow=c(1,2))
boxplot(moneyball$IMP_TEAM_BASERUN_CS,col='blue',main='IMP Team Baserun CS original')
boxplot(moneyball$TRIM_IMP_TEAM_BASERUN_CS,col='red',main='IMP Team Baserun CS trimmed')
skewness(moneyball$IMP_TEAM_BASERUN_CS)
skewness(moneyball$TRIM_IMP_TEAM_BASERUN_CS)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_IMP_TEAM_BASERUN_CS=quantile(moneyball$TRIM_IMP_TEAM_BASERUN_CS,0.95)
per95upper_IMP_TEAM_BASERUN_CS
per95lower_IMP_TEAM_BASERUN_CS=quantile(moneyball$TRIM_IMP_TEAM_BASERUN_CS,0.05)
per95lower_IMP_TEAM_BASERUN_CS
moneyball$TRIM_IMP_TEAM_BASERUN_CS[moneyball$TRIM_IMP_TEAM_BASERUN_CS<per95lower_IMP_TEAM_BASERUN_CS]=per95lower_IMP_TEAM_BASERUN_CS
moneyball$TRIM_IMP_TEAM_BASERUN_CS[moneyball$TRIM_IMP_TEAM_BASERUN_CS>per95upper_IMP_TEAM_BASERUN_CS]=per95upper_IMP_TEAM_BASERUN_CS
summary(moneyball$TRIM_IMP_TEAM_BASERUN_CS)
par(mfrow=c(1,2))
boxplot(moneyball$IMP_TEAM_BASERUN_CS,col='blue',main='IMP Team Baserun CS original')
boxplot(moneyball$TRIM_IMP_TEAM_BASERUN_CS,col='red',main='IMP Team Baserun CS trimmed')
skewness(moneyball$IMP_TEAM_BASERUN_CS)
skewness(moneyball$TRIM_IMP_TEAM_BASERUN_CS)

#outlier IMP_TEAM_BASERUN_SB
moneyball$TRIM_IMP_TEAM_BASERUN_SB=moneyball$IMP_TEAM_BASERUN_SB
sd3upper_IMP_TEAM_BASERUN_SB=mean(moneyball$IMP_TEAM_BASERUN_SB)+3*sd(moneyball$IMP_TEAM_BASERUN_SB)
sd3upper_IMP_TEAM_BASERUN_SB
sd3lower_IMP_TEAM_BASERUN_SB=mean(moneyball$IMP_TEAM_BASERUN_SB)-3*sd(moneyball$IMP_TEAM_BASERUN_SB)
sd3lower_IMP_TEAM_BASERUN_SB
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_IMP_TEAM_BASERUN_SB,col='blue',main='IMP Team Baserun SB at 3 standard deviations')
abline(h=sd3lower_IMP_TEAM_BASERUN_SB,col='red')
abline(h=sd3upper_IMP_TEAM_BASERUN_SB,col='red')
moneyball$TRIM_IMP_TEAM_BASERUN_SB[moneyball$TRIM_IMP_TEAM_BASERUN_SB>sd3upper_IMP_TEAM_BASERUN_SB]=sd3upper_IMP_TEAM_BASERUN_SB
moneyball$TRIM_IMP_TEAM_BASERUN_SB[moneyball$TRIM_IMP_TEAM_BASERUN_SB<sd3lower_IMP_TEAM_BASERUN_SB]=sd3lower_IMP_TEAM_BASERUN_SB
summary(moneyball$TRIM_IMP_TEAM_BASERUN_SB)
par(mfrow=c(1,2))
boxplot(moneyball$IMP_TEAM_BASERUN_SB,col='blue',main='IMP Team Baserun SB original')
boxplot(moneyball$TRIM_IMP_TEAM_BASERUN_SB,col='red',main='IMP Team Baserun SB trimmed')
skewness(moneyball$IMP_TEAM_BASERUN_SB)
skewness(moneyball$TRIM_IMP_TEAM_BASERUN_SB)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_IMP_TEAM_BASERUN_SB=quantile(moneyball$TRIM_IMP_TEAM_BASERUN_SB,0.95)
per95upper_IMP_TEAM_BASERUN_SB
per95lower_IMP_TEAM_BASERUN_SB=quantile(moneyball$TRIM_IMP_TEAM_BASERUN_SB,0.05)
per95lower_IMP_TEAM_BASERUN_SB
moneyball$TRIM_IMP_TEAM_BASERUN_SB[moneyball$TRIM_IMP_TEAM_BASERUN_SB<per95lower_IMP_TEAM_BASERUN_SB]=per95lower_IMP_TEAM_BASERUN_SB
moneyball$TRIM_IMP_TEAM_BASERUN_SB[moneyball$TRIM_IMP_TEAM_BASERUN_SB>per95upper_IMP_TEAM_BASERUN_SB]=per95upper_IMP_TEAM_BASERUN_SB
summary(moneyball$TRIM_IMP_TEAM_BASERUN_SB)
par(mfrow=c(1,2))
boxplot(moneyball$IMP_TEAM_BASERUN_SB,col='blue',main='IMP Team Baserun SB original')
boxplot(moneyball$TRIM_IMP_TEAM_BASERUN_SB,col='red',main='IMP Team Baserun SB trimmed')
skewness(moneyball$IMP_TEAM_BASERUN_SB)
skewness(moneyball$TRIM_IMP_TEAM_BASERUN_SB)

#outlier TEAM_PITCHING_H
moneyball$TRIM_TEAM_PITCHING_H=moneyball$TEAM_PITCHING_H
sd3upper_TEAM_PITCHING_H=mean(moneyball$TEAM_PITCHING_H)+3*sd(moneyball$TEAM_PITCHING_H)
sd3upper_TEAM_PITCHING_H
sd3lower_TEAM_PITCHING_H=mean(moneyball$TEAM_PITCHING_H)-3*sd(moneyball$TEAM_PITCHING_H)
sd3lower_TEAM_PITCHING_H
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_TEAM_PITCHING_H,col='blue',main='Team Pitching H at 3 standard deviations')
abline(h=sd3lower_TEAM_PITCHING_H,col='red')
abline(h=sd3upper_TEAM_PITCHING_H,col='red')
moneyball$TRIM_TEAM_PITCHING_H[moneyball$TRIM_TEAM_PITCHING_H>sd3upper_TEAM_PITCHING_H]=sd3upper_TEAM_PITCHING_H
moneyball$TRIM_TEAM_PITCHING_H[moneyball$TRIM_TEAM_PITCHING_H<sd3lower_TEAM_PITCHING_H]=sd3lower_TEAM_PITCHING_H
summary(moneyball$TRIM_TEAM_PITCHING_H)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_PITCHING_H,col='blue',main='Team Pitching H original')
boxplot(moneyball$TRIM_TEAM_PITCHING_H,col='red',main='Team Pitching H trimmed')
skewness(moneyball$TEAM_PITCHING_H)
skewness(moneyball$TRIM_TEAM_PITCHING_H)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_TEAM_PITCHING_H=quantile(moneyball$TRIM_TEAM_PITCHING_H,0.95)
per95upper_TEAM_PITCHING_H
per95lower_TEAM_PITCHING_H=quantile(moneyball$TRIM_TEAM_PITCHING_H,0.05)
per95lower_TEAM_PITCHING_H
moneyball$TRIM_TEAM_PITCHING_H[moneyball$TRIM_TEAM_PITCHING_H<per95lower_TEAM_PITCHING_H]=per95lower_TEAM_PITCHING_H
moneyball$TRIM_TEAM_PITCHING_H[moneyball$TRIM_TEAM_PITCHING_H>per95upper_TEAM_PITCHING_H]=per95upper_TEAM_PITCHING_H
summary(moneyball$TRIM_TEAM_PITCHING_H)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_PITCHING_H,col='blue',main='Team Pitching H original')
boxplot(moneyball$TRIM_TEAM_PITCHING_H,col='red',main='Team Pitching H trimmed')
skewness(moneyball$TEAM_PITCHING_H)
skewness(moneyball$TRIM_TEAM_PITCHING_H)

#outlier TEAM_PITCHING_BB
moneyball$TRIM_TEAM_PITCHING_BB=moneyball$TEAM_PITCHING_BB
sd3upper_TEAM_PITCHING_BB=mean(moneyball$TEAM_PITCHING_BB)+3*sd(moneyball$TEAM_PITCHING_BB)
sd3upper_TEAM_PITCHING_BB
sd3lower_TEAM_PITCHING_BB=mean(moneyball$TEAM_PITCHING_BB)-3*sd(moneyball$TEAM_PITCHING_BB)
sd3lower_TEAM_PITCHING_BB
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_TEAM_PITCHING_BB,col='blue',main='Team Pitching BB at 3 standard deviations')
abline(h=sd3lower_TEAM_PITCHING_BB,col='red')
abline(h=sd3upper_TEAM_PITCHING_BB,col='red')
moneyball$TRIM_TEAM_PITCHING_BB[moneyball$TRIM_TEAM_PITCHING_BB>sd3upper_TEAM_PITCHING_BB]=sd3upper_TEAM_PITCHING_BB
moneyball$TRIM_TEAM_PITCHING_BB[moneyball$TRIM_TEAM_PITCHING_BB<sd3lower_TEAM_PITCHING_BB]=sd3lower_TEAM_PITCHING_BB
summary(moneyball$TRIM_TEAM_PITCHING_BB)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_PITCHING_BB,col='blue',main='Team Pitching BB original')
boxplot(moneyball$TRIM_TEAM_PITCHING_BB,col='red',main='Team Pitching BB trimmed')
skewness(moneyball$TEAM_PITCHING_BB)
skewness(moneyball$TRIM_TEAM_PITCHING_BB)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_TEAM_PITCHING_BB=quantile(moneyball$TRIM_TEAM_PITCHING_BB,0.95)
per95upper_TEAM_PITCHING_BB
per95lower_TEAM_PITCHING_BB=quantile(moneyball$TRIM_TEAM_PITCHING_BB,0.05)
per95lower_TEAM_PITCHING_BB
moneyball$TRIM_TEAM_PITCHING_BB[moneyball$TRIM_TEAM_PITCHING_BB<per95lower_TEAM_PITCHING_BB]=per95lower_TEAM_PITCHING_BB
moneyball$TRIM_TEAM_PITCHING_BB[moneyball$TRIM_TEAM_PITCHING_BB>per95upper_TEAM_PITCHING_BB]=per95upper_TEAM_PITCHING_BB
summary(moneyball$TRIM_TEAM_PITCHING_BB)
par(mfrow=c(1,2))
boxplot(moneyball$TEAM_PITCHING_BB,col='blue',main='Team Pitching BB original')
boxplot(moneyball$TRIM_TEAM_PITCHING_BB,col='red',main='Team Pitching BB trimmed')
skewness(moneyball$TEAM_PITCHING_BB)
skewness(moneyball$TRIM_TEAM_PITCHING_BB)

#outlier IMP_TEAM_PITCHING_SO
moneyball$TRIM_IMP_TEAM_PITCHING_SO=moneyball$IMP_TEAM_PITCHING_SO
sd3upper_IMP_TEAM_PITCHING_SO=mean(moneyball$IMP_TEAM_PITCHING_SO)+3*sd(moneyball$IMP_TEAM_PITCHING_SO)
sd3upper_IMP_TEAM_PITCHING_SO
sd3lower_IMP_TEAM_PITCHING_SO=mean(moneyball$IMP_TEAM_PITCHING_SO)-3*sd(moneyball$IMP_TEAM_PITCHING_SO)
sd3lower_IMP_TEAM_PITCHING_SO
par(mfrow=c(1,1))
boxplot(moneyball$TRIM_IMP_TEAM_PITCHING_SO,col='blue',main='IMP Team Pitching SO at 3 standard deviations')
abline(h=sd3lower_IMP_TEAM_PITCHING_SO,col='red')
abline(h=sd3upper_IMP_TEAM_PITCHING_SO,col='red')
moneyball$TRIM_IMP_TEAM_PITCHING_SO[moneyball$TRIM_IMP_TEAM_PITCHING_SO>sd3upper_IMP_TEAM_PITCHING_SO]=sd3upper_IMP_TEAM_PITCHING_SO
moneyball$TRIM_IMP_TEAM_PITCHING_SO[moneyball$TRIM_IMP_TEAM_PITCHING_SO<sd3lower_IMP_TEAM_PITCHING_SO]=sd3lower_IMP_TEAM_PITCHING_SO
summary(moneyball$TRIM_IMP_TEAM_PITCHING_SO)
par(mfrow=c(1,2))
boxplot(moneyball$IMP_TEAM_PITCHING_SO,col='blue',main='IMP Team Pitching SO original')
boxplot(moneyball$TRIM_IMP_TEAM_PITCHING_SO,col='red',main='IMP Team Pitching SO trimmed')
skewness(moneyball$IMP_TEAM_PITCHING_SO)
skewness(moneyball$TRIM_IMP_TEAM_PITCHING_SO)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_IMP_TEAM_PITCHING_SO=quantile(moneyball$TRIM_IMP_TEAM_PITCHING_SO,0.95)
per95upper_IMP_TEAM_PITCHING_SO
per95lower_IMP_TEAM_PITCHING_SO=quantile(moneyball$TRIM_IMP_TEAM_PITCHING_SO,0.05)
per95lower_IMP_TEAM_PITCHING_SO
moneyball$TRIM_IMP_TEAM_PITCHING_SO[moneyball$TRIM_IMP_TEAM_PITCHING_SO<per95lower_IMP_TEAM_PITCHING_SO]=per95lower_IMP_TEAM_PITCHING_SO
moneyball$TRIM_IMP_TEAM_PITCHING_SO[moneyball$TRIM_IMP_TEAM_PITCHING_SO>per95upper_IMP_TEAM_PITCHING_SO]=per95upper_IMP_TEAM_PITCHING_SO
summary(moneyball$TRIM_IMP_TEAM_PITCHING_SO)
par(mfrow=c(1,2))
boxplot(moneyball$IMP_TEAM_PITCHING_SO,col='blue',main='IMP Team Pitching SO original')
boxplot(moneyball$TRIM_IMP_TEAM_PITCHING_SO,col='red',main='IMP Team Pitching SO trimmed')
skewness(moneyball$IMP_TEAM_PITCHING_SO)
skewness(moneyball$TRIM_IMP_TEAM_PITCHING_SO)

str(moneyball)
summary(moneyball)

#part 3: model development

model=data.frame(moneyball$INDEX,moneyball$TARGET_WINS,moneyball$TEAM_BATTING_2B,moneyball$TEAM_BATTING_HR,moneyball$TEAM_PITCHING_HR,moneyball$M_TEAM_BASERUN_CS,moneyball$M_TEAM_FIELDING_DP,moneyball$M_TEAM_BASERUN_SB,moneyball$M_TEAM_BATTING_SO,moneyball$M_TEAM_PITCHING_SO,moneyball$IMP_TEAM_FIELDING_DP,moneyball$IMP_TEAM_BATTING_SO,moneyball[28:36])
str(model)
names(model)=c('INDEX','TARGET_WINS','TEAM_BATTING_2B','TEAM_BATTING_HR','TEAM_PITCHING_HR','M_TEAM_BASERUN_CS','M_TEAM_FIELDING_DP','M_TEAM_BASERUN_SB','M_TEAM_BATTING_SO','M_TEAM_PITCHING_SO','IMP_TEAM_FIELDING_DP','IMP_TEAM_BATTING_SO','TRIM_TEAM_BATTING_H','TRIM_TEAM_BATTING_3B','TRIM_TEAM_BATTING_BB','TRIM_TEAM_FIELDING_E','TRIM_IMP_TEAM_BASERUN_CS','TRIM_IMP_TEAM_BASERUN_SB','TRIM_TEAM_PITCHING_H','TRIM_TEAM_PITCHING_BB','TRIM_IMP_TEAM_PITCHING_SO')
str(model)
summary(model)
modelnum=data.frame(moneyball$INDEX,moneyball$TARGET_WINS,moneyball$TEAM_BATTING_2B,moneyball$TEAM_BATTING_HR,moneyball$TEAM_PITCHING_HR,moneyball$IMP_TEAM_FIELDING_DP,moneyball$IMP_TEAM_BATTING_SO,moneyball[28:36])
names(modelnum)=c('INDEX','TARGET_WINS','TEAM_BATTING_2B','TEAM_BATTING_HR','TEAM_PITCHING_HR','IMP_TEAM_FIELDING_DP','IMP_TEAM_BATTING_SO','TRIM_TEAM_BATTING_H','TRIM_TEAM_BATTING_3B','TRIM_TEAM_BATTING_BB','TRIM_TEAM_FIELDING_E','TRIM_IMP_TEAM_BASERUN_CS','TRIM_IMP_TEAM_BASERUN_SB','TRIM_TEAM_PITCHING_H','TRIM_TEAM_PITCHING_BB','TRIM_IMP_TEAM_PITCHING_SO')
str(modelnum)
summary(modelnum)
par(mfrow=c(1,1))
pairs(modelnum[1:8],upper.panel=panel.cor)
pairs(modelnum[9:16],upper.panel=panel.cor)
pairs(modelnum[2:16],upper.panel=panel.cor)
mse=function(sm)
  mean(sm$residuals^2)

#model #1: stepwise
model1=lm(TARGET_WINS~.,data=model)
model1=stepAIC(model1,direction='both')
summary(model1)
vif(model1)
model1=lm(model$TARGET_WINS~model$TEAM_PITCHING_HR+model$M_TEAM_FIELDING_DP+model$M_TEAM_BASERUN_SB+model$M_TEAM_BATTING_SO+model$IMP_TEAM_FIELDING_DP+model$IMP_TEAM_BATTING_SO+model$TRIM_TEAM_BATTING_H+model$TRIM_TEAM_BATTING_3B+model$TRIM_TEAM_BATTING_BB+model$TRIM_TEAM_FIELDING_E+model$TRIM_IMP_TEAM_BASERUN_CS+model$TRIM_IMP_TEAM_BASERUN_SB+model$TRIM_TEAM_PITCHING_H+model$TRIM_IMP_TEAM_PITCHING_SO)
summary(model1)
vif(model1)
#predictors with severe VIF issues: IMP_TEAM_BATTING_SO, TRIM_IMP_TEAM_PITCHING_SO
#predictors with light VIF issues: TRIM_TEAM_FIELDING_E, TRIM_TEAM_PITCHING_H
mse(model1)
AIC(model1)

#model #2: backward selection
model2=lm(TARGET_WINS~.,data=model)
model2=stepAIC(model1,direction='backward')
summary(model2)
vif(model2)
mse(model2)
AIC(model2)

#model 3: forward selection
model3=lm(TARGET_WINS~.,data=model)
model3=stepAIC(model1,direction='forward')
summary(model3)
vif(model3)
mse(model3)
AIC(model3)
#all 3 models using stepwise, backward, forward gave the same results/model
#model 4 will address VIF issues

#model 4: model with no VIF issues
#drop predictor with the worst VIF issues: IMP_TEAM_BATTING_SO
model4=lm(model$TARGET_WINS~model$TEAM_PITCHING_HR+ model$M_TEAM_FIELDING_DP+ 
            model$M_TEAM_BASERUN_SB+ model$M_TEAM_BATTING_SO+ model$IMP_TEAM_FIELDING_DP+ 
            model$TRIM_TEAM_BATTING_H+ model$TRIM_TEAM_BATTING_3B+ 
            model$TRIM_TEAM_BATTING_BB+ model$TRIM_TEAM_FIELDING_E+ model$TRIM_IMP_TEAM_BASERUN_CS+ 
            model$TRIM_IMP_TEAM_BASERUN_SB+ model$TRIM_TEAM_PITCHING_H+ 
            model$TRIM_IMP_TEAM_PITCHING_SO,model)
vif(model4)
#still has VIF issue with TRIM_TEAM_FIELDING_E, so will continue to get rid of it 
model4=lm(model$TARGET_WINS~model$TEAM_PITCHING_HR+ model$M_TEAM_FIELDING_DP+ 
            model$M_TEAM_BASERUN_SB+ model$M_TEAM_BATTING_SO+ model$IMP_TEAM_FIELDING_DP+ 
            model$TRIM_TEAM_BATTING_H+ model$TRIM_TEAM_BATTING_3B+ 
            model$TRIM_TEAM_BATTING_BB+ model$TRIM_IMP_TEAM_BASERUN_CS+ 
            model$TRIM_IMP_TEAM_BASERUN_SB+ model$TRIM_TEAM_PITCHING_H+ 
            model$TRIM_IMP_TEAM_PITCHING_SO,model)
vif(model4) #no more VIF issue
summary(model4)
#TRIM_IMP_TEAM_BASERUN_CS is now no longer significant, so remove this one
model4=lm(model$TARGET_WINS~model$TEAM_PITCHING_HR+ model$M_TEAM_FIELDING_DP+ 
            model$M_TEAM_BASERUN_SB+ model$M_TEAM_BATTING_SO+ model$IMP_TEAM_FIELDING_DP+ 
            model$TRIM_TEAM_BATTING_H+ model$TRIM_TEAM_BATTING_3B+ 
            model$TRIM_TEAM_BATTING_BB+ 
            model$TRIM_IMP_TEAM_BASERUN_SB+ model$TRIM_TEAM_PITCHING_H+ 
            model$TRIM_IMP_TEAM_PITCHING_SO,model)
summary(model4)
mse(model4)
AIC(model4)
#some predictors in model 4 don't match industry knowledge
#TEAM_PITCHING_HR, IMP_TEAM_FIELDING_DP, TRIM_IMP_TEAM_PITCHING_SO
summary(lm(model$TARGET_WINS~model$TEAM_PITCHING_HR,model))
summary(lm(model$TARGET_WINS~model$IMP_TEAM_FIELDING_DP,model))
summary(lm(model$TARGET_WINS~model$TRIM_IMP_TEAM_PITCHING_SO,model))
#model 5 will remove them to match industry knowledge

#model 5: matching industry knowledge
model5=lm(model$TARGET_WINS~model$M_TEAM_FIELDING_DP+ 
            model$M_TEAM_BASERUN_SB+ model$M_TEAM_BATTING_SO+    model$TRIM_TEAM_BATTING_H+ model$TRIM_TEAM_BATTING_3B+ model$TRIM_TEAM_BATTING_BB+ 
            model$TRIM_IMP_TEAM_BASERUN_SB+ model$TRIM_TEAM_PITCHING_H,model)
vif(model5)
summary(model5) 
#all betas match industry knowledge now
#now TRIM_TEAM_BATTING_3B is not significant at 95% level anymore, but still significant at 90%
#so we'll keep it in the model
mse(model5)
AIC(model5)

#part 4: model selection

modelcomp=c('model 1/2/3','model 4','model5')
adjr2comp=c(round(summary(model1)$adj.r.squared,2),round(summary(model4)$adj.r.squared,2),round(summary(model5)$adj.r.squared,2))
msecomp=c(round(mse(model1),2),round(mse(model4),2),round(mse(model5),2))
AICcomp=c(round(AIC(model1),2),round(AIC(model4),2),round(AIC(model5),2))
tablecomp=cbind(modelcomp,adjr2comp,msecomp,AICcomp)
tablecomp
#choose model 5 though its adjusted R2, MSE, AIC are the worst b/c
#model 5 has no VIF issues, unlike model 1
#model 5 matches industry knowlege, unlike model 4
#model 5 overall is still significant using overall F test with each significant beta at 90% level
#in model deployment section below, model 5 has the closest mean to training y-var's mean


