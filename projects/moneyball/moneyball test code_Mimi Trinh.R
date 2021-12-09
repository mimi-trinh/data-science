#Mimi Trinh
#part 5: model deployment

setwd('/Users/mimitrinh/Desktop')
moneyball_test=read.csv('moneyball_test.csv',header=T)
skewness(moneyball_test$TEAM_BASERUN_CS,na.exclude(moneyball_test$TEAM_BASERUN_CS))
par(mfrow=c(1,2))
hist(moneyball_test$TEAM_BASERUN_CS,col='blue',main='Team Baserun CS Histogram',xlab='Team Baserun CS')
boxplot(moneyball_test$TEAM_BASERUN_CS,col='red',main='Team Baserun CS Boxplot')
#replace missing value with median b/c so many outliers in x-var
median_TEAM_BASERUN_CS=median(moneyball_test$TEAM_BASERUN_CS,na.exclude(moneyball_test$TEAM_BASERUN_CS))
median_TEAM_BASERUN_CS
moneyball_test$IMP_TEAM_BASERUN_CS=moneyball_test$TEAM_BASERUN_CS
moneyball_test$IMP_TEAM_BASERUN_CS[is.na(moneyball_test$IMP_TEAM_BASERUN_CS)]=median_TEAM_BASERUN_CS
str(moneyball_test$IMP_TEAM_BASERUN_CS)
summary(moneyball_test$IMP_TEAM_BASERUN_CS)
moneyball_test$M_TEAM_BASERUN_CS=moneyball_test$TEAM_BASERUN_CS
moneyball_test$M_TEAM_BASERUN_CS[moneyball_test$M_TEAM_BASERUN_CS>=0]=0
moneyball_test$M_TEAM_BASERUN_CS[is.na(moneyball_test$M_TEAM_BASERUN_CS)]=1
str(moneyball_test$M_TEAM_BASERUN_CS)
summary(moneyball_test$M_TEAM_BASERUN_CS)

#mising value TEAM_FIELDING_DP
skewness(moneyball_test$TEAM_FIELDING_DP,na.exclude(moneyball_test$TEAM_FIELDING_DP))
par(mfrow=c(1,2))
hist(moneyball_test$TEAM_FIELDING_DP,col='blue',main='Team Fielding DP Histogram',xlab='Team Field DP')
boxplot(moneyball_test$TEAM_FIELDING_DP,col='red',main='Team Fielding DP Boxplot')
#replace missing value with mean b/c don't have many outliers in x-var
mean_TEAM_FIELDING_DP=mean(moneyball_test$TEAM_FIELDING_DP,na.rm=TRUE)
mean_TEAM_FIELDING_DP
moneyball_test$IMP_TEAM_FIELDING_DP=moneyball_test$TEAM_FIELDING_DP
moneyball_test$IMP_TEAM_FIELDING_DP[is.na(moneyball_test$IMP_TEAM_FIELDING_DP)]=mean_TEAM_FIELDING_DP
str(moneyball_test$IMP_TEAM_FIELDING_DP)
summary(moneyball_test$IMP_TEAM_FIELDING_DP)
moneyball_test$M_TEAM_FIELDING_DP=moneyball_test$TEAM_FIELDING_DP
moneyball_test$M_TEAM_FIELDING_DP[moneyball_test$M_TEAM_FIELDING_DP>=0]=0
moneyball_test$M_TEAM_FIELDING_DP[is.na(moneyball_test$M_TEAM_FIELDING_DP)]=1
str(moneyball_test$M_TEAM_FIELDING_DP)
summary(moneyball_test$M_TEAM_FIELDING_DP)

#missing value TEAM_BASERUN_SB
skewness(moneyball_test$TEAM_BASERUN_SB,na.exclude(moneyball_test$TEAM_BASERUN_SB))
par(mfrow=c(1,2))
hist(moneyball_test$TEAM_BASERUN_SB,col='blue',main='Team Baserun SB Histogram',xlab='Team Baserun SB')
boxplot(moneyball_test$TEAM_BASERUN_SB,col='red',main='Team Baserun SB Boxplot')
#replace missing values with median b/c so many outliers in x-var
median_TEAM_BASERUN_SB=median(moneyball_test$TEAM_BASERUN_SB,na.exclude(moneyball_test$TEAM_BASERUN_SB))
median_TEAM_BASERUN_SB
moneyball_test$IMP_TEAM_BASERUN_SB=moneyball_test$TEAM_BASERUN_SB
moneyball_test$IMP_TEAM_BASERUN_SB[is.na(moneyball_test$IMP_TEAM_BASERUN_SB)]=median_TEAM_BASERUN_SB
str(moneyball_test$IMP_TEAM_BASERUN_SB)
summary(moneyball_test$IMP_TEAM_BASERUN_SB)
moneyball_test$M_TEAM_BASERUN_SB=moneyball_test$TEAM_BASERUN_SB
moneyball_test$M_TEAM_BASERUN_SB[moneyball_test$M_TEAM_BASERUN_SB>=0]=0
moneyball_test$M_TEAM_BASERUN_SB[is.na(moneyball_test$M_TEAM_BASERUN_SB)]=1
str(moneyball_test$M_TEAM_BASERUN_SB)
summary(moneyball_test$M_TEAM_BASERUN_SB)

#missing value TEAM_BATTING_SO
skewness(moneyball_test$TEAM_BATTING_SO,na.exclude(moneyball_test$TEAM_BATTING_SO))
par(mfrow=c(1,2))
hist(moneyball_test$TEAM_BATTING_SO,col='blue',main='Team Batting SO Histogram',xlab='Team Batting SO')
boxplot(moneyball_test$TEAM_BATTING_SO,col='red',main='Team Batting SO Boxplot')
#replace missing value with mean b/c don't have many outliers in x-var
mean_TEAM_BATTING_SO=mean(moneyball_test$TEAM_BATTING_SO,na.rm=TRUE)
mean_TEAM_BATTING_SO
moneyball_test$IMP_TEAM_BATTING_SO=moneyball_test$TEAM_BATTING_SO
moneyball_test$IMP_TEAM_BATTING_SO[is.na(moneyball_test$IMP_TEAM_BATTING_SO)]=mean_TEAM_BATTING_SO
str(moneyball_test$IMP_TEAM_BATTING_SO)
summary(moneyball_test$IMP_TEAM_BATTING_SO)
moneyball_test$M_TEAM_BATTING_SO=moneyball_test$TEAM_BATTING_SO
moneyball_test$M_TEAM_BATTING_SO[moneyball_test$M_TEAM_BATTING_SO>=0]=0
moneyball_test$M_TEAM_BATTING_SO[is.na(moneyball_test$M_TEAM_BATTING_SO)]=1
str(moneyball_test$M_TEAM_BATTING_SO)
summary(moneyball_test$M_TEAM_BATTING_SO)

#missing value TEAM_PITCHING_SO
skewness(moneyball_test$TEAM_PITCHING_SO,na.exclude(moneyball_test$TEAM_PITCHING_SO))
par(mfrow=c(1,2))
hist(moneyball_test$TEAM_PITCHING_SO,col='blue',main='Team Pitching SO Histogram',xlab='Team Pitching SO')
boxplot(moneyball_test$TEAM_PITCHING_SO,col='red',main='Team Pitching SO Boxplot')
#replace missing values with median b/c so many outliers in x-var
median_TEAM_PITCHING_SO=median(moneyball_test$TEAM_PITCHING_SO,na.exclude(moneyball_test$TEAM_PITCHING_SO))
median_TEAM_PITCHING_SO
moneyball_test$IMP_TEAM_PITCHING_SO=moneyball_test$TEAM_PITCHING_SO
moneyball_test$IMP_TEAM_PITCHING_SO[is.na(moneyball_test$IMP_TEAM_PITCHING_SO)]=median_TEAM_PITCHING_SO
str(moneyball_test$IMP_TEAM_PITCHING_SO)
summary(moneyball_test$IMP_TEAM_PITCHING_SO)
moneyball_test$M_TEAM_PITCHING_SO=moneyball_test$TEAM_PITCHING_SO
moneyball_test$M_TEAM_PITCHING_SO[moneyball_test$M_TEAM_PITCHING_SO>=0]=0
moneyball_test$M_TEAM_PITCHING_SO[is.na(moneyball_test$M_TEAM_PITCHING_SO)]=1
str(moneyball_test$M_TEAM_PITCHING_SO)
summary(moneyball_test$M_TEAM_PITCHING_SO)

#outlier issues
skewness(moneyball_test[2:7])
skewness(moneyball_test[12:14])
skewness(moneyball_test[16:16])
skewness(moneyball_test[18:18])
skewness(moneyball_test[20:20])
skewness(moneyball_test[22:22])
skewness(moneyball_test[24:24])
skewness(moneyball_test[26:26])
#small outlier issues: TEAM_BATTING_H, TEAM_BATTING_3B, TEAM_BATTING_BB
#moderate outlier issues: TEAM_FIELDING_E, IMP_TEAM_BASERUN_CS, IMP_TEAM_BASERUN_SB
#serious outlier issues:TEAM_PITCHING_H, TEAM_PITCHING_BB, IMP_TEAM_PITCHING_SO

#outlier TEAM_BATTING_H
moneyball_test$TRIM_TEAM_BATTING_H=moneyball_test$TEAM_BATTING_H
per99_TEAM_BATTING_H=quantile(moneyball_test$TRIM_TEAM_BATTING_H,0.99)
per99_TEAM_BATTING_H
par(mfrow=c(1,1))
plot(moneyball_test$TRIM_TEAM_BATTING_H,col='blue',main='Team Batting H at 99% percentile',ylab='Team Batting H')
abline(h=per99_TEAM_BATTING_H,col='red')
moneyball_test$TRIM_TEAM_BATTING_H[moneyball_test$TRIM_TEAM_BATTING_H>per99_TEAM_BATTING_H]=per99_TEAM_BATTING_H
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_BATTING_H,col='blue',main='Team Batting H original')
boxplot(moneyball_test$TRIM_TEAM_BATTING_H,col='red',main='Team Batting H trimmed')
skewness(moneyball_test$TEAM_BATTING_H)
skewness(moneyball_test$TRIM_TEAM_BATTING_H)

#outlier TEAM_BATTING_3B
moneyball_test$TRIM_TEAM_BATTING_3B=moneyball_test$TEAM_BATTING_3B
per99_TEAM_BATTING_3B=quantile(moneyball_test$TEAM_BATTING_3B,0.99)
per99_TEAM_BATTING_3B
par(mfrow=c(1,1))
plot(moneyball_test$TRIM_TEAM_BATTING_3B,col='blue',main='Team Batting 3B at 99% percentile',ylab='Team Batting 3B')
abline(h=per99_TEAM_BATTING_3B,col='red')
moneyball_test$TRIM_TEAM_BATTING_3B[moneyball_test$TRIM_TEAM_BATTING_3B>per99_TEAM_BATTING_3B]=per99_TEAM_BATTING_3B
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_BATTING_3B,col='blue',main='Team Batting 3B original')
boxplot(moneyball_test$TRIM_TEAM_BATTING_3B,col='red',main='Team Batting 3B trimmed')
skewness(moneyball_test$TEAM_BATTING_3B)
skewness(moneyball_test$TRIM_TEAM_BATTING_3B)

#outlier TEAM_BATTING_BB
moneyball_test$TRIM_TEAM_BATTING_BB=moneyball_test$TEAM_BATTING_BB
per99upper_TEAM_BATTING_BB=quantile(moneyball_test$TEAM_BATTING_BB,0.99)
per99upper_TEAM_BATTING_BB
per99lower_TEAM_BATTING_BB=quantile(moneyball_test$TEAM_BATTING_BB,0.01)
per99lower_TEAM_BATTING_BB
par(mfrow=c(1,1))
plot(moneyball_test$TRIM_TEAM_BATTING_BB,col='blue',main='Team Batting BB at 99% percentile',ylab='Team Batting BB')
abline(h=per99upper_TEAM_BATTING_BB,col='red')
abline(h=per99lower_TEAM_BATTING_BB,col='red')
moneyball_test$TRIM_TEAM_BATTING_BB[moneyball_test$TRIM_TEAM_BATTING_BB>per99upper_TEAM_BATTING_BB]=per99upper_TEAM_BATTING_BB
moneyball_test$TRIM_TEAM_BATTING_BB[moneyball_test$TRIM_TEAM_BATTING_BB<per99lower_TEAM_BATTING_BB]=per99lower_TEAM_BATTING_BB
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_BATTING_BB,col='blue',main='Team Batting BB original')
boxplot(moneyball_test$TRIM_TEAM_BATTING_BB,col='red',main='Team Batting BB trimmed')
skewness(moneyball_test$TEAM_BATTING_BB)
skewness(moneyball_test$TRIM_TEAM_BATTING_BB)
#the 99% percentile trim still doesn't fix the problem, so will try another outlier technique: z-standardization
sd3upper_TEAM_BATTING_BB=mean(moneyball_test$TEAM_BATTING_BB)+3*sd(moneyball_test$TEAM_BATTING_BB)
sd3upper_TEAM_BATTING_BB
sd3lower_TEAM_BATTING_BB=mean(moneyball_test$TEAM_BATTING_BB)-3*sd(moneyball_test$TEAM_BATTING_BB)
sd3lower_TEAM_BATTING_BB
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_TEAM_BATTING_BB,col='blue',main='Team Batting BB at 3 standard deviations')
abline(h=sd3lower_TEAM_BATTING_BB,col='red')
abline(h=sd3upper_TEAM_BATTING_BB,col='red')
moneyball_test$TRIM_TEAM_BATTING_BB[moneyball_test$TRIM_TEAM_BATTING_BB>sd3upper_TEAM_BATTING_BB]=sd3upper_TEAM_BATTING_BB
moneyball_test$TRIM_TEAM_BATTING_BB[moneyball_test$TRIM_TEAM_BATTING_BB<sd3lower_TEAM_BATTING_BB]=sd3lower_TEAM_BATTING_BB
summary(moneyball_test$TRIM_TEAM_BATTING_BB)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_BATTING_BB,col='blue',main='Team Batting BB original')
boxplot(moneyball_test$TRIM_TEAM_BATTING_BB,col='red',main='Team Batting BB trimmed')
skewness(moneyball_test$TEAM_BATTING_BB)
skewness(moneyball_test$TRIM_TEAM_BATTING_BB)

#outlier TEAM_FIELDING_E
moneyball_test$TRIM_TEAM_FIELDING_E=moneyball_test$TEAM_FIELDING_E
sd3upper_TEAM_FIELDING_E=mean(moneyball_test$TEAM_FIELDING_E)+3*sd(moneyball_test$TEAM_FIELDING_E)
sd3upper_TEAM_FIELDING_E
sd3lower_TEAM_FIELDING_E=mean(moneyball_test$TEAM_FIELDING_E)-3*sd(moneyball_test$TEAM_FIELDING_E)
sd3lower_TEAM_FIELDING_E
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_TEAM_FIELDING_E,col='blue',main='Team Fielding E at 3 standard deviations')
abline(h=sd3lower_TEAM_FIELDING_E,col='red')
abline(h=sd3upper_TEAM_FIELDING_E,col='red')
moneyball_test$TRIM_TEAM_FIELDING_E[moneyball_test$TRIM_TEAM_FIELDING_E>sd3upper_TEAM_FIELDING_E]=sd3upper_TEAM_FIELDING_E
moneyball_test$TRIM_TEAM_FIELDING_E[moneyball_test$TRIM_TEAM_FIELDING_E<sd3lower_TEAM_FIELDING_E]=sd3lower_TEAM_FIELDING_E
summary(moneyball_test$TRIM_TEAM_FIELDING_E)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_FIELDING_E,col='blue',main='Team Fielding E original')
boxplot(moneyball_test$TRIM_TEAM_FIELDING_E,col='red',main='Team Fielding E trimmed')
skewness(moneyball_test$TEAM_FIELDING_E)
skewness(moneyball_test$TRIM_TEAM_FIELDING_E)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_TEAM_FIELDING_E=quantile(moneyball_test$TRIM_TEAM_FIELDING_E,0.95)
per95upper_TEAM_FIELDING_E
per95lower_TEAM_FIELDING_E=quantile(moneyball_test$TRIM_TEAM_FIELDING_E,0.05)
per95lower_TEAM_FIELDING_E
moneyball_test$TRIM_TEAM_FIELDING_E[moneyball_test$TRIM_TEAM_FIELDING_E<per95lower_TEAM_FIELDING_E]=per95lower_TEAM_FIELDING_E
moneyball_test$TRIM_TEAM_FIELDING_E[moneyball_test$TRIM_TEAM_FIELDING_E>per95upper_TEAM_FIELDING_E]=per95upper_TEAM_FIELDING_E
summary(moneyball_test$TRIM_TEAM_FIELDING_E)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_FIELDING_E,col='blue',main='Team Fielding E original')
boxplot(moneyball_test$TRIM_TEAM_FIELDING_E,col='red',main='Team Fielding E trimmed')
skewness(moneyball_test$TEAM_FIELDING_E)
skewness(moneyball_test$TRIM_TEAM_FIELDING_E)

#outlier IMP_TEAM_BASERUN_CS
moneyball_test$TRIM_IMP_TEAM_BASERUN_CS=moneyball_test$IMP_TEAM_BASERUN_CS
sd3upper_IMP_TEAM_BASERUN_CS=mean(moneyball_test$IMP_TEAM_BASERUN_CS)+3*sd(moneyball_test$IMP_TEAM_BASERUN_CS)
sd3upper_IMP_TEAM_BASERUN_CS
sd3lower_IMP_TEAM_BASERUN_CS=mean(moneyball_test$IMP_TEAM_BASERUN_CS)-3*sd(moneyball_test$IMP_TEAM_BASERUN_CS)
sd3lower_IMP_TEAM_BASERUN_CS
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS,col='blue',main='IMP Team Baserun CS at 3 standard deviations')
abline(h=sd3lower_IMP_TEAM_BASERUN_CS,col='red')
abline(h=sd3upper_IMP_TEAM_BASERUN_CS,col='red')
moneyball_test$TRIM_IMP_TEAM_BASERUN_CS[moneyball_test$TRIM_IMP_TEAM_BASERUN_CS>sd3upper_IMP_TEAM_BASERUN_CS]=sd3upper_IMP_TEAM_BASERUN_CS
moneyball_test$TRIM_IMP_TEAM_BASERUN_CS[moneyball_test$TRIM_IMP_TEAM_BASERUN_CS<sd3lower_IMP_TEAM_BASERUN_CS]=sd3lower_IMP_TEAM_BASERUN_CS
summary(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS)
par(mfrow=c(1,2))
boxplot(moneyball_test$IMP_TEAM_BASERUN_CS,col='blue',main='IMP Team Baserun CS original')
boxplot(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS,col='red',main='IMP Team Baserun CS trimmed')
skewness(moneyball_test$IMP_TEAM_BASERUN_CS)
skewness(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_IMP_TEAM_BASERUN_CS=quantile(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS,0.95)
per95upper_IMP_TEAM_BASERUN_CS
per95lower_IMP_TEAM_BASERUN_CS=quantile(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS,0.05)
per95lower_IMP_TEAM_BASERUN_CS
moneyball_test$TRIM_IMP_TEAM_BASERUN_CS[moneyball_test$TRIM_IMP_TEAM_BASERUN_CS<per95lower_IMP_TEAM_BASERUN_CS]=per95lower_IMP_TEAM_BASERUN_CS
moneyball_test$TRIM_IMP_TEAM_BASERUN_CS[moneyball_test$TRIM_IMP_TEAM_BASERUN_CS>per95upper_IMP_TEAM_BASERUN_CS]=per95upper_IMP_TEAM_BASERUN_CS
summary(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS)
par(mfrow=c(1,2))
boxplot(moneyball_test$IMP_TEAM_BASERUN_CS,col='blue',main='IMP Team Baserun CS original')
boxplot(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS,col='red',main='IMP Team Baserun CS trimmed')
skewness(moneyball_test$IMP_TEAM_BASERUN_CS)
skewness(moneyball_test$TRIM_IMP_TEAM_BASERUN_CS)

#outlier IMP_TEAM_BASERUN_SB
moneyball_test$TRIM_IMP_TEAM_BASERUN_SB=moneyball_test$IMP_TEAM_BASERUN_SB
sd3upper_IMP_TEAM_BASERUN_SB=mean(moneyball_test$IMP_TEAM_BASERUN_SB)+3*sd(moneyball_test$IMP_TEAM_BASERUN_SB)
sd3upper_IMP_TEAM_BASERUN_SB
sd3lower_IMP_TEAM_BASERUN_SB=mean(moneyball_test$IMP_TEAM_BASERUN_SB)-3*sd(moneyball_test$IMP_TEAM_BASERUN_SB)
sd3lower_IMP_TEAM_BASERUN_SB
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB,col='blue',main='IMP Team Baserun SB at 3 standard deviations')
abline(h=sd3lower_IMP_TEAM_BASERUN_SB,col='red')
abline(h=sd3upper_IMP_TEAM_BASERUN_SB,col='red')
moneyball_test$TRIM_IMP_TEAM_BASERUN_SB[moneyball_test$TRIM_IMP_TEAM_BASERUN_SB>sd3upper_IMP_TEAM_BASERUN_SB]=sd3upper_IMP_TEAM_BASERUN_SB
moneyball_test$TRIM_IMP_TEAM_BASERUN_SB[moneyball_test$TRIM_IMP_TEAM_BASERUN_SB<sd3lower_IMP_TEAM_BASERUN_SB]=sd3lower_IMP_TEAM_BASERUN_SB
summary(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB)
par(mfrow=c(1,2))
boxplot(moneyball_test$IMP_TEAM_BASERUN_SB,col='blue',main='IMP Team Baserun SB original')
boxplot(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB,col='red',main='IMP Team Baserun SB trimmed')
skewness(moneyball_test$IMP_TEAM_BASERUN_SB)
skewness(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_IMP_TEAM_BASERUN_SB=quantile(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB,0.95)
per95upper_IMP_TEAM_BASERUN_SB
per95lower_IMP_TEAM_BASERUN_SB=quantile(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB,0.05)
per95lower_IMP_TEAM_BASERUN_SB
moneyball_test$TRIM_IMP_TEAM_BASERUN_SB[moneyball_test$TRIM_IMP_TEAM_BASERUN_SB<per95lower_IMP_TEAM_BASERUN_SB]=per95lower_IMP_TEAM_BASERUN_SB
moneyball_test$TRIM_IMP_TEAM_BASERUN_SB[moneyball_test$TRIM_IMP_TEAM_BASERUN_SB>per95upper_IMP_TEAM_BASERUN_SB]=per95upper_IMP_TEAM_BASERUN_SB
summary(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB)
par(mfrow=c(1,2))
boxplot(moneyball_test$IMP_TEAM_BASERUN_SB,col='blue',main='IMP Team Baserun SB original')
boxplot(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB,col='red',main='IMP Team Baserun SB trimmed')
skewness(moneyball_test$IMP_TEAM_BASERUN_SB)
skewness(moneyball_test$TRIM_IMP_TEAM_BASERUN_SB)

#outlier TEAM_PITCHING_H
moneyball_test$TRIM_TEAM_PITCHING_H=moneyball_test$TEAM_PITCHING_H
sd3upper_TEAM_PITCHING_H=mean(moneyball_test$TEAM_PITCHING_H)+3*sd(moneyball_test$TEAM_PITCHING_H)
sd3upper_TEAM_PITCHING_H
sd3lower_TEAM_PITCHING_H=mean(moneyball_test$TEAM_PITCHING_H)-3*sd(moneyball_test$TEAM_PITCHING_H)
sd3lower_TEAM_PITCHING_H
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_TEAM_PITCHING_H,col='blue',main='Team Pitching H at 3 standard deviations')
abline(h=sd3lower_TEAM_PITCHING_H,col='red')
abline(h=sd3upper_TEAM_PITCHING_H,col='red')
moneyball_test$TRIM_TEAM_PITCHING_H[moneyball_test$TRIM_TEAM_PITCHING_H>sd3upper_TEAM_PITCHING_H]=sd3upper_TEAM_PITCHING_H
moneyball_test$TRIM_TEAM_PITCHING_H[moneyball_test$TRIM_TEAM_PITCHING_H<sd3lower_TEAM_PITCHING_H]=sd3lower_TEAM_PITCHING_H
summary(moneyball_test$TRIM_TEAM_PITCHING_H)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_PITCHING_H,col='blue',main='Team Pitching H original')
boxplot(moneyball_test$TRIM_TEAM_PITCHING_H,col='red',main='Team Pitching H trimmed')
skewness(moneyball_test$TEAM_PITCHING_H)
skewness(moneyball_test$TRIM_TEAM_PITCHING_H)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_TEAM_PITCHING_H=quantile(moneyball_test$TRIM_TEAM_PITCHING_H,0.95)
per95upper_TEAM_PITCHING_H
per95lower_TEAM_PITCHING_H=quantile(moneyball_test$TRIM_TEAM_PITCHING_H,0.05)
per95lower_TEAM_PITCHING_H
moneyball_test$TRIM_TEAM_PITCHING_H[moneyball_test$TRIM_TEAM_PITCHING_H<per95lower_TEAM_PITCHING_H]=per95lower_TEAM_PITCHING_H
moneyball_test$TRIM_TEAM_PITCHING_H[moneyball_test$TRIM_TEAM_PITCHING_H>per95upper_TEAM_PITCHING_H]=per95upper_TEAM_PITCHING_H
summary(moneyball_test$TRIM_TEAM_PITCHING_H)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_PITCHING_H,col='blue',main='Team Pitching H original')
boxplot(moneyball_test$TRIM_TEAM_PITCHING_H,col='red',main='Team Pitching H trimmed')
skewness(moneyball_test$TEAM_PITCHING_H)
skewness(moneyball_test$TRIM_TEAM_PITCHING_H)

#outlier TEAM_PITCHING_BB
moneyball_test$TRIM_TEAM_PITCHING_BB=moneyball_test$TEAM_PITCHING_BB
sd3upper_TEAM_PITCHING_BB=mean(moneyball_test$TEAM_PITCHING_BB)+3*sd(moneyball_test$TEAM_PITCHING_BB)
sd3upper_TEAM_PITCHING_BB
sd3lower_TEAM_PITCHING_BB=mean(moneyball_test$TEAM_PITCHING_BB)-3*sd(moneyball_test$TEAM_PITCHING_BB)
sd3lower_TEAM_PITCHING_BB
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_TEAM_PITCHING_BB,col='blue',main='Team Pitching BB at 3 standard deviations')
abline(h=sd3lower_TEAM_PITCHING_BB,col='red')
abline(h=sd3upper_TEAM_PITCHING_BB,col='red')
moneyball_test$TRIM_TEAM_PITCHING_BB[moneyball_test$TRIM_TEAM_PITCHING_BB>sd3upper_TEAM_PITCHING_BB]=sd3upper_TEAM_PITCHING_BB
moneyball_test$TRIM_TEAM_PITCHING_BB[moneyball_test$TRIM_TEAM_PITCHING_BB<sd3lower_TEAM_PITCHING_BB]=sd3lower_TEAM_PITCHING_BB
summary(moneyball_test$TRIM_TEAM_PITCHING_BB)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_PITCHING_BB,col='blue',main='Team Pitching BB original')
boxplot(moneyball_test$TRIM_TEAM_PITCHING_BB,col='red',main='Team Pitching BB trimmed')
skewness(moneyball_test$TEAM_PITCHING_BB)
skewness(moneyball_test$TRIM_TEAM_PITCHING_BB)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_TEAM_PITCHING_BB=quantile(moneyball_test$TRIM_TEAM_PITCHING_BB,0.95)
per95upper_TEAM_PITCHING_BB
per95lower_TEAM_PITCHING_BB=quantile(moneyball_test$TRIM_TEAM_PITCHING_BB,0.05)
per95lower_TEAM_PITCHING_BB
moneyball_test$TRIM_TEAM_PITCHING_BB[moneyball_test$TRIM_TEAM_PITCHING_BB<per95lower_TEAM_PITCHING_BB]=per95lower_TEAM_PITCHING_BB
moneyball_test$TRIM_TEAM_PITCHING_BB[moneyball_test$TRIM_TEAM_PITCHING_BB>per95upper_TEAM_PITCHING_BB]=per95upper_TEAM_PITCHING_BB
summary(moneyball_test$TRIM_TEAM_PITCHING_BB)
par(mfrow=c(1,2))
boxplot(moneyball_test$TEAM_PITCHING_BB,col='blue',main='Team Pitching BB original')
boxplot(moneyball_test$TRIM_TEAM_PITCHING_BB,col='red',main='Team Pitching BB trimmed')
skewness(moneyball_test$TEAM_PITCHING_BB)
skewness(moneyball_test$TRIM_TEAM_PITCHING_BB)

#outlier IMP_TEAM_PITCHING_SO
moneyball_test$TRIM_IMP_TEAM_PITCHING_SO=moneyball_test$IMP_TEAM_PITCHING_SO
sd3upper_IMP_TEAM_PITCHING_SO=mean(moneyball_test$IMP_TEAM_PITCHING_SO)+3*sd(moneyball_test$IMP_TEAM_PITCHING_SO)
sd3upper_IMP_TEAM_PITCHING_SO
sd3lower_IMP_TEAM_PITCHING_SO=mean(moneyball_test$IMP_TEAM_PITCHING_SO)-3*sd(moneyball_test$IMP_TEAM_PITCHING_SO)
sd3lower_IMP_TEAM_PITCHING_SO
par(mfrow=c(1,1))
boxplot(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO,col='blue',main='IMP Team Pitching SO at 3 standard deviations')
abline(h=sd3lower_IMP_TEAM_PITCHING_SO,col='red')
abline(h=sd3upper_IMP_TEAM_PITCHING_SO,col='red')
moneyball_test$TRIM_IMP_TEAM_PITCHING_SO[moneyball_test$TRIM_IMP_TEAM_PITCHING_SO>sd3upper_IMP_TEAM_PITCHING_SO]=sd3upper_IMP_TEAM_PITCHING_SO
moneyball_test$TRIM_IMP_TEAM_PITCHING_SO[moneyball_test$TRIM_IMP_TEAM_PITCHING_SO<sd3lower_IMP_TEAM_PITCHING_SO]=sd3lower_IMP_TEAM_PITCHING_SO
summary(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO)
par(mfrow=c(1,2))
boxplot(moneyball_test$IMP_TEAM_PITCHING_SO,col='blue',main='IMP Team Pitching SO original')
boxplot(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO,col='red',main='IMP Team Pitching SO trimmed')
skewness(moneyball_test$IMP_TEAM_PITCHING_SO)
skewness(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO)
#use 95% percentile trim after z-standardization to get even better result 
per95upper_IMP_TEAM_PITCHING_SO=quantile(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO,0.95)
per95upper_IMP_TEAM_PITCHING_SO
per95lower_IMP_TEAM_PITCHING_SO=quantile(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO,0.05)
per95lower_IMP_TEAM_PITCHING_SO
moneyball_test$TRIM_IMP_TEAM_PITCHING_SO[moneyball_test$TRIM_IMP_TEAM_PITCHING_SO<per95lower_IMP_TEAM_PITCHING_SO]=per95lower_IMP_TEAM_PITCHING_SO
moneyball_test$TRIM_IMP_TEAM_PITCHING_SO[moneyball_test$TRIM_IMP_TEAM_PITCHING_SO>per95upper_IMP_TEAM_PITCHING_SO]=per95upper_IMP_TEAM_PITCHING_SO
summary(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO)
par(mfrow=c(1,2))
boxplot(moneyball_test$IMP_TEAM_PITCHING_SO,col='blue',main='IMP Team Pitching SO original')
boxplot(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO,col='red',main='IMP Team Pitching SO trimmed')
skewness(moneyball_test$IMP_TEAM_PITCHING_SO)
skewness(moneyball_test$TRIM_IMP_TEAM_PITCHING_SO)

str(moneyball_test)
summary(moneyball_test)

#model 1, 2, 3
moneyball_test$predict1=34.898297+
  moneyball_test$TEAM_PITCHING_HR*0.067848+
  moneyball_test$M_TEAM_FIELDING_DP*6.925695 +
  moneyball_test$M_TEAM_BASERUN_SB*41.843653+
  moneyball_test$M_TEAM_BATTING_SO*6.690207+
  moneyball_test$IMP_TEAM_FIELDING_DP*-0.100069+
  moneyball_test$IMP_TEAM_BATTING_SO*-0.027077+
  moneyball_test$TRIM_TEAM_BATTING_H*0.049592+
  moneyball_test$TRIM_TEAM_BATTING_3B*0.127181+
  moneyball_test$TRIM_TEAM_BATTING_BB*0.026716+
  moneyball_test$TRIM_TEAM_FIELDING_E*-0.074771+
  moneyball_test$TRIM_IMP_TEAM_BASERUN_CS*-0.098158+
  moneyball_test$TRIM_IMP_TEAM_BASERUN_SB*0.080052+
  moneyball_test$TRIM_TEAM_PITCHING_H*-0.013200+
  moneyball_test$TRIM_IMP_TEAM_PITCHING_SO*0.013352 
summary(moneyball_test$predict1)
#prediction stays within the 30-120 assumption range

#model 4
moneyball_test$predict4=8.333419 +    
  moneyball_test$TEAM_PITCHING_HR*0.091864+
  moneyball_test$M_TEAM_FIELDING_DP*-6.858533+
  moneyball_test$M_TEAM_BASERUN_SB*28.043702+
  moneyball_test$M_TEAM_BATTING_SO*6.055024+
  moneyball_test$IMP_TEAM_FIELDING_DP*-0.071992+
  moneyball_test$TRIM_TEAM_BATTING_H*0.051148+
  moneyball_test$TRIM_TEAM_BATTING_3B*0.089867+
  moneyball_test$TRIM_TEAM_BATTING_BB*0.034880+
  moneyball_test$TRIM_IMP_TEAM_BASERUN_SB*0.054623+
  moneyball_test$TRIM_TEAM_PITCHING_H*-0.014893+
  moneyball_test$TRIM_IMP_TEAM_PITCHING_SO*-0.009604 
summary(moneyball_test$predict4)
#prediction stays within the 30-120 assumption range

#model 5
moneyball_test$predict5=-20.554988+
  moneyball_test$M_TEAM_FIELDING_DP*-8.408647+
  moneyball_test$M_TEAM_BASERUN_SB*25.415368+
  moneyball_test$M_TEAM_BATTING_SO*5.094446+
  moneyball_test$TRIM_TEAM_BATTING_H*0.063673+
  moneyball_test$TRIM_TEAM_BATTING_3B*0.025631+  
  moneyball_test$TRIM_TEAM_BATTING_BB*0.043445+
  moneyball_test$TRIM_IMP_TEAM_BASERUN_SB*0.048417+
  moneyball_test$TRIM_TEAM_PITCHING_H*-0.013416 
summary(moneyball_test$predict5)
#prediction stays within the 30-120 assumption range

#comparison
meancomp=c(round(mean(moneyball_test$predict1),2),round(mean(moneyball_test$predict4),2),round(mean(moneyball_test$predict5),2))
tablecomp=cbind(tablecomp,meancomp)
tablecomp
round(mean(moneyball$TARGET_WINS),2)

#part 6: stand alone scoring

moneyball_test$P_TARGET_WINS=-20.554988+
  moneyball_test$M_TEAM_FIELDING_DP*-8.408647+
  moneyball_test$M_TEAM_BASERUN_SB*25.415368+
  moneyball_test$M_TEAM_BATTING_SO*5.094446+
  moneyball_test$TRIM_TEAM_BATTING_H*0.063673+
  moneyball_test$TRIM_TEAM_BATTING_3B*0.025631+  
  moneyball_test$TRIM_TEAM_BATTING_BB*0.043445+
  moneyball_test$TRIM_IMP_TEAM_BASERUN_SB*0.048417+
  moneyball_test$TRIM_TEAM_PITCHING_H*-0.013416 
prediction=moneyball_test[c('INDEX','P_TARGET_WINS')]
write.csv(prediction,file='MimiTrinh_write.csv',col.names=TRUE)