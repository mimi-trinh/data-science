# Final Project by Mimi Trinh
# drivendata.org username: goldenfaith
# drivendata.org password: binhtuongson
library(forecast)
library(ggplot2)
library(fpp2)
library(car)
library(MASS)
library(seasonal)
library(lubridate)
library(rdatamarket)
library(hts)
library(readr)
library(tidyverse)
library(vars)
library(corrplot)
set.seed(123)
setwd('/Users/mimitrinh/Desktop')
predictors=read.csv('dengue_features_train.csv',header=T)
target=read.csv('dengue_labels_train.csv',header=T)
str(predictors)
str(target)
data=data.frame(predictors,target$total_cases)
str(data)
colnames(data)[colnames(data)=='target.total_cases']='total_cases'
str(data)
str(data$city)
iqtrain=subset(data,city=='iq')
sjtrain=subset(data,city=='sj')
str(iqtrain)
str(sjtrain)
summary(iqtrain) # have missing values but no column should be dropped with many missing values
summary(sjtrain) # same as above

par(mfrow=c(1,2))
hist(iqtrain$total_cases,col='red',main='Iquitos Total Cases',xlab='Total Cases')
hist(sjtrain$total_cases,col='blue',main='San Juan Total Cases',xlab='Total Cases')
summary(iqtrain$total_cases) # very skewed since mean <> median, confirmed by histogram
summary(sjtrain$total_cases) # same as above 
length(iqtrain$total_cases[iqtrain$total_cases==0]) 
# 96 out of 520 with 0 value in target variable, which is 18%
length(sjtrain$total_cases[iqtrain$total_cases==0]) 
# 184 out of 936 with 0 value in target variable, which is 20%
head(iqtrain)
tail(iqtrain)
head(sjtrain)
tail(sjtrain)
summary(iqtrain)
summary(sjtrain)
par(mfrow=c(1,1))
iqcor1=cor(na.omit(iqtrain[5:15]))
corrplot(iqcor1,type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
iqcor2=cor(na.omit(iqtrain[16:25]))
corrplot(iqcor2,type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
sjcor1=cor(na.omit(sjtrain[5:15]))
corrplot(sjcor1,type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
sjcor2=cor(na.omit(sjtrain[16:25]))
corrplot(sjcor2,type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# all predictors have missing values
# b/c data is skewed, replace missing values with medians of each column
iqtrain$ndvi_ne[is.na(iqtrain$ndvi_ne)]=median(iqtrain$ndvi_ne,na.rm=TRUE)
iqtrain$ndvi_nw[is.na(iqtrain$ndvi_nw)]=median(iqtrain$ndvi_nw,na.rm=TRUE)
iqtrain$ndvi_se[is.na(iqtrain$ndvi_se)]=median(iqtrain$ndvi_se,na.rm=TRUE)
iqtrain$ndvi_sw[is.na(iqtrain$ndvi_sw)]=median(iqtrain$ndvi_sw,na.rm=TRUE)
iqtrain$precipitation_amt_mm[is.na(iqtrain$precipitation_amt_mm)]=median(iqtrain$precipitation_amt_mm,na.rm=TRUE)
iqtrain$reanalysis_air_temp_k[is.na(iqtrain$reanalysis_air_temp_k)]=median(iqtrain$reanalysis_air_temp_k,na.rm=TRUE)
iqtrain$reanalysis_avg_temp_k[is.na(iqtrain$reanalysis_avg_temp_k)]=median(iqtrain$reanalysis_avg_temp_k,na.rm=TRUE)
iqtrain$reanalysis_dew_point_temp_k[is.na(iqtrain$reanalysis_dew_point_temp_k)]=median(iqtrain$reanalysis_dew_point_temp_k,na.rm=TRUE)
iqtrain$reanalysis_max_air_temp_k[is.na(iqtrain$reanalysis_max_air_temp_k)]=median(iqtrain$reanalysis_max_air_temp_k,na.rm=TRUE)
iqtrain$reanalysis_min_air_temp_k[is.na(iqtrain$reanalysis_min_air_temp_k)]=median(iqtrain$reanalysis_min_air_temp_k,na.rm=TRUE)
iqtrain$reanalysis_precip_amt_kg_per_m2[is.na(iqtrain$reanalysis_precip_amt_kg_per_m2)]=median(iqtrain$reanalysis_precip_amt_kg_per_m2,na.rm=TRUE)
iqtrain$reanalysis_relative_humidity_percent[is.na(iqtrain$reanalysis_relative_humidity_percent)]=median(iqtrain$reanalysis_relative_humidity_percent,na.rm=TRUE)
iqtrain$reanalysis_sat_precip_amt_mm[is.na(iqtrain$reanalysis_sat_precip_amt_mm)]=median(iqtrain$reanalysis_sat_precip_amt_mm,na.rm=TRUE)
iqtrain$reanalysis_specific_humidity_g_per_kg[is.na(iqtrain$reanalysis_specific_humidity_g_per_kg)]=median(iqtrain$reanalysis_specific_humidity_g_per_kg,na.rm=TRUE)
iqtrain$reanalysis_tdtr_k[is.na(iqtrain$reanalysis_tdtr_k)]=median(iqtrain$reanalysis_tdtr_k,na.rm=TRUE)
iqtrain$station_avg_temp_c[is.na(iqtrain$station_avg_temp_c)]=median(iqtrain$station_avg_temp_c,na.rm=TRUE)
iqtrain$station_diur_temp_rng_c[is.na(iqtrain$station_diur_temp_rng_c)]=median(iqtrain$station_diur_temp_rng_c,na.rm=TRUE)
iqtrain$station_max_temp_c[is.na(iqtrain$station_max_temp_c)]=median(iqtrain$station_max_temp_c,na.rm=TRUE)
iqtrain$station_min_temp_c[is.na(iqtrain$station_min_temp_c)]=median(iqtrain$station_min_temp_c,na.rm=TRUE)
iqtrain$station_precip_mm[is.na(iqtrain$station_precip_mm)]=median(iqtrain$station_precip_mm,na.rm=TRUE)
summary(iqtrain)
sjtrain$ndvi_ne[is.na(sjtrain$ndvi_ne)]=median(sjtrain$ndvi_ne,na.rm=TRUE)
sjtrain$ndvi_nw[is.na(sjtrain$ndvi_nw)]=median(sjtrain$ndvi_nw,na.rm=TRUE)
sjtrain$ndvi_se[is.na(sjtrain$ndvi_se)]=median(sjtrain$ndvi_se,na.rm=TRUE)
sjtrain$ndvi_sw[is.na(sjtrain$ndvi_sw)]=median(sjtrain$ndvi_sw,na.rm=TRUE)
sjtrain$precipitation_amt_mm[is.na(sjtrain$precipitation_amt_mm)]=median(sjtrain$precipitation_amt_mm,na.rm=TRUE)
sjtrain$reanalysis_air_temp_k[is.na(sjtrain$reanalysis_air_temp_k)]=median(sjtrain$reanalysis_air_temp_k,na.rm=TRUE)
sjtrain$reanalysis_avg_temp_k[is.na(sjtrain$reanalysis_avg_temp_k)]=median(sjtrain$reanalysis_avg_temp_k,na.rm=TRUE)
sjtrain$reanalysis_dew_point_temp_k[is.na(sjtrain$reanalysis_dew_point_temp_k)]=median(sjtrain$reanalysis_dew_point_temp_k,na.rm=TRUE)
sjtrain$reanalysis_max_air_temp_k[is.na(sjtrain$reanalysis_max_air_temp_k)]=median(sjtrain$reanalysis_max_air_temp_k,na.rm=TRUE)
sjtrain$reanalysis_min_air_temp_k[is.na(sjtrain$reanalysis_min_air_temp_k)]=median(sjtrain$reanalysis_min_air_temp_k,na.rm=TRUE)
sjtrain$reanalysis_precip_amt_kg_per_m2[is.na(sjtrain$reanalysis_precip_amt_kg_per_m2)]=median(sjtrain$reanalysis_precip_amt_kg_per_m2,na.rm=TRUE)
sjtrain$reanalysis_relative_humidity_percent[is.na(sjtrain$reanalysis_relative_humidity_percent)]=median(sjtrain$reanalysis_relative_humidity_percent,na.rm=TRUE)
sjtrain$reanalysis_sat_precip_amt_mm[is.na(sjtrain$reanalysis_sat_precip_amt_mm)]=median(sjtrain$reanalysis_sat_precip_amt_mm,na.rm=TRUE)
sjtrain$reanalysis_specific_humidity_g_per_kg[is.na(sjtrain$reanalysis_specific_humidity_g_per_kg)]=median(sjtrain$reanalysis_specific_humidity_g_per_kg,na.rm=TRUE)
sjtrain$reanalysis_tdtr_k[is.na(sjtrain$reanalysis_tdtr_k)]=median(sjtrain$reanalysis_tdtr_k,na.rm=TRUE)
sjtrain$station_avg_temp_c[is.na(sjtrain$station_avg_temp_c)]=median(sjtrain$station_avg_temp_c,na.rm=TRUE)
sjtrain$station_diur_temp_rng_c[is.na(sjtrain$station_diur_temp_rng_c)]=median(sjtrain$station_diur_temp_rng_c,na.rm=TRUE)
sjtrain$station_max_temp_c[is.na(sjtrain$station_max_temp_c)]=median(sjtrain$station_max_temp_c,na.rm=TRUE)
sjtrain$station_min_temp_c[is.na(sjtrain$station_min_temp_c)]=median(sjtrain$station_min_temp_c,na.rm=TRUE)
sjtrain$station_precip_mm[is.na(sjtrain$station_precip_mm)]=median(sjtrain$station_precip_mm,na.rm=TRUE)
summary(sjtrain)

iqtrain=ts(iqtrain[5:25],start=c(2000,07,01),end=c(2010,06,25),frequency=52)
str(iqtrain)
sjtrain=ts(sjtrain[5:25],start=c(1990,04,30),end=c(2008,04,22),frequency=52)
str(sjtrain)
autoplot(iqtrain[,'total_cases']) + ggtitle('Total Cases in Iquitos') + ylab('Total Cases') +
  geom_line(col='red') 
# middle of year 2014 has a big outlier with almost 120 cases in Iquitos
autoplot(sjtrain[,'total_cases']) + ggtitle('Total Cases in San Juan') + ylab('Total Cases') +
  geom_line(col='blue')
# last quarter of year 1995 has a big outlier with more than 400 cases in San Juan

iqfit1=naive(iqtrain[,'total_cases'],h=156)
iqfit2=snaive(iqtrain[,'total_cases'],h=156)
iqfit3=rwf(iqtrain[,'total_cases'],h=156,drift=TRUE)
autoplot(iqfit1) + ylab('Iquitos total cases')
autoplot(iqfit2) + ylab('Iquitos total cases')
autoplot(iqfit3) + ylab('Iquitos total cases')

sjfit1=naive(sjtrain[,'total_cases'],h=260)
sjfit2=snaive(sjtrain[,'total_cases'],h=260)
sjfit3=rwf(sjtrain[,'total_cases'],h=260,drift=TRUE)
autoplot(sjfit1) + ylab('San Juan total cases')
autoplot(sjfit2) + ylab('San Juan total cases')
autoplot(sjfit3) + ylab('San Juan total cases')

# feature engineering
# from corrplots above, don't need all variables b/c some of them are highly correlated
# take out ndvi_nw, ndvi_se, ndvi_sw, 
# take out reanalysis_air_temp_k, reanalysis_dew_point_temp_k, reanalysis_max_air_temp_k,
# reanalysis_min_air_temp_k
# R takes a long time to process remaining variables, so from reading instruction on drivendata.org
# take out more variables similar to others such as
# reanalysis_precip_amt_kg_per_m2, reanalysis_relative_humidity_percent, reanalysis_sat_precip_amt_mm
# station_avg_temp_c, station_diur_temp_rng_c
# station_max_temp_c, station_min_temp_c, station_precip_mm
iqxvar=cbind(iqtrain[,'ndvi_ne'],
             iqtrain[,'precipitation_amt_mm'],
             iqtrain[,'reanalysis_avg_temp_k'],
             iqtrain[,'reanalysis_specific_humidity_g_per_kg'],
             iqtrain[,'reanalysis_tdtr_k'])
sjxvar=cbind(sjtrain[,'ndvi_ne'],
            sjtrain[,'precipitation_amt_mm'],
            sjtrain[,'reanalysis_avg_temp_k'],
            sjtrain[,'reanalysis_specific_humidity_g_per_kg'],
            sjtrain[,'reanalysis_tdtr_k'])
# to use Box Cox transformation with lambda='auto' in model 6, need to have non 0 values 
# for log transformation, so add 1 to each variable, both predictors and response variable
iqx6=cbind(iqtrain[,'ndvi_ne'],
             iqtrain[,'precipitation_amt_mm'],
             iqtrain[,'reanalysis_avg_temp_k'],
             iqtrain[,'reanalysis_specific_humidity_g_per_kg'],
             iqtrain[,'reanalysis_tdtr_k'])
iqy6=iqtrain[,'total_cases']
iqx6=iqx6+1
iqy6=iqy6+1
sjx6=cbind(sjtrain[,'ndvi_ne'],
             sjtrain[,'precipitation_amt_mm'],
             sjtrain[,'reanalysis_avg_temp_k'],
             sjtrain[,'reanalysis_specific_humidity_g_per_kg'],
             sjtrain[,'reanalysis_tdtr_k'])
sjy6=sjtrain[,'total_cases']
sjx6=sjx6+1
sjy6=sjy6+1

iqfit4=auto.arima(iqtrain[,'total_cases'],xreg=iqxvar) # ARIMA(1,0,4)
iqfit4
sjfit4=auto.arima(sjtrain[,'total_cases'],xreg=sjxvar) # ARIMA(1,1,1)
sjfit4

iqfit5=nnetar(iqtrain[,'total_cases'],xreg=iqxvar,lambda=NULL) # NNAR(5,1,6)[52]
iqfit5
sjfit5=nnetar(sjtrain[,'total_cases'],xreg=sjxvar,lambda=NULL) # NNAR(14,1,10)[52]
sjfit5

iqfit6=nnetar(iqy6,xreg=iqx6,lambda='auto') # NNAR(6,1,6)[52]
iqfit6
sjfit6=nnetar(sjy6,xreg=sjx6,lambda='auto') # NNAR(16,1,12)[52] 
sjfit6

# check residuals
# small p-value < 0.05 from the Ljung-Box test shows that the residuals don’t follow white noise
checkresiduals(iqfit1) # not white noise
checkresiduals(iqfit2) # not white noise
checkresiduals(iqfit3) # not white noise
checkresiduals(iqfit4) # follow white noise
checkresiduals(iqfit5) # don't have p-value but from ACF plot, seems to follow white noise
# can't check model 6 b/c ggtsdisplay is only for univariate time series

checkresiduals(sjfit1) # not white noise
checkresiduals(sjfit2) # not white noise
checkresiduals(sjfit3) # not white noise
checkresiduals(sjfit4) # follow white noise
checkresiduals(sjfit5) # don't have p-value but from ACF plot, seems to follow white noise
# can't check model 6 b/c ggtsdisplay is only for univariate time series

accuracy(iqfit1)
accuracy(iqfit2)
accuracy(iqfit3)
accuracy(iqfit4)
accuracy(iqfit5)
accuracy(iqfit6)
# models 4, 5, 6 perform much better than models 1, 2, 3 => predictors are helpful
# model 5 is the best using default neural networks with no Box Cox transformation

accuracy(sjfit1)
accuracy(sjfit2)
accuracy(sjfit3)
accuracy(sjfit4)
accuracy(sjfit5)
accuracy(sjfit6)
# similar to IQ, model 5 is the best using default neural networks with no Box Cox transformation

# make predictions for models 4, 5, 6
test=read.csv('dengue_features_test.csv',header=T)
str(test)
iqtest=subset(test,city=='iq')
sjtest=subset(test,city=='sj')
str(iqtest)
str(sjtest)
summary(iqtest) # there are missing values
summary(sjtest) # there are missing values
head(iqtest)
tail(iqtest)
head(sjtest)
tail(sjtest)

# clean test data using median to replace missing values, just like with train data
iqtest$ndvi_ne[is.na(iqtest$ndvi_ne)]=median(iqtest$ndvi_ne,na.rm=TRUE)
iqtest$ndvi_nw[is.na(iqtest$ndvi_nw)]=median(iqtest$ndvi_nw,na.rm=TRUE)
iqtest$ndvi_se[is.na(iqtest$ndvi_se)]=median(iqtest$ndvi_se,na.rm=TRUE)
iqtest$ndvi_sw[is.na(iqtest$ndvi_sw)]=median(iqtest$ndvi_sw,na.rm=TRUE)
iqtest$precipitation_amt_mm[is.na(iqtest$precipitation_amt_mm)]=median(iqtest$precipitation_amt_mm,na.rm=TRUE)
iqtest$reanalysis_air_temp_k[is.na(iqtest$reanalysis_air_temp_k)]=median(iqtest$reanalysis_air_temp_k,na.rm=TRUE)
iqtest$reanalysis_avg_temp_k[is.na(iqtest$reanalysis_avg_temp_k)]=median(iqtest$reanalysis_avg_temp_k,na.rm=TRUE)
iqtest$reanalysis_dew_point_temp_k[is.na(iqtest$reanalysis_dew_point_temp_k)]=median(iqtest$reanalysis_dew_point_temp_k,na.rm=TRUE)
iqtest$reanalysis_max_air_temp_k[is.na(iqtest$reanalysis_max_air_temp_k)]=median(iqtest$reanalysis_max_air_temp_k,na.rm=TRUE)
iqtest$reanalysis_min_air_temp_k[is.na(iqtest$reanalysis_min_air_temp_k)]=median(iqtest$reanalysis_min_air_temp_k,na.rm=TRUE)
iqtest$reanalysis_precip_amt_kg_per_m2[is.na(iqtest$reanalysis_precip_amt_kg_per_m2)]=median(iqtest$reanalysis_precip_amt_kg_per_m2,na.rm=TRUE)
iqtest$reanalysis_relative_humidity_percent[is.na(iqtest$reanalysis_relative_humidity_percent)]=median(iqtest$reanalysis_relative_humidity_percent,na.rm=TRUE)
iqtest$reanalysis_sat_precip_amt_mm[is.na(iqtest$reanalysis_sat_precip_amt_mm)]=median(iqtest$reanalysis_sat_precip_amt_mm,na.rm=TRUE)
iqtest$reanalysis_specific_humidity_g_per_kg[is.na(iqtest$reanalysis_specific_humidity_g_per_kg)]=median(iqtest$reanalysis_specific_humidity_g_per_kg,na.rm=TRUE)
iqtest$reanalysis_tdtr_k[is.na(iqtest$reanalysis_tdtr_k)]=median(iqtest$reanalysis_tdtr_k,na.rm=TRUE)
iqtest$station_avg_temp_c[is.na(iqtest$station_avg_temp_c)]=median(iqtest$station_avg_temp_c,na.rm=TRUE)
iqtest$station_diur_temp_rng_c[is.na(iqtest$station_diur_temp_rng_c)]=median(iqtest$station_diur_temp_rng_c,na.rm=TRUE)
iqtest$station_max_temp_c[is.na(iqtest$station_max_temp_c)]=median(iqtest$station_max_temp_c,na.rm=TRUE)
iqtest$station_min_temp_c[is.na(iqtest$station_min_temp_c)]=median(iqtest$station_min_temp_c,na.rm=TRUE)
iqtest$station_precip_mm[is.na(iqtest$station_precip_mm)]=median(iqtest$station_precip_mm,na.rm=TRUE)
summary(iqtest)
str(iqtest)
sjtest$ndvi_ne[is.na(sjtest$ndvi_ne)]=median(sjtest$ndvi_ne,na.rm=TRUE)
sjtest$ndvi_nw[is.na(sjtest$ndvi_nw)]=median(sjtest$ndvi_nw,na.rm=TRUE)
sjtest$ndvi_se[is.na(sjtest$ndvi_se)]=median(sjtest$ndvi_se,na.rm=TRUE)
sjtest$ndvi_sw[is.na(sjtest$ndvi_sw)]=median(sjtest$ndvi_sw,na.rm=TRUE)
sjtest$precipitation_amt_mm[is.na(sjtest$precipitation_amt_mm)]=median(sjtest$precipitation_amt_mm,na.rm=TRUE)
sjtest$reanalysis_air_temp_k[is.na(sjtest$reanalysis_air_temp_k)]=median(sjtest$reanalysis_air_temp_k,na.rm=TRUE)
sjtest$reanalysis_avg_temp_k[is.na(sjtest$reanalysis_avg_temp_k)]=median(sjtest$reanalysis_avg_temp_k,na.rm=TRUE)
sjtest$reanalysis_dew_point_temp_k[is.na(sjtest$reanalysis_dew_point_temp_k)]=median(sjtest$reanalysis_dew_point_temp_k,na.rm=TRUE)
sjtest$reanalysis_max_air_temp_k[is.na(sjtest$reanalysis_max_air_temp_k)]=median(sjtest$reanalysis_max_air_temp_k,na.rm=TRUE)
sjtest$reanalysis_min_air_temp_k[is.na(sjtest$reanalysis_min_air_temp_k)]=median(sjtest$reanalysis_min_air_temp_k,na.rm=TRUE)
sjtest$reanalysis_precip_amt_kg_per_m2[is.na(sjtest$reanalysis_precip_amt_kg_per_m2)]=median(sjtest$reanalysis_precip_amt_kg_per_m2,na.rm=TRUE)
sjtest$reanalysis_relative_humidity_percent[is.na(sjtest$reanalysis_relative_humidity_percent)]=median(sjtest$reanalysis_relative_humidity_percent,na.rm=TRUE)
sjtest$reanalysis_sat_precip_amt_mm[is.na(sjtest$reanalysis_sat_precip_amt_mm)]=median(sjtest$reanalysis_sat_precip_amt_mm,na.rm=TRUE)
sjtest$reanalysis_specific_humidity_g_per_kg[is.na(sjtest$reanalysis_specific_humidity_g_per_kg)]=median(sjtest$reanalysis_specific_humidity_g_per_kg,na.rm=TRUE)
sjtest$reanalysis_tdtr_k[is.na(sjtest$reanalysis_tdtr_k)]=median(sjtest$reanalysis_tdtr_k,na.rm=TRUE)
sjtest$station_avg_temp_c[is.na(sjtest$station_avg_temp_c)]=median(sjtest$station_avg_temp_c,na.rm=TRUE)
sjtest$station_diur_temp_rng_c[is.na(sjtest$station_diur_temp_rng_c)]=median(sjtest$station_diur_temp_rng_c,na.rm=TRUE)
sjtest$station_max_temp_c[is.na(sjtest$station_max_temp_c)]=median(sjtest$station_max_temp_c,na.rm=TRUE)
sjtest$station_min_temp_c[is.na(sjtest$station_min_temp_c)]=median(sjtest$station_min_temp_c,na.rm=TRUE)
sjtest$station_precip_mm[is.na(sjtest$station_precip_mm)]=median(sjtest$station_precip_mm,na.rm=TRUE)
summary(sjtest)
str(sjtest)

iqtest=ts(iqtest[5:24],start=c(2010,07,02),end=c(2013,06,25),frequency=52)
str(iqtest)
sjtest=ts(sjtest[5:24],start=c(2008,04,29),frequency=52)
str(sjtest)

iqvar=cbind(iqtest[,'ndvi_ne'],
             iqtest[,'precipitation_amt_mm'],
             iqtest[,'reanalysis_avg_temp_k'],
             iqtest[,'reanalysis_specific_humidity_g_per_kg'],
             iqtest[,'reanalysis_tdtr_k'])
sjvar=cbind(sjtest[,'ndvi_ne'],
             sjtest[,'precipitation_amt_mm'],
             sjtest[,'reanalysis_avg_temp_k'],
             sjtest[,'reanalysis_specific_humidity_g_per_kg'],
             sjtest[,'reanalysis_tdtr_k'])
iqxvar6=cbind(iqtest[,'ndvi_ne'],
           iqtest[,'precipitation_amt_mm'],
           iqtest[,'reanalysis_avg_temp_k'],
           iqtest[,'reanalysis_specific_humidity_g_per_kg'],
           iqtest[,'reanalysis_tdtr_k'])
iqxvar6=iqxvar6+1
sjxvar6=cbind(sjtest[,'ndvi_ne'],
           sjtest[,'precipitation_amt_mm'],
           sjtest[,'reanalysis_avg_temp_k'],
           sjtest[,'reanalysis_specific_humidity_g_per_kg'],
           sjtest[,'reanalysis_tdtr_k'])
sjxvar6=sjxvar6+1

iqfc4=forecast(iqfit4,xreg=iqvar)
sjfc4=forecast(sjfit4,xreg=sjvar)

iqfc5=forecast(iqfit5,xreg=iqvar)
sjfc5=forecast(sjfit5,xreg=sjvar)

iqfc6=forecast(iqfit6,xreg=iqxvar6)
sjfc6=forecast(sjfit6,xreg=sjxvar6)

autoplot(iqfc4) + xlab('Year') + ylab('Iquitos Total Cases')
autoplot(iqfc5) + xlab('Year') + ylab('Iquitos Total Cases')
autoplot(iqfc6) + xlab('Year') + ylab('Iquitos Total Cases')

autoplot(sjfc4) + xlab('Year') + ylab('San Juan Total Cases')
autoplot(sjfc5) + xlab('Year') + ylab('San Juan Total Cases')
autoplot(sjfc6) + xlab('Year') + ylab('San Juan Total Cases')

write.csv(iqfc4,file='IQ4.csv',row.names=FALSE)
write.csv(iqfc5,file='IQ5.csv',row.names=FALSE)
write.csv(iqfc6,file='IQ6.csv',row.names=FALSE)

write.csv(sjfc4,file='SJ4.csv',row.names=FALSE)
write.csv(sjfc5,file='SJ5.csv',row.names=FALSE)
write.csv(sjfc6,file='SJ6.csv',row.names=FALSE)

# scores on validation dataset on drivendata.org competition website
# model 4 MAE: 33.1875
# model 5 MAE: 32.0865 
# model 6 MAE: 31.5625
# to submit model 6, need to get the predicted values - 1 b/c add 1 to each variable earlier 
# using best model 6, have current rank 1743 out of 5192 competitors
# so that's 33.6%, top 1/3 of competition
# #1 model on drivendata.org leaderboard has MAE of 13.0144 with 22 entries 
# train set, using all metrics in accuracy() function, indicates model 5 is best 
# test set, using MAE on drivendata.org competition website, shows model 6 is best
# model 5 probably has overfit issue since it's best in train performance but not in test performance 
# best model is model 6 using neural networks with Box Cox transformation lambda='auto'
# in model 6, IQ uses NNAR(6,1,6)[52], SJ uses NNAR(16,1,12)[52]


