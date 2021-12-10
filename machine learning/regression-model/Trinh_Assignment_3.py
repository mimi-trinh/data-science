# Assignment 3 by Mimi Trinh
RANDOM_SEED = 1
SET_FIT_INTERCEPT = True
import numpy as np
import pandas as pd
import sklearn.linear_model 
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.metrics import mean_squared_error, r2_score  
from math import sqrt
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from sklearn.model_selection import cross_val_score
from sklearn.metrics import mean_squared_error
import statsmodels.api as sm
boston_input = pd.read_csv('boston.csv')
print('\nboston DataFrame (first and last five rows):')
print(boston_input.head())
print(boston_input.tail())
print('\nGeneral description of the boston_input DataFrame:')
print(boston_input.info())
boston_input.shape
# drop neighborhood from the data being considered
boston = boston_input.drop('neighborhood', 1)
print('\nGeneral description of the boston DataFrame:')
print(boston.info())
# no NA value, no need to address missing value issue
print('\nDescriptive statistics of the boston DataFrame:')
print(boston.describe())
boston.shape
# create model data with the first column as response variable (mv) and remaining columns as predictor variables
prelim_model_data = np.array([boston.mv,\
    boston.crim,\
    boston.zn,\
    boston.indus,\
    boston.chas,\
    boston.nox,\
    boston.rooms,\
    boston.age,\
    boston.dis,\
    boston.rad,\
    boston.tax,\
    boston.ptratio,\
    boston.lstat]).T
print('\nData dimensions:', prelim_model_data.shape)
# best practice to standardize variables into a single scale
scaler = StandardScaler()
print(scaler.fit(prelim_model_data))
print(scaler.mean_)
print(scaler.scale_)
model_data = scaler.fit_transform(prelim_model_data)
# use this model data for analysis after scaler transformation
print('\nDimensions for model_data:', model_data.shape)
visual_data=pd.DataFrame(model_data)
visual_data.columns=['mv','crim','zn','indus','chas','nox','rooms','age','dis','rad','tax','ptratio','lstat']
visual_data.info()
plt.plot(visual_data.mv,color='green')
plt.title('Median Values of Homes in Thousands of 1970 Dollars',color='blue')
plt.boxplot(visual_data.mv,patch_artist=True)
plt.title('Median Values of Homes in Thousands of 1970 Dollars',color='green')
# there are many outliers in response variable, skewing toward the right 
# no outlier outside +/-3, so no extreme outlier
corr_matrix=visual_data.corr()
corr_matrix['mv'].sort_values(ascending=False)
visual_data.plot(kind='scatter',x='rooms',y='mv',color='green')
plt.title('Average # of Rooms vs. Median Home Values',color='blue')
plt.xlabel('Average # of Rooms per Home',color='blue')
plt.ylabel('Median Value of Homes in Thousands of 1970 Dollars',color='blue')
visual_data.plot(kind='scatter',x='lstat',y='mv',color='red')
plt.title('Lower Socio-economic Status vs. Median Home Values',color='blue')
plt.xlabel('% of Population of Lower Socio-Economic Status',color='blue')
plt.ylabel('Median Value of Homes in Thousands of 1970 Dollars',color='blue')
visual_data.head()
visual_data.tail()
model_data
y=model_data[:, :1]
y
x=model_data[:,1:13]
x
# model 1: linear regression
linear=LinearRegression()
linear.fit(x,y)
linear.score(x,y) 
# this is R2
linear_predict=linear.predict(x)
sqrt(mean_squared_error(y,linear_predict)) 
# RMSE of linear regression 
scores1=cross_val_score(linear, x, y, cv=10, scoring='neg_mean_squared_error')
scores1=np.sqrt(-scores1)
scores1 
# RMSE scores using 10 fold cross validation
scores1.mean()
# mean of 10 RMSE scores
# model 2: ridge regression
ridge=Ridge(alpha=10).fit(x,y)
ridge.score(x,y)
# lower R2 than linear regression, linear regression is better model
ridge_predict=ridge.predict(x)
np.sqrt(mean_squared_error(y,ridge_predict))
# higher RMSE, linear regression is better model
scores2=cross_val_score(ridge, x, y, cv=10, scoring='neg_mean_squared_error')
scores2=np.sqrt(-scores2)
scores2 
# RMSE scores using 10 fold cross validation
scores2.mean()
# lower mean of 10 RMSE, ridge regression is better model 
# conclusion: ridge regression is better than linear regression 
# b/c mean of 10 RMSE of ridge regression < mean of 10 RMSE of linear regression using 10 fold cross validation
lasso=Lasso().fit(x,y)
lasso.score(x,y)
# really bad R2
lasso_predict=lasso.predict(x)
np.sqrt(mean_squared_error(y,lasso_predict))
# really bad RMSE
scores3=cross_val_score(lasso, x, y, cv=10, scoring='neg_mean_squared_error')
scores3=np.sqrt(-scores3)
scores3 
# RMSE scores using 10 fold cross validation
scores3.mean()
# really bad (high) mean of 10 RMSE scores using 10 fold cross validation
# conclusion: ridge model is still winning here with lowest mean of 10 RMSE
elastic=ElasticNet().fit(x,y)
elastic.score(x,y)
# very low R2
elastic_predict=elastic.predict(x)
np.sqrt(mean_squared_error(y,elastic_predict))
# very high RMSE
scores4=cross_val_score(elastic, x, y, cv=10, scoring='neg_mean_squared_error')
scores4=np.sqrt(-scores4)
scores4 
# RMSE scores using 10 fold cross validation
scores4.mean()
# still very high RMSE mean
# conclusion: using the mean of 10 RMSE scores of 10 fold cross validation, best model is ridge regression
# in order of ridge > linear > elastic net > lasso 
ridge.intercept_
# intercept of ridge regression
ridge.coef_
# coefficients of ridge regression
# Thank you for viewing my code!