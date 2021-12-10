# Assignment 3 by Mimi Trinh
RANDOM_SEED = 9999
SET_FIT_INTERCEPT = True
import numpy as np
import pandas as pd
import sklearn.linear_model 
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score  
from math import sqrt
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from sklearn.tree import export_graphviz
from sklearn import model_selection
from sklearn.model_selection import cross_val_score, KFold
from sklearn.metrics import mean_squared_error, confusion_matrix, roc_curve, roc_auc_score
import statsmodels.api as sm
from sklearn import model_selection
from sklearn.model_selection import train_test_split
import os
from sklearn.svm import SVC
import seaborn as sns; sns.set(font_scale=1.2)
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
names = ["Linear_Regression", "Ridge_Regression","Random_Forest_7","Random_Forest_3","Random_Forest_10"]
classifiers = [LinearRegression(), Ridge(alpha=10), 
               RandomForestRegressor(n_estimators=100,criterion='mse',max_depth=None, min_samples_split=2,
                                    min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features=7,
                                    max_leaf_nodes=None, min_impurity_split=0.01, bootstrap=True, oob_score=False,
                                    n_jobs=1, random_state=RANDOM_SEED,verbose=0,warm_start=False),
              RandomForestRegressor(n_estimators=100,criterion='mse',max_depth=None, min_samples_split=2,
                                    min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features=3,
                                    max_leaf_nodes=None, min_impurity_split=0.01, bootstrap=True, oob_score=False,
                                    n_jobs=1, random_state=RANDOM_SEED,verbose=0,warm_start=False),
              RandomForestRegressor(n_estimators=100,criterion='mse',max_depth=None, min_samples_split=2,
                                    min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features=10,
                                    max_leaf_nodes=None, min_impurity_split=0.01, bootstrap=True, oob_score=False,
                                    n_jobs=1, random_state=RANDOM_SEED,verbose=0,warm_start=False)]
N_FOLDS = 10
cv_results = np.zeros((N_FOLDS, len(names)))
kf = KFold(n_splits = N_FOLDS, shuffle=False, random_state = RANDOM_SEED)
index_for_fold = 0  # fold count initialized 
for train_index, test_index in kf.split(model_data):
    print('\nFold index:', index_for_fold,
          '------------------------------------------')  
    X_train = model_data[train_index, 1:model_data.shape[1]]
    X_test = model_data[test_index, 1:model_data.shape[1]]
    y_train = model_data[train_index, 0]
    y_test = model_data[test_index, 0]  
    print('\nShape of input data for this fold:',
          '\nData Set: (Observations, Variables)')
    print('X_train:', X_train.shape)
    print('X_test:',X_test.shape)
    print('y_train:', y_train.shape)
    print('y_test:',y_test.shape)
    index_for_method = 0  # initialize
    for name, clf in zip(names, classifiers):
        print('\nClassifier evaluation for:', name)
        print('  Scikit Learn method:', clf)
        clf.fit(X_train, y_train)  # fit on the train set for this fold
        # evaluate on the test set for this fold
        y_test_predict = clf.predict(X_test)
        fold_method_result = np.sqrt(mean_squared_error(y_test,y_test_predict)) 
        print('RMSE:', fold_method_result)
        cv_results[index_for_fold, index_for_method] = fold_method_result
        index_for_method += 1
    index_for_fold += 1
cv_results_df = pd.DataFrame(cv_results)
cv_results_df.columns = names
print('\n----------------------------------------------')
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      '\nMethod                 RMSE', sep = '')     
print(cv_results_df.mean()) 
# RMSE above show that random forest is the best model with lowest RMSE
# among 3 random forest models, the best one is the random forest with max_features=7
# we will apply random forest with max_features to the full dataset to obtain results for management
x=model_data[:,1:model_data.shape[1]]
y=model_data[:,0]
model=RandomForestRegressor(n_estimators=100,criterion='mse',max_depth=None, min_samples_split=2,
                                    min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features=7,
                                    max_leaf_nodes=None, min_impurity_split=0.01, bootstrap=True, oob_score=False,
                                    n_jobs=1, random_state=RANDOM_SEED,verbose=0,warm_start=False)
model.fit(x,y)
print('\n---------------------------------------------------------------------------')
print('Random Forests Regression Model Explanatory Variable Importance Results')
var_name = [    
    'crim',
    'zn',
    'indus',
    'chas',
    'nox',
    'rooms',
    'age',
    'dis',
    'rad',
    'tax',
    'ptratio',
    'lstat']
var_description = [  
    'Crime rate',
    'Percentage of land zoned for lots',
    'Percentage of business that is industrial or nonretail',
    'On the Charles River (1) or not (0)',
    'Air pollution (nitrogen oxide concentration)',
    'Average number of rooms per home',
    'Percentage of homes built before 1940',
    'Weighted distance to employment centers',
    'Accessibility to radial highways',
    'Tax rate',
    'Pupil/teacher ratio in public schools',
    'Percentage of population of lower socio-economic status']
var_importance = model.feature_importances_
final_model_results = pd.DataFrame({'name': var_name,
                                    'description': var_description,
                                    'importance': var_importance})

print(final_model_results.sort_values('importance', ascending = False))  
# Thank you Dr. Miller and TA Jessica for helping me with the code.
# Thank you for viewing my code!