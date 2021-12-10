# Assignment 2 Python Code by Mimi Trinh
RANDOM_SEED = 1
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score
from sklearn.metrics import confusion_matrix, roc_curve, roc_auc_score
import matplotlib.pyplot as plt
from sklearn.naive_bayes import BernoulliNB
from sklearn.preprocessing import StandardScaler 
from sklearn.svm import SVC
import seaborn as sns; sns.set(font_scale=1.2)
from sklearn import model_selection
from sklearn.model_selection import train_test_split
import os
bank = pd.read_csv('bank.csv', sep = ';')  
print(bank.shape)
# there are 4521 rows / observations and 17 columns / variables in the dataset
bank.info()
# there's no missing value in dataset
bank.dropna()
print(bank.shape)
# confirm there's no missing value in dataset b/c have the same # of rows after using dropna()
list(bank.columns.values)
bank.head()
pd.value_counts(bank['response'])
# only 11.5% observations belong to positive class (yes response)
pd.value_counts(bank['default'])
pd.value_counts(bank['housing'])
pd.value_counts(bank['loan'])
bank['response'].value_counts().plot(kind='bar',color='blue')
plt.title('Response Class Proportion',color='red')
bank['default'].value_counts().plot(kind='bar',color='blue')
plt.title('Default Class Proportion',color='red')
bank['housing'].value_counts().plot(kind='bar',color='blue')
plt.title('Housing Class Proportion',color='red')
bank['loan'].value_counts().plot(kind='bar',color='blue')
plt.title('Loan Class Proportion',color='red')
# except housing, all three variables (response, default, loan) have unequal proportion of class (yes/no)
# this may cause issues later on when running logistic regression and naive bayes model
# mapping function to convert text no/yes to integer 0/1
convert_to_binary = {'no' : 0, 'yes' : 1}
# define binary variable for having credit in default
default = bank['default'].map(convert_to_binary)
# define binary variable for having a mortgage or housing loan
housing = bank['housing'].map(convert_to_binary)
# define binary variable for having a personal loan
loan = bank['loan'].map(convert_to_binary)
# define response variable to use in the model
response = bank['response'].map(convert_to_binary)
# gather three explanatory variables and response into a numpy array 
# here we use .T to obtain the transpose for the structure we want
# need to convert bank dataframe to model_data array to run logistic regression
model_data = np.array([np.array(default), np.array(housing), np.array(loan), 
    np.array(response)]).T
# examine the shape of model_data, which we will use in subsequent modeling
print(model_data.shape)
# model data now only have 4 columns but still have all rows
model_data
# predictor variables: default, housing, loan
x=model_data[:,:3]
x
# dependent variable: response
y=model_data[:,3]
y
pd.value_counts(model_data[:,3])
# classification model #1: logistic regression
logit=LogisticRegression()
logit.fit(x,y)
# quick assessment of model performance
logit.score(x,y)
# the model correctly classifies 88% of all observations 
# predict() will give only output of 0 or 1
# predict_proba() will give probability of 1
# ie: model.predict_proba(test)[:,1] here 1 means to predict 1. If change to 0, model will give probability of 0
# can't use predict_proba() with confusion_matrix()
# confusion matrix
confusion_matrix(y,logit.predict(x))
# each row respresnts actual class, each column represents predicted class
# first row includes negative class or no response 
# true negative 4000 means 4000 observations are correctly classified as no response (actual = predict = no)
# false positive 0 means 0 observation is incorrectly classfied as yes response (actual = no, predict = yes)
# false negative 521 means that 521 observations are incorrectly classified as no response (actual = yes, predict = no)
# true positive 0 means that 0 observation is correctly classified as yes response (actual = predict = yes)
# confusion matrix shows 100% correct classification for class 0 (no) and 100% error for class 1 (yes)
# b/c response column is highly imbalanced with only 11.5% belong to positive class 1 (yes)
# draw ROC curve
fpr, tpr, thresholds = roc_curve(y,logit.predict_proba(x)[:,1])
plt.figure()
plt.plot(fpr, tpr, color='red',
 lw=2, label='ROC curve')
plt.plot([0, 1], [0, 1], color='blue', lw=2, linestyle='--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve - Logistic Regression Model')
plt.text(0,0.9,'\nROC AUC is '+str(round(roc_auc_score(y,logit.predict_proba(x)[:,1]),2)),fontsize=20)
plt.show()
# calculate AUC of ROC curve using the following formula
roc_auc_score(y,logit.predict_proba(x)[:,1])
# calculate AUC of ROC using cross validation design
scores1 = cross_val_score(logit, x, y, cv=10, scoring='roc_auc')
print(scores1)
# the result now is not as great as the 88% logit score metric earlier anymore b/c only 11.5% dataset is yes response 
# mean of 10 evaluation scores above
scores1.mean()
# standard deviation of 10 evaluation scores above
scores1.std()
# coefficients of logistic regression model
result1=logit.fit(x,y)
result1.coef_
# classification model #2: naive Bayes 
bayes = BernoulliNB()
bayes.fit(x,y)
# quick assessment of model performance
bayes.score(x,y)
# the model correctly classifies 88% of all observations
# bayes model has same result as logit model
# confusion matrix
confusion_matrix(y,bayes.predict(x))
# bayes model has same result as logit model
# draw ROC curve
fpr, tpr, thresholds = roc_curve(y,bayes.predict_proba(x)[:,1])
plt.figure()
plt.plot(fpr, tpr, color='red',
 lw=2, label='ROC curve')
plt.plot([0, 1], [0, 1], color='blue', lw=2, linestyle='--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve - Naive Bayes Model')
plt.text(0,0.9,'\nROC AUC is '+str(round(roc_auc_score(y,bayes.predict_proba(x)[:,1]),2)),fontsize=20)
plt.show()
# calculate AUC of ROC curve using the following formula
roc_auc_score(y,bayes.predict_proba(x)[:,1])
# same result as logit
# calculate AUC of ROC using cross validation design
scores2 = cross_val_score(bayes, x, y, cv=10, scoring='roc_auc')
print(scores2)
# all scores below are same as logit except minor difference in one 
# mean of 10 evaluation scores above
scores2.mean()
# standard deviation of 10 evaluation scores above
scores2.std()
# coefficients of lbayes model
result2=bayes.fit(x,y)
result2.coef_
# bayes and logit coefficients are very different in terms of absolute values and positive / negative signs
# TA Jessica has helped me with the code. 
# the code above uses cross_val_score() to automatically do 10-fold cross validation design
# the code below uses KFold() to manually set up the 10-fold cross validation design
# it also applies the model to predict a set of data 
RANDOM_SEED = 1
names = ["Naive_Bayes", "Logistic_Regression"]
classifiers = [BernoulliNB(alpha=1.0, binarize=0.5, 
                           class_prior = [0.5, 0.5], fit_prior=False), 
               LogisticRegression()]
N_FOLDS = 10
cv_results = np.zeros((N_FOLDS, len(names)))
kf = KFold(n_splits = N_FOLDS, shuffle=False, random_state = RANDOM_SEED)
index_for_fold = 0  # fold count initialized 
for train_index, test_index in kf.split(model_data):
    print('\nFold index:', index_for_fold,
          '------------------------------------------')
#   note that 0:model_data.shape[1]-1 slices for explanatory variables
#   and model_data.shape[1]-1 is the index for the response variable    
    X_train = model_data[train_index, 0:model_data.shape[1]-1]
    X_test = model_data[test_index, 0:model_data.shape[1]-1]
    y_train = model_data[train_index, model_data.shape[1]-1]
    y_test = model_data[test_index, model_data.shape[1]-1]   
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
        y_test_predict = clf.predict_proba(X_test)
        fold_method_result = roc_auc_score(y_test, y_test_predict[:,1]) 
        print('Area under ROC curve:', fold_method_result)
        cv_results[index_for_fold, index_for_method] = fold_method_result
        index_for_method += 1
    index_for_fold += 1
cv_results_df = pd.DataFrame(cv_results)
cv_results_df.columns = names
print('\n----------------------------------------------')
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      '\nMethod                 Area under ROC Curve', sep = '')     
print(cv_results_df.mean())   
# because Logistic Regression and Naive Bayes Classification provide the same result
# we only need to apply one test on the following dataset to predict the response variable
# so we use Logistic Regression
my_default = np.array([1, 1, 1, 1, 0, 0, 0, 0], np.int32)
my_housing = np.array([1, 1, 0, 0, 1, 1, 0, 0], np.int32)
my_loan = np.array([1, 0, 1, 0, 1, 0, 1, 0], np.int32)
my_X_test = np.vstack([my_default, my_housing, my_loan]).T
clf = LogisticRegression()
X_train = model_data[:, 0:model_data.shape[1]-1]
y_train = model_data[:, model_data.shape[1]-1]
clf.fit(X_train, y_train)
y_my_test_predict = clf.predict_proba(my_X_test)
my_targeting_df = pd.DataFrame(np.hstack([my_X_test, y_my_test_predict]))
my_targeting_df.columns = ['default', 'housing', 'loan', 
                           'predict_NO', 'predict_YES']
print('\n\nLogistic regression model predictions for test cases:')
print(my_targeting_df) 
# Thank you for viewing my code! 