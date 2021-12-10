# Assignment 5 by Mimi Trinh
RANDOM_SEED = 9999
SET_FIT_INTERCEPT = True
import numpy as np
import pandas as pd
import sklearn.linear_model 
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.metrics import mean_squared_error, r2_score  
from math import sqrt
from sklearn.preprocessing import StandardScaler
import matplotlib
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
from sklearn.pipeline import Pipeline
from sklearn.datasets import fetch_mldata
from __future__ import division, print_function
from sklearn.decomposition import PCA
from sklearn.metrics import f1_score
import csv
import time
mnist = fetch_mldata('MNIST original', data_home='./')
mnist.data.shape
mnist
X, y = mnist['data'], mnist['target']
X.shape
y.shape
# Display 36,000th image
some_digit = X[36000]
some_digit_image = some_digit.reshape(28, 28)
plt.imshow(some_digit_image, cmap = matplotlib.cm.binary, 
           interpolation = 'nearest')
plt.axis('off')
plt.show()
# Utilize the first 60,000 images as train set and the last 10,000 as holdout test set 
X_train, X_test, y_train, y_test = X[:60000], X[60000:], y[:60000], y[60000:]
# Model 1: Random Forest Classifier using Train-Test Split
rfClf = RandomForestClassifier(n_estimators=100, max_leaf_nodes=10, n_jobs=-1)    
np.random.seed(seed = 9999)
replications = 10  # repeat the trial ten times
x_time = [] 
n = 0  
print('--- Time to Fit Random Forest Classifier ---')
while (n < replications): 
    start_time = time.clock()
    # generate 1 million random negative binomials and store in a vector
    rfClf.fit(X_train, y_train)
    end_time = time.clock()
    runtime = end_time - start_time  # seconds of wall-clock time
    x_time.append(runtime * 1000)  # report in milliseconds 
    print("replication", n + 1, ":", x_time[n], "milliseconds\n") 
    n = n + 1
with open('rf_fit.csv', 'wt') as f:
    writer = csv.writer(f, quoting = csv.QUOTE_NONNUMERIC, dialect = 'excel')
    writer.writerow('x_time')    
    for i in range(replications):
        writer.writerow(([x_time[i],]))
# preliminary analysis for this cell of the design
print(pd.DataFrame(x_time).describe())
y_predict = rfClf.predict(X_test)
# Performance measurement using F1-score
f1Score = f1_score(y_test, y_predict, average='weighted')
print('\nF1 Score: ', f1Score)
# Model 2: PCA using the Full Dataset
pca = PCA(n_components=0.95)
pca_time = []
n = 0  
print('--- Time to Identify Pricipal Components ---')
while (n < replications): 
    start_time = time.clock()
    # generate 1 million random negative binomials and store in a vector
    X_pca = pca.fit_transform(X) 
    end_time = time.clock()
    runtime = end_time - start_time  # seconds of wall-clock time
    pca_time.append(runtime * 1000)  # report in milliseconds 
    print("replication", n + 1, ":", pca_time[n], "milliseconds\n") 
    n = n + 1
with open('pca_fit.csv', 'wt') as f:
    writer = csv.writer(f, quoting = csv.QUOTE_NONNUMERIC, dialect = 'excel')
    writer.writerow('pca_time')    
    for i in range(replications):
        writer.writerow(([pca_time[i],]))
print(pd.DataFrame(pca_time).describe())
pca_explained_variance = pca.explained_variance_ratio_
print('Proportion of variance explained:', pca_explained_variance)
print('Number of principal components that represent 95% varibility:',len(pca_explained_variance))
# Dimension is reduced to 154 variables from 784 variables
# Model 3: Random Forest Classifier and PCA using Train-Test Split
X_pca_train, X_pca_test = X_pca[:60000], X_pca[60000:]
rfClf_pca = RandomForestClassifier(n_estimators=100, max_leaf_nodes=10, n_jobs=-1)
x_pca_time = [] 
n = 0  
print('--- Time to Fit Random Forest Classifier using Principal Components ---')
while (n < replications): 
    start_time = time.clock()
    # generate 1 million random negative binomials and store in a vector
    rfClf_pca.fit(X_pca_train, y_train)
    end_time = time.clock()
    runtime = end_time - start_time  # seconds of wall-clock time
    x_pca_time.append(runtime * 1000)  # report in milliseconds 
    print("replication", n + 1, ":", x_pca_time[n], "milliseconds\n") 
    n = n + 1
with open('rf_pca_fit.csv', 'wt') as f:
    writer = csv.writer(f, quoting = csv.QUOTE_NONNUMERIC, dialect = 'excel')
    writer.writerow('x_pca_time')    
    for i in range(replications):
        writer.writerow(([x_pca_time[i],]))
print(pd.DataFrame(x_pca_time).describe())
y_predict_pca = rfClf_pca.predict(X_pca_test)
f1Score_pca = f1_score(y_test, y_predict_pca, average='weighted')
print('\nF1 Score: ', f1Score_pca)
# Model 4: Random Forest Classifier and PCA using Train-Test Split and Standard Scaler
mnist = fetch_mldata('MNIST original', data_home='./')
mnist.data.shape
mnist
X, y = mnist['data'], mnist['target']
scaler=StandardScaler()
scaler.fit(X)
X=scaler.transform(X)
X_train, X_test, y_train, y_test = X[:60000], X[60000:], y[:60000], y[60000:]
pca = PCA(n_components=0.95)
pca_time = []
n = 0  
print('--- Time to Identify Pricipal Components ---')
while (n < replications): 
    start_time = time.clock()
    # generate 1 million random negative binomials and store in a vector
    X_pca = pca.fit_transform(X) 
    end_time = time.clock()
    runtime = end_time - start_time  # seconds of wall-clock time
    pca_time.append(runtime * 1000)  # report in milliseconds 
    print("replication", n + 1, ":", pca_time[n], "milliseconds\n") 
    n = n + 1
with open('pca_fit.csv', 'wt') as f:
    writer = csv.writer(f, quoting = csv.QUOTE_NONNUMERIC, dialect = 'excel')
    writer.writerow('pca_time')    
    for i in range(replications):
        writer.writerow(([pca_time[i],]))
print(pd.DataFrame(pca_time).describe())
pca_explained_variance = pca.explained_variance_ratio_
print('Proportion of variance explained:', pca_explained_variance)
print('Number of principal components that represent 95% varibility:',len(pca_explained_variance))
X_pca_train, X_pca_test = X_pca[:60000], X_pca[60000:]
rfClf_pca = RandomForestClassifier(n_estimators=100, max_leaf_nodes=10, n_jobs=-1)
x_pca_time = [] 
n = 0  
print('--- Time to Fit Random Forest Classifier using Principal Components ---')
while (n < replications): 
    start_time = time.clock()
    # generate 1 million random negative binomials and store in a vector
    rfClf_pca.fit(X_pca_train, y_train)
    end_time = time.clock()
    runtime = end_time - start_time  # seconds of wall-clock time
    x_pca_time.append(runtime * 1000)  # report in milliseconds 
    print("replication", n + 1, ":", x_pca_time[n], "milliseconds\n") 
    n = n + 1
with open('rf_pca_fit.csv', 'wt') as f:
    writer = csv.writer(f, quoting = csv.QUOTE_NONNUMERIC, dialect = 'excel')
    writer.writerow('x_pca_time')    
    for i in range(replications):
        writer.writerow(([x_pca_time[i],]))
print(pd.DataFrame(x_pca_time).describe())
y_predict_pca = rfClf_pca.predict(X_pca_test)
f1Score_pca = f1_score(y_test, y_predict_pca, average='weighted')
print('\nF1 Score: ', f1Score_pca)
#332 components from 784 variables 
# TA Jessica helped me with this project.
# Thank you for viewing my code. 