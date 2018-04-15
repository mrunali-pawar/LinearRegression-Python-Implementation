# -*- coding: utf-8 -*-
"""
Created on Sat Apr 14 14:11:43 2018

@author: Mrunali Pawar
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

#import data
data = pd.read_csv("Concrete_Data.csv")

#Differenciate dependent and independent variables
x = data.iloc[:,0:8]
y = data.iloc[:,8:]

#Split data into train and test (80-20%)
from sklearn.cross_validation import train_test_split
x_train,x_test,y_train,y_test = train_test_split(x,y,test_size = 0.2,random_state = 100) 

#Build the model
from sklearn.linear_model import LinearRegression
lm = LinearRegression()
lm = lm.fit(x_train,y_train)

#Coefficient array
lm.coef_
coefficients = pd.concat([pd.DataFrame(x_train.columns),pd.DataFrame(np.transpose(lm.coef_))], axis = 1)
coefficients

#Prediction
y_pred = lm.predict(x_test)

from sklearn.metrics import r2_score
r2_score(y_test,y_pred)

