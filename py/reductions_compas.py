import functools
import numpy as np
import pandas
import fairlearn.reductions as red

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score

import warnings
warnings.filterwarnings("ignore", category=FutureWarning)

def reduce_and_predict(df1, df2, epsilon):
  # concatenate the data and clean it
  df = pandas.concat([df1, df2])
  ntrain = len(df1)
  ntest = len(df2)

  df = pandas.get_dummies(df, prefix = ['sex', 'race', 'c_charge_degree'], drop_first = True)
  df = df.rename(columns = {'race_Non-White':'race', 'sex_Male':'sex', 'c_charge_degree_M':'charge_degree'})
  df = df.astype('int64')
  # set up the BinaryLabelDataset
  label_names = ['two_year_recid']
  protected_attribute_names = ['race']
  train_data = df.head(ntrain)
  test_data = df.tail(ntest)


  dataX = train_data.drop(columns = ['two_year_recid', 'race'])
  dataA = train_data['race']
  dataY = train_data['two_year_recid']
  X_test = test_data.drop(columns = ['two_year_recid', 'race'])
  learner = LogisticRegression()


  expgrad = red.ExponentiatedGradient(learner, constraints = red.DemographicParity(), eps = epsilon)
  expgrad.fit(dataX, dataY, sensitive_features = dataA)

  Y_hat = expgrad.predict(X_test)

  return Y_hat
