import pandas
from aif360.datasets import BinaryLabelDataset
from aif360.datasets import AdultDataset, GermanDataset, CompasDataset
from aif360.metrics import BinaryLabelDatasetMetric
from aif360.metrics import ClassificationMetric
from aif360.algorithms.preprocessing.reweighing import Reweighing

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score

import warnings
warnings.filterwarnings("ignore", category=FutureWarning)


def reweigh_and_predict(df1, df2):
  # concatenate the data and clean it
  df = pandas.concat([df1, df2])
  ntrain = df1.shape[0]
  ntest = df2.shape[0]
  df1 = df
  #df = pandas.read_csv("compas.csv")
  df = pandas.get_dummies(df, prefix = ['sex', 'race', 'c_charge_degree'], drop_first = True)
  df = df.rename(columns = {'race_Non-White':'race', 'sex_Male':'sex', 'c_charge_degree_M':'charge_degree'})
  # set up the BinaryLabelDataset
  label_names = ['two_year_recid']
  protected_attribute_names = ['race']
  train_data = df.head(ntrain)
  test_data = df.tail(ntest)


  train_data = BinaryLabelDataset(df = train_data, label_names = label_names,
                                   protected_attribute_names = protected_attribute_names)
  test_data = BinaryLabelDataset(df = test_data, label_names = label_names,
                                   protected_attribute_names = protected_attribute_names)

  privileged_groups = [{'race': 1}]
  unprivileged_groups = [{'race':0}]
  RW = Reweighing(unprivileged_groups=unprivileged_groups,
                 privileged_groups=privileged_groups)
  RW.fit(train_data)
  dataset_transf_train = RW.transform(train_data)

  scale_transf = StandardScaler()
  X_train = scale_transf.fit_transform(dataset_transf_train.features)
  y_train = dataset_transf_train.labels.ravel()

  lmod = LogisticRegression()
  lmod.fit(X_train, y_train,
        sample_weight=dataset_transf_train.instance_weights)
  y_train_pred = lmod.predict(X_train)

  dataset_transf_test_pred = test_data
  X_test = scale_transf.fit_transform(dataset_transf_test_pred.features)
  y_test = dataset_transf_test_pred.labels
  dataset_transf_test_pred.scores = lmod.predict(X_test)
  Y_hat = dataset_transf_test_pred.scores

  return Y_hat
