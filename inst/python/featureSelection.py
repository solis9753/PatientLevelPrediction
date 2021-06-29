#  Feature selection
#===============================================================
# INPUT:
# 1) location of files: libsvm file + indexes file (rowId, index)
# 2) 
#
# OUTPUT:
# it returns a file with indexes merged with prediction for test index  
#================================================================
import numpy as np
#from collections import OrderedDict
import os
import sys
import timeit
import math
from scipy.sparse import coo_matrix,csr_matrix,vstack,hstack
#from sklearn.feature_selection import SelectFromModel#from sklearn.cross_validation import PredefinedSplit
#from sklearn.datasets import load_svmlight_file
import joblib
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2

#================================================================
def univariate_feature_selection(population, plpData, variableNumber, quiet):
  print("Performing univariate feature selection " )
  y = population[:,1]
  X = plpData[population[:,0].astype(int),:]
  print("population loaded- %s rows and %s columns" %(np.shape(population)[0], np.shape(population)[1]))
  print("Dataset has %s rows and %s columns" %(X.shape[0], X.shape[1]))
  ###########################################################################
  featnum = min(variableNumber,X.shape[1])
  print("Applying univariate feature selection to select %s features as algorithm requires non-sparse data " %(featnum))
  kbest = SelectKBest(chi2, k=featnum).fit(X[population[:,population.shape[1]-1] > 0,:], y[population[:,population.shape[1]-1] > 0])
  kbest.scores_ = np.nan_to_num(kbest.scores_)
  print("Test kbest length: %s non-zero: %s" %(kbest.scores_.shape[0], np.sum(kbest.scores_!=0)))
  threshold = -np.sort(-kbest.scores_)[featnum-1]
  print("Threshold varImp set at %s" %(threshold))
  X = X[population[:,population.shape[1]-1] > 0, :]
  print("Test X dim: %s , %s" %(X.shape[0], X.shape[1]) )
  X = X[:,kbest.scores_ >=threshold]
  print("Test X dim: %s , %s" %(X.shape[0], X.shape[1]) )
  X = X.toarray()
  y = y[population[:,population.shape[1]-1] > 0]
  ppopulation = population[population[:,population.shape[1]-1] > 0,:]
  
  return X, y, ppopulation, kbest.scores_;
