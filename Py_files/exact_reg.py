'''
#python for  ML first
#numpy
#scipy
#matplotlib.pyplot
#pandas
#scikit-learn
'''
import numpy 
import scipy
class RegressionLinearExact:
  def __init__(self, with_bias = 'yes'):
    self.coef = None
    self.with_bias = with_bias
#
  def qr_solver(self,X,y):
    if self.with_bias=='yes':
# 
      n_line = X.shape[0]
      one_column = numpy.ones((n_line,1))
      Xt = numpy.hstack((one_column,X))
#
      q, r = numpy.linalg.qr(Xt)
      r_inv = numpy.linalg.inv(r)
      return r_inv @ q.T @ y 
    else:
      q, r = numpy.linalg.qr(X)
      r_inv = numpy.linalg.inv(r)
      return r_inv @ q.T @ y 
  def sdv_solver(self,X,y):
    if self.with_bias == 'yes':
      n_line = X.shape[0]
      one_column = numpy.ones((n_line,1))
      xt = numpy.hstack((one_column,X))
      u,d,vt = numpy.linalg.svd(Xt, full_matrices=False)
      return ((vt.T)@numpy.linalg.inv(numpy.diag(d)))@(u.T@y)
    else:
      u,d,vt = numpy.linalg.svd(X, full_matrices=False)
      d     = numpy.linalg.inv(numpy.diag(d))
      return (((vt.T)@d)@u.T)@y
  def fit(self, X, y, fit_method = 'qr'):
    if fit_method == 'qr':
      self.coef = self.qr_solver(X,y)
    else:
      self.coef = self.sdv_solver(X,y)
  def predict(self, X):
    self.coef = numpy.expand_dims(self.coef,axis=1)
    n_param = self.coef.shape[0]
    n_X, n_y = X.shape[0], X.shape[1]
    if (self.with_bias != 'yes'and n_param == n_y):
      return numpy.dot(X,self.coef)
    elif (self.with_bias == 'yes' and n_param == (n_y+1)):
      X = numpy.hstack((numpy.ones((n_X,1)),X))
      return X.dot(self.coef) 
    else:
      print(f'mismatch in data {n_param} = {n_y + 1}')
      return None

