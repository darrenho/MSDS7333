# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 11:06:06 2016

@author: darrenho
"""
#from datetime import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from urllib import urlretrieve
localDir = '/Users/darrenho/Box Sync/teaching/MSDS7333/Units7and8/code/'
outDict  = dict()
import re
for ticker in ['^HSI','^SSMI', 'AAPL','^GSPC','^TYX']:
    print ticker
    urlretrieve(  
#    http://chart.finance.yahoo.com/table.csv?s=^HSI&a=8&b=19&c=2016&d=9&e=19&f=2016&g=d&ignore=.csv
    'http://real-chart.finance.yahoo.com/table.csv?s='+ticker+'&a=10&b=21&c=2015&d=10&b=20&c=2016',
    localDir+ticker+'.csv'
    )
    pd_tmp = pd.read_csv(localDir+ticker+'.csv', parse_dates=True, index_col=0)    
    date   = pd_tmp.index
    ticker = re.sub('\^','',ticker)
    outDict[ticker] = pd_tmp['Adj Close']

close_px = pd.DataFrame(outDict,index=date)
close_px = close_px.resample('B').ffill()
close_px = close_px.fillna(method='ffill')

###
# Write a 
###
from sklearn import preprocessing

GSPC = close_px.GSPC
del close_px['GSPC']


class backFit:
    def __init__(self,X,Y):
        import numpy as np
        import statsmodels.api as sm
        n,p      = X.shape
        self.old = np.zeros((n,p))
        self.fit = np.zeros((n,p))        
        for j in xrange(p):
            fit = sm.nonparametric.KernelReg(Y,X[:,j],var_type = 'c',bw='aic')
            self.fit[:,j] = fit.fit()[0]
    def smooth(self,X,R,j):
        import statsmodels.api as sm
        fit           = sm.nonparametric.KernelReg(R,X[:,j],var_type = 'c',bw='aic')
        self.fit[:,j] = fit.fit()[0] #.fit() return a tuple with "mean" & "marginal effects 
    def residuals(self,Y,j):
        return Y - Y.mean() - np.delete(self.fit,j,axis=1).sum(1)
    def converged(self,tolerance,verbose=0):
        if(verbose > 0):
            print np.max(abs(self.old-self.fit))/Y.std()
        return np.max(abs(self.old-self.fit))/Y.std() < tolerance


X        = close_px.values
X_scaled = preprocessing.scale(X)
n,p      = X_scaled.shape        
Y        = GSPC - GSPC.mean()


fHat       = backFit(X_scaled,Y)
initialFit = fHat.fit.copy()
tolerance  = 1e-1
converged  = False
maxIter    = 10
sweeps     = 0
while not converged and sweeps < maxIter:
    for j in xrange(p):
        print j
        #Get residuals
        #get smoothed
    converged = fHat.converged(tolerance,verbose=1)    
    fHat.old = fHat.fit.copy()
    sweeps += 1


fig = plt.figure()
for j in xrange(p):
    ax = fig.add_subplot(2, 2, j+1)
    ax.plot(X[:,j],Y+GSPC.mean(),'b.')
    ax.plot(X[:,j],initialFit[:,j]+GSPC.mean(),'r.')
    ax.plot(X[:,j],fHat.fit[:,j]+GSPC.mean(),'g.')
    ax.set_title(close_px.keys()[j])
