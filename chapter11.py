# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 11:06:06 2016

@author: darrenho
"""
from datetime import datetime
import pandas as pd
import numpy as np
from pandas_datareader import wb as web
from pandas_datareader import data as pd_data
import matplotlib.pyplot as plt

#From Chapter 10 stuff
from urllib import urlretrieve
localDir = '/Users/darrenho/Box Sync/teaching/MSDS7333/Units3and4/data/'
outDict  = dict()
import re
for ticker in ['AAPL', 'MSFT', 'XOM','^GSPC']:
    urlretrieve(  
    'http://real-chart.finance.yahoo.com/table.csv?s='+ticker+'&d=1&e=12&f=2016&g=d&a=0&b=29&c=1993',
    localDir+ticker+'.csv'
    )
    pd_tmp = pd.read_csv(localDir+ticker+'.csv', parse_dates=True, index_col=0)    
    date   = pd_tmp.index
    ticker = re.sub('\^','',ticker)
    outDict[ticker] = pd_tmp['Adj Close']

close_px = pd.DataFrame(outDict,index=date)
close_px = close_px.resample('B').ffill()


aapl_px = close_px.AAPL['2005':'2009']
aapl_px.plot()

#Deprecated
ma60tmp = pd.rolling_mean(aapl_px,window=60,min_periods=50,center=True)
ma60tmp.plot()
#Using the methods in the class "pandas.core.series.Series")
ma60    = aapl_px.rolling(window=60,min_periods=50,
                       center=True,win_type='boxcar').mean()
ma60.plot()



ma60    = aapl_px.rolling(window=60,min_periods=50,
                          win_type='boxcar').mean()

ewma60    = aapl_px.ewm(span=60).mean()

ewmPlotF = lambda span: aapl_px.ewm(span=span).mean()

fig,axes = plt.subplots(nrows=2,ncols=1,
                        sharex=True,sharey=True,figsize=(12,7))
aapl_px.plot(style='k-',ax=axes[0])
aapl_px.plot(style='k-',ax=axes[1])
ma60.plot(style='r--',ax=axes[0])
ewma60.plot(style='r--',ax=axes[1])
ewmPlotF(30).plot(style='r-.',ax=axes[1])
ewmPlotF(120).plot(style='r:',ax=axes[1])
axes[0].set_title('Simple Moving Average')
axes[1].set_title('Exponentially-weighted Moving Average')


#rolling correlation
spx_px = close_px.GSPC['2005':'2009']
spx_rets = spx_px/spx_px.shift(1)-1
returns = close_px['2005':'2009'].pct_change()
#corr_spxaapl = returns.AAPL.rolling(window=125,min_periods=100,
#                          win_type='boxcar').aggregate('np.corrcoef',returns.GSPC)

corr = pd.rolling_corr(returns.GSPC,returns.AAPL,window=125,min_periods=100)

corr.plot()

#correlation of many stocks at once:
returns = close_px['2005':'2009'].pct_change()
corr = pd.rolling_corr(returns,returns.GSPC,window=125,min_periods=100)
corr.plot()

#Over entire window:
returns = close_px.pct_change()
corr = pd.rolling_corr(returns,returns.GSPC,window=125,min_periods=100)
corr.plot()

from scipy.stats import percentileofscore
score_at_2percent = lambda x: percentileofscore(x,0.02)

result = pd.rolling_apply(returns.AAPL,250,score_at_2percent)
result.plot()


