# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 11:06:06 2016

@author: darrenho
"""
from datetime import datetime
import pandas as pd
import numpy as np

###
# Date and time date types and tools
###

now = datetime.now()
now

now.year, now.month, now.day 

delta = datetime(2011, 1, 7) - datetime(2008, 6, 24, 8, 15)
delta

delta.days
delta.seconds
delta.microseconds


from datetime import timedelta
start = datetime(2011, 1, 7)
start + timedelta(12)
 
start - 2 * timedelta(12)


stamp = datetime(2011, 1, 3)
str(stamp)
stamp.strftime('%Y-%m-%d')

value = '2011-01-03'
datetime.strptime(value, '%Y-%m-%d')

datestrs = ['7/6/2011', '8/6/2011']
dates = [datetime.strptime(x, '%m/%d/%Y') for x in datestrs]

#Third party parser
from dateutil.parser import parse
parse('2011-01-03')
parse('Jan 31, 1997 10:45 PM')
parse('6/12/2011', dayfirst=True)


#Using pandas and datetime

pd.to_datetime(datestrs)
idx = pd.to_datetime(datestrs + [None])


###
# Time series basics (Unit 3.6)
###
dates = [datetime(2011, 1, 2), datetime(2011, 1, 5), datetime(2011, 1, 7), 
         datetime(2011, 1, 8), datetime(2011, 1, 10), datetime(2011, 1, 12)]

ts = pd.Series(np.random.randn(6), index=dates)

ts.index

ts + ts[::2] #take every other entry e.g. range(10)[::2]

stamp = ts.index[2]  #can look at a ts object: dir(ts) and type(ts) 'introspection'

ts[stamp]

longer_ts = pd.Series(np.random.randn(1000), 
                   index=pd.date_range('1/1/2000', periods=1000))

longer_ts['2001'] #freaking cool!
longer_ts['2001-05']
ts[datetime(2011, 1, 7):]

#indexing and slicing
ts['1/6/2011':'1/11/2011'] #doesn't have to be in the index
ts.truncate(after='1/9/2011') #equivalent to slicing

#Investigation of ix, loc, iloc slicing with pandas
long_df = pd.DataFrame(np.random.randn(100, 4), index=range(100),
                    columns=['Colorado', 'Texas', 'New York', 'Ohio'])
long_df[1:3]
long_df.ix[1:3]
long_df.loc[1:3]
long_df.iloc[1:3]
dates = pd.date_range('1/1/2000', periods=100, freq='W-WED')
long_df = pd.DataFrame(np.random.randn(100, 4), index=dates,
                    columns=['Colorado', 'Texas', 'New York', 'Ohio'])
long_df[1:3]
long_df.ix['5-2001']
long_df[dates[1:3]]
long_df.loc[dates[1:3]]

#time series with duplicate indices
dates = pd.DatetimeIndex(['1/1/2000', '1/2/2000', '1/2/2000',
                          '1/2/2000', '1/3/2000'])

dup_ts = pd.Series(np.arange(5), index=dates)
dup_ts.index.is_unique
dup_ts['1/2/2000']


grouped = dup_ts.groupby(by=dates)
grouped = dup_ts.groupby(level=0)
grouped.mean()
grouped.count()

#Date ranges, shifting, frequencies, resampling
dates = [datetime(2011, 1, 2), datetime(2011, 1, 5), datetime(2011, 1, 7), 
         datetime(2011, 1, 8), datetime(2011, 1, 10), datetime(2011, 1, 12)]
ts    = pd.Series(np.random.randn(6), index=dates)
ts.resample('1D') #different from book, note int/str combo
ts.resample('1D').asfreq() #different from book
ts.resample('1D').pad()
ts.resample('1D').bfill()
ts.resample('1D').ffill()

#Generating date ranges
index = pd.date_range('4/1/2012', '6/1/2012')
index = pd.date_range(start='4/1/2012', periods=20)
index = pd.date_range(end='6/1/2012', periods=20)
pd.date_range('1/1/2000', '12/1/2000', freq='BM') #business end of month

# Shifting
ts = pd.Series(np.random.randn(4),
            index=pd.date_range('1/1/2000', periods=4, freq='M'))
ts
ts.shift(2)
lag1 = ts - ts.shift(1)
percChange = ts / ts.shift(1) - 1

ts.shift(2) #vs
ts.shift(2,freq='M') #this keeps last two obs, assigning them to next months

#Periods (note, some book funcs omitted e.g. quarterly)
p = pd.Period(2007, freq='A-DEC')
p + 5

rng = pd.period_range('1/1/2000', '6/30/2000', freq='M')


#Resampling
rng = pd.date_range('1/1/2000', periods=100, freq='D')
ts  = pd.Series(np.random.randn(len(rng)), index=rng)
ts.resample('M', how='mean') #deprecated, use...
ts.resample('M',kind='period').mean()
ts.groupby(lambda x: x.month).mean()
ts.groupby(level=0).mean()

frame = pd.DataFrame(np.random.randn(24, 4), 
                     index=pd.period_range('1-2000', '12-2001', freq='M'),
                     columns=['Colorado', 'Texas', 'New York', 'Ohio'])
annual_frame = frame.resample('A-DEC').mean()


##
# Time series plotting
##
# in book. where is data?
#close_px_all = pd.read_csv('ch09/stock_px.csv', parse_dates=True, index_col=0)
from urllib import urlretrieve
localDir = '/Users/darrenho/Box Sync/teaching/MSDS7333/Units3and4/data/'
outDict  = dict()
for ticker in ['AAPL', 'MSFT', 'XOM']:
    urlretrieve(  
    'http://real-chart.finance.yahoo.com/table.csv?s='+ticker+'&d=1&e=12&f=2016&g=d&a=0&b=29&c=1993',
    localDir+ticker+'.csv'
    )
    pd_tmp = pd.read_csv(localDir+ticker+'.csv', parse_dates=True, index_col=0)    
    date   = pd_tmp.index
    outDict[ticker] = pd_tmp['Adj Close']

close_px = pd.DataFrame(outDict,index=date)
close_px = close_px.resample('B').ffill()
close_px.plot()
close_px.ix['2001'].plot()

appl_q   = close_px['AAPL'].resample('Q-DEC').ffill() #Note, using fill_method is deprecated
appl_q.plot()

close_px.AAPL.ix['2011':].plot()
close_px.AAPL.rolling(window=250,center=False).mean().ix['2011':].plot()
close_px.AAPL.ewm(span=90).mean().ix['2011':].plot()

#book goes over creating beta from CAPM w.r.t. S&P 500 p.g. 325-326