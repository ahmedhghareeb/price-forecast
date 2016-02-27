# Title: Explore Data
# Description: A little script to figure out how to use python and also what the data looks like.
__author__ = "Cameron Roach"

#import csv as csv
import pandas as pd
import numpy as np
import pylab as P
import matplotlib as plt
from datetime import datetime


# Load data with pandas. Easier than csv module.
genES = pd.read_csv('../data/HistData/gen_ES.csv', header=0, na_values='n.a.', parse_dates=True)
genES.rename(columns = {'data':'date'}, inplace=True)
genPT = pd.read_csv('../data/HistData/gen_PT.csv', header=0)
pricePT = pd.read_csv('../data/HistData/price_PT.csv', header=0)
weather = pd.read_csv('../data/HistWeather/weather_hist.csv')





# Check import
print genES.info()
print genES.describe()
print genPT.info()
print genPT.describe()
print pricePT.info()
print pricePT.describe()



# Investigate nulls
genES[genES['solar_photovoltaic'].isnull()]


# Plots
genES['solar_photovoltaic'].dropna().hist(bins=16, range=(0,80), alpha = .5)
genES.plot(x = 'date', y ='solar_photovoltaic')
#genES.boxplot()
#genES.boxplot(column='solar_photovoltaic')

P.show()