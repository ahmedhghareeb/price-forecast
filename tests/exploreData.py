# Title: Explore Data
# Description: A little script to figure out how to use python and also what the data looks like.
__author__ = "Cameron Roach"

#import csv as csv
import pandas as pd
import numpy as np
import pylab as P
import matplotlib.pyplot as plt
plt.style.use('ggplot')
from datetime import datetime


# Load data with pandas. Easier than csv module.
genES = pd.read_csv('../data/HistData/gen_ES.csv', header=0, na_values='n.a.',
                    parse_dates=True)
genES.rename(columns = {'data':'date'}, inplace=True)
genPT = pd.read_csv('../data/HistData/gen_PT.csv', header=0)
pricePT = pd.read_csv('../data/HistData/price_PT.csv', header=0)
pricePT.rename(columns = {'value':'price'}, inplace=True)
weather = pd.read_csv('../data/HistWeather/weather_hist.csv')
weather.rename(columns = {'prediction_date':'date'}, inplace=True)



# Sort out date.time types. Slow. Probably a better (but less user-friendly) way.
genES['date'] = genES['date'].apply(pd.to_datetime)
genPT['date'] = genPT['date'].apply(pd.to_datetime)
pricePT['date'] = pricePT['date'].apply(pd.to_datetime)
weather['date'] = weather['date'].apply(pd.to_datetime)




# Check import
print genES.info()
print genES.describe()
print genPT.info()
print genPT.describe()
print pricePT.info()
print pricePT.describe()



# Investigate nulls
genES[genES['solar_photovoltaic'].isnull()]



# Can query data frames
weather.query("point==1")

# Plots
#genES['solar_photovoltaic'].dropna().hist(bins=16, range=(0,80), alpha = .5)
#genES.plot(x = 'date', y ='solar_photovoltaic')
#genES.boxplot()
#genES.boxplot(column='solar_photovoltaic')


#pd.tools.plotting.scatter_matrix(
#        genES.sample(10).drop(['date', 'real_demand', 'forecast_demand',
#                                'programmed_demand'], axis=1),
#        alpha=0.2)
pd.tools.plotting.scatter_matrix(
        genPT.sample(10).drop(['id', 'date', 'demand'], axis=1),
        alpha=0.2)



P.show()




# Merge price and generation data to check correlations
price_merge=pd.merge(genES, pricePT)

idx=(price_merge['date']>='2015-01-01 00:00:00') & \
    (price_merge['date']<'2015-01-02 00:00:00')
price_merge[idx].plot(x='date')


#TODO: Trying to figure out how to get subplots working!!!!
#fig, axes = plt.subplots(nrows=2, ncols=1)
# plt.subplots(2,1)
# price_merge[idx][['date', 'price']].plot(x='date', ax=axes(0,0))
# price_merge[idx].drop('price', axis=1).plot(x='date', ax=axes(1,0))
#

# P.subplot(2, 1, 1)
# P.plot(x=price_merge[idx]['date'], y=price_merge[idx]['price'])
# P.title("Price and demand (MW)")
# P.ylabel("Price")
#
# P.subplot(2, 1, 2)
# P.plot(x=price_merge[idx]['date'], y=price_merge[idx].drop(['date','price'], axis=1))
# P.ylabel("Demand (MW)")



# Scatterplots of price and temperature
weatherPrice = pd.merge(weather, pricePT)
# plt.scatter(x=weatherPrice['temperature'], y=weatherPrice['price'],
#             c=weatherPrice['point'])

for i, group in weatherPrice.groupby('point'):
    fig = plt.figure()
    ax = plt.scatter(x=group['temperature'], y=group['price'])
    fig.suptitle(str("Weather station %d" % i))
    #ax = group.plot(x='temperature', y='price', title=str(i)) # line plot
    #ax.set_title("asdf")


# Better attempt
for i, group in weatherPrice.groupby('point'):
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    ax.scatter(x=group['temperature'], y=group['price'])
    ax.set_title(str("Weather station %d" % i))
    ax.set_xlabel("Temperature")
    ax.set_ylabel("Price $/MWh")
