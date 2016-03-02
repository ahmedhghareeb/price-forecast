"""testModels.py: Trying to fit some models to the weather and price data.
Also calculating some evaluation metrics. Trying to plot using pandas plot
functionality.
"""
__author__ = "Cameron Roach"


import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
plt.style.use('ggplot')

os.chdir("/Users/cameronroach/Documents/PyCharm Projects/PriceForecast/tests")


#region Load and transform data
# Load data with pandas. Easier than csv module.
pricePT = pd.read_csv('../data/HistData/price_PT.csv', header=0)
pricePT.rename(columns = {'value':'price'}, inplace=True)
weather = pd.read_csv('../data/HistWeather/weather_hist.csv')
weather.rename(columns = {'prediction_date':'date'}, inplace=True)
locations = pd.read_csv('../data/HistWeather/locations.csv')

# Convert date columns to datetime type.
pricePT['date'] = pricePT['date'].apply(pd.to_datetime)
weather['date'] = weather['date'].apply(pd.to_datetime)


# Group weather stations in same countries and take simple average of
# temperatures, wind speeds, etc.
weatherMean = pd.merge(weather, locations)
#weather.query("Country=='Spain'")
weatherMean = (
    #weatherMean.groupby(['date', 'Country'], as_index=False)
    weatherMean.groupby(['date', 'Country'])
    [['temperature']]
    .mean()
    #.reset_index() #ungroups
    .unstack() #Used for MultiIndex. Similar to cast.
)
#sns.lmplot(x="date", y="temperature", col="Country", data=weatherMean)


# weatherMean currently has a multiIndex (temperature and country. Need to
# convert to single index so that merge can work.
weatherMean.columns = ['_'.join(col).strip() for col in
                       weatherMean.columns.values]
weatherMean = weatherMean.reset_index()


# Merge data frames
price = pd.merge(pricePT, weatherMean)

# Want date as index so that can do the resample function so that NaN's
# appear for missing data.
# TODO: Figure out if it's possible to do this without setting the index - just specify a column instead. Makes more sense that way! See: http://pandas.pydata.org/pandas-docs/stable/timeseries.html#resampling
price = price.set_index('date').resample("60 min").reset_index()

# Add calendar variables to price dataframe. Period of day, day of week,
# weekends, month, season, etc.
price = (price.assign(Year = price['date'].dt.year)
         .assign(Month = price['date'].dt.month)
         .assign(Hour = price['date'].dt.hour)
         .assign(DoW = price['date'].dt.dayofweek) #Monday=0
         .assign(DoY = price['date'].dt.dayofyear))
# TODO: Instead of >=, should really use in. Returns error if using in [5,6]
price = (price.assign(Weekend = np.where(price['DoW'] >= 5, "Weekend",
                                    "Weekday")))
#endregion


#region Plots
# Plot of average temperature and demand for Spain and Portugal
color_dict = {'Weekend':'red', 'Weekday':'blue'}
fig = plt.figure()
ax = fig.add_subplot(1,2,1)
ax.scatter(x=price['temperature_Portugal'], y=price['price'],
           c=[color_dict[i] for i in price["Weekend"]])
ax.set_title(str("Portugal electricity price and temperature"))
ax.set_xlabel("Temperature")
ax.set_ylabel("Price $/MWh")
ax = fig.add_subplot(1,2,2)
ax.scatter(x=price['temperature_Spain'], y=price['price'],
           c=[color_dict[i] for i in price["Weekend"]])
ax.set_title(str("Spain electricity price and temperature"))
ax.set_xlabel("Temperature")
ax.set_ylabel("Price $/MWh")

# This plot is the reason resample had to happen above. Ensures that
# interpolation doesn't happen for missing values because we now have NaN
# values instead.
ax = price.plot(x='date', y=['temperature_Portugal', 'temperature_Spain'],
           subplots=True, sharex=True, title="Average temperatures in Spain "
                                             "and Portugal",
                color="blue")

ax = price.plot(x='date', y='price', title="Electricity price in Portugal")

#This is one way of splitting up the boxplots
ax = price[["DoW", "price"]].groupby("DoW").boxplot()
#But this way is better
# TODO: pivot introduces NaNs because it treats the row number as an index. Figure out a way to get it to ignore that so that there aren't so many NaNs!
ax = price[["DoW", "price"]].pivot(columns="DoW").boxplot()
ax.set_title("Price boxplots for each day of the week (0 = Monday)")

#endregion


#region Fit models

# Remove NaNs and unwanted columns for modelling
price = price.dropna()
price = price.drop(['Year','DoW','DoY'], axis=1)
price.dtypes


#endregion