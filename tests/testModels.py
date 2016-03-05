"""testModels.py: Trying to fit some models to the weather and price data.
Also calculating some evaluation metrics. Trying to plot using pandas plot
functionality.
"""
__author__ = "Cameron Roach"


import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor

plt.style.use("ggplot")

os.chdir("/Users/cameronroach/Documents/PyCharm Projects/PriceForecast/tests")


#region Load and transform data
# Load data with pandas. Easier than csv module.
pricePT = pd.read_csv("../data/HistData/price_PT.csv", header=0)
weather = pd.read_csv("../data/HistWeather/weather_hist.csv")
locations = pd.read_csv("../data/HistWeather/locations.csv")

# Rename columns
pricePT.rename(columns = {"value":"price",
                          "date":"ts"}, inplace=True)
weather.rename(columns = {"prediction_date":"ts"}, inplace=True)


# Convert date columns to datetime type.
pricePT["ts"] = pricePT["ts"].apply(pd.to_datetime)
weather["ts"] = weather["ts"].apply(pd.to_datetime)


# Group weather stations in same countries and take simple average of
# temperatures, wind speeds, etc.
weatherMean = pd.merge(weather, locations)
#weather.query("Country=='Spain'")
# TODO: calculate the difference between the two country averages and maybe avg countries to get one average temp value and one difference between countries value. Only do this if there is strong correlation between the two country average temperatures - CHECK!
weatherMean = (
    #weatherMean.groupby(["ts", "Country"], as_index=False)
    weatherMean.groupby(["ts", "Country"])
    [["temperature"]]
    .mean()
    #.reset_index() #ungroups
    .unstack() #Used for MultiIndex. Similar to cast.
)
#sns.lmplot(x="ts", y="temperature", col="Country", data=weatherMean)


# weatherMean currently has a multiIndex (temperature and country. Need to
# convert to single index so that merge can work.
weatherMean.columns = ["_".join(col).strip() for col in
                       weatherMean.columns.values]
weatherMean = weatherMean.reset_index()


# Merge data frames
price = pd.merge(pricePT, weatherMean)

# Want date as index so that can do the resample function so that NaN's
# appear for missing data.
# TODO: Figure out if it's possible to do this without setting the index - just specify a column instead. Makes more sense that way! See: http://pandas.pydata.org/pandas-docs/stable/timeseries.html#resampling
price = price.set_index("ts").resample("60 min").reset_index()

# Add calendar variables to price dataframe. Period of day, day of week,
# weekends, month, season, etc.
price = (price.assign(Year = price["ts"].dt.year)
         .assign(Month = price["ts"].dt.month)
         .assign(Hour = price["ts"].dt.hour)
         .assign(DoW = price["ts"].dt.dayofweek) #Monday=0
         .assign(DoY = price["ts"].dt.dayofyear)
         .assign(Date = price["ts"].dt.date))
# TODO: Instead of >=, should really use in. Returns error if using in [5,6]
price = (price.assign(Weekend = np.where(price["DoW"] >= 5, "Weekend",
                                    "Weekday")))
#endregion


#region Plots
# Plot of average temperature and demand for Spain and Portugal
color_dict = {"Weekend":"red", "Weekday":"blue"}
fig = plt.figure()
ax = fig.add_subplot(1,2,1)
ax.scatter(x=price["temperature_Portugal"], y=price["price"],
           c=[color_dict[i] for i in price["Weekend"]])
ax.set_title(str("Portugal electricity price and temperature"))
ax.set_xlabel("Temperature")
ax.set_ylabel("Price $/MWh")
ax = fig.add_subplot(1,2,2)
ax.scatter(x=price["temperature_Spain"], y=price["price"],
           c=[color_dict[i] for i in price["Weekend"]])
ax.set_title(str("Spain electricity price and temperature"))
ax.set_xlabel("Temperature")
ax.set_ylabel("Price $/MWh")

# This plot is the reason resample had to happen above. Ensures that
# interpolation doesn't happen for missing values because we now have NaN
# values instead.
ax = price.plot(x="ts", y=["temperature_Portugal", "temperature_Spain"],
           subplots=True, sharex=True, title="Average temperatures in Spain "
                                             "and Portugal",
                color="blue")

ax = price.plot(x="ts", y="price", title="Electricity price in Portugal")

#This is one way of splitting up the boxplots. Looks gross though.
# ax = price[["DoW", "price"]].groupby("DoW").boxplot()
#But this way is better
# TODO: Figure out a way to get it to ignore that so that there aren"t so many NaNs, without requiring index - like tidyr spread
ax = price[["DoW", "price"]].pivot(columns="DoW").boxplot()
ax = price[["Date", "Hour", "price"]].pivot(
        index="Date", columns="Hour").boxplot()

#endregion


#region Fit models

# Remove NaNs and unwanted columns for modelling and put into training data
# dataframe
# TODO: could try a better method for dealing with NaNs, e.g., fill in with the median, but ignoring for the moment. See: https://www.kaggle.com/c/titanic/details/getting-started-with-python-ii
train_data_pd = price.dropna() #df used later when comparing predicted values
train_data = train_data_pd.drop(["Year", "DoW", "DoY", "ts"], axis=1)

#sklearn needs a numpy.array which in turn needs all numeric data. Hence,
# need to convert the Weekend column to a numeric
train_data["Weekend"] = train_data["Weekend"]\
    .map( {"Weekend": 1, "Weekday": 0} )\
    .astype(int)
train_data = train_data.values #numpy.array

# Fit a model!!!! :D :D :D
forest = RandomForestRegressor(n_estimators = 100)
# Driver variables in columns 1 onwards. Response variable in colum zero.
# 0:: means all rows, but could also have just used :
forest = forest.fit(train_data[0::,1::],train_data[0::,0])
print forest.feature_importances_

# TODO: Put together linear regression model




# Compare predictions against actuals
# TODO: This throws a warning. Not the right way to assign numpy.ndarray to pandas data frame.
train_data_pd["price_rf"] = forest.predict(train_data[0::,1::])

train_data_pd.plot(x="ts", y=["price", "price_rf"],
                   title="Price predictions compared to actuals")

#endregion