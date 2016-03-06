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
from sklearn import cross_validation

plt.style.use("ggplot")

os.chdir("/Users/cameronroach/Documents/PyCharm Projects/PriceForecast/tests")


#region Load and transform data
# Load data with pandas. Easier than csv module.
pricePT = pd.read_csv("../data/HistData/price_PT.csv", header=0, sep=";")
weather = pd.read_csv("../data/HistWeather/weather_hist.csv")
locations = pd.read_csv("../data/HistWeather/locations.csv")

# Rename columns
pricePT.rename(columns = {"Price":"price",
                          "date (UTC)":"ts"}, inplace=True)
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

# Add hourly lags for weather variables
# TODO: See how gaps in the data are affecting the lags. Could possibly have
#  a situation where the accuracy introduced by including lags is offset by
# exluding more rows with NaNs. Maybe replace missing lagged values with
# average?
price = (price
         .assign(temperature_Portugal_lag1 = price[
    "temperature_Portugal"].shift(1))
         .assign(temperature_Portugal_lag2 = price[
    "temperature_Portugal"].shift(2))
         .assign(temperature_Portugal_lag3 = price[
    "temperature_Portugal"].shift(3))
         .assign(temperature_Spain_lag1 = price[
    "temperature_Spain"].shift(1))
         .assign(temperature_Spain_lag2 = price[
    "temperature_Spain"].shift(2))
         .assign(temperature_Spain_lag3 = price[
    "temperature_Spain"].shift(3)))

# TODO: Add lags for previous day. Same time yesterday or avg temp?

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
all_data = price.dropna().copy() #copy needed or get SettingWithCopyWarning
all_data["Weekend"] = all_data["Weekend"]\
    .map( {"Weekend": 1, "Weekday": 0} )\
    .astype(int) #numpy and sklearn need numerics
rndm_idx = np.random.rand(len(all_data)) < 0.8
train_data = all_data[rndm_idx].copy()
test_data = all_data[~rndm_idx].copy()

x_train = train_data.drop(["Year", "DoW", "DoY", "ts", "Date", "price"],
                          axis=1)
x_test = test_data.drop(["Year", "DoW", "DoY", "ts", "Date", "price"],
                          axis=1)
y_train = train_data["price"]


# Baseline models
baseline_avg = y_train.mean()
# TODO: baseline_naive = last value. Need to do k-fold cross validation for
# this with the folds equal to the size of the forecast horizon (5 days)



# Fit random forest
forest = RandomForestRegressor(n_estimators = 100)
forest_fit = forest.fit(x_train, y_train)
# TODO: setup k-fold cross validation
scores = cross_validation.cross_val_score(forest, x_train, y_train, cv=5)

print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 1.96))
print forest_fit.feature_importances_

# TODO: Put together linear regression model and baseline models
# Fit a regression model



# Compare predictions against actuals
test_data["price_bl_avg"] = baseline_avg
test_data["price_rf"] = forest_fit.predict(x_test)

test_data.plot(x="ts", y=["price", "price_bl_avg", "price_rf"],
                   title="Price predictions compared to actuals")

#endregion



#region Evaluation metrics
test_data["ae_rf"] = abs(test_data["price"]-test_data["price_rf"])
test_data["ape_rf"] = test_data["ae_rf"]/test_data["price"]
test_data["ae_bl_avg"] = abs(test_data["price"]-test_data["price_bl_avg"])
test_data["ape_bl_avg"] = test_data["ae_bl_avg"]/test_data["price"]

test_data.plot(x="ts", y=["ae_rf", "ae_bl_avg"])
test_data.plot(x="ts", y=["ape_rf", "ape_bl_avg"])

plt.scatter(x=test_data["temperature_Portugal"],
            y=test_data["ae_rf"])
plt.scatter(x=test_data["temperature_Portugal"],
            y=test_data["ape_rf"])

#endregion