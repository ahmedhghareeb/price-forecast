# Name: Forecast electricity prices
# 
# Description: This script is a conversion of the testModels.py script for R.
# Additional functionality will be added. Tests models and predicts prices.
#
# Author: Cameron Roach

rm(list=ls())

require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)
require(lubridate)
require(caret)
require(splines)


#### Load data ================================================================
#Load and transform data
pricePT = read.csv("./data/HistData/price_PT.csv", sep=";") %>% 
  rename(ts = `date..UTC.`) %>% 
  mutate(ts = dmy_hm(ts))
weather = read.csv("./data/HistWeather/weather_hist.csv") %>% 
  rename(ts = prediction_date) %>% 
  mutate(ts = dmy_hm(ts))
locations = read.csv("./data/HistWeather/locations.csv")



#### Engineer features ========================================================
# Group weather stations in same countries and take simple average of
# temperatures, wind speeds, etc.
weatherMean = inner_join(weather, locations)
# TODO: calculate the difference between the two country averages and maybe avg countries to get one average temp value and one difference between countries value. Only do this if there is strong correlation between the two country average temperatures - CHECK!
weatherMean = weatherMean %>% 
  group_by(ts, Country) %>% 
  summarise(temperature = mean(temperature, na.rm=TRUE)) %>% 
  spread(Country, temperature) %>% 
  rename(P_temp = Portugal, S_temp = Spain)


# Merge data frames
price = inner_join(pricePT, weatherMean)

# Add calendar variables to price dataframe. Period of day, day of week,
# weekends, month, season, etc.
price <- price %>% 
  mutate(
    Year = year(ts),
    Month = factor(month(ts)),
    Hour = factor(hour(ts)),
    DoW = wday(ts, label=TRUE),
    DoY = yday(ts),
    Date = floor_date(ts, "day"),
    Weekend = ifelse(DoW %in% c("Sun", "Sat"), TRUE, FALSE)
  )

# Add hourly lags for weather variables
# TODO: See how gaps in the data are affecting the lags. Could possibly have
#  a situation where the accuracy introduced by including lags is offset by
# exluding more rows with NaNs. Maybe replace missing lagged values with
# average?
price = price %>% 
  mutate(P_temp_l1 = lag(P_temp, 1),
         P_temp_l2 = lag(P_temp, 2),
         S_temp_l1 = lag(S_temp, 1),
         S_temp_l2 = lag(S_temp, 2)
         )



#### Fit models ===============================================================
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

model_lm <- train(Price ~ P_temp + S_temp + Hour + DoW + ns(DoY, 4),
                  data = price,
                  method="lm",
                  trControl = fitControl
                  )


price <- price %>% 
  mutate(predict_lm = predict(model_lm, newdata = price),
         r_lm = Price - predict_lm)

price %>% 
  filter(month(ts)==4) %>% 
  select(ts, Price, predict_lm, r_lm) %>% 
  gather(var, value, -ts) %>% 
  ggplot(aes(x=ts, y=value, colour=var)) +
  geom_line()