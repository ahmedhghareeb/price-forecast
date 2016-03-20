# Name: Forecast electricity prices
# 
# Description: This script is a conversion of the testModels.py script for R.
# Additional functionality will be added. Tests models and predicts prices.
#
# Author: Cameron Roach

# TODO: Try to split up hours so that each hour has it's own additive model
# TODO: Add public holiday effects.
# TODO: Double check prices are loaded correctly. Worried that day might be out
# by one. Double check when I can think clearly.

rm(list=ls())

require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)
require(lubridate)
require(caret)
require(splines)


#### Functions ================================================================
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- mean(abs(data$obs - data$pred), na.rm=TRUE)
  names(out) <- "MAE"
  out
}

#### Load data ================================================================
#Load and transform data
pricePT = read.csv("./data/HistData/price_PT.csv", sep=";") %>% 
  rename(ts = `date..UTC.`) %>% 
  mutate(ts = dmy_hm(ts))
weather = read.csv("./data/HistWeather/weather_hist.csv") %>% 
  rename(ts = prediction_date) %>% 
  mutate(ts = dmy_hm(ts))
locations = read.csv("./data/HistWeather/locations.csv", stringsAsFactors = FALSE)

#merge(pricePT, weather[weather$point==5,]) %>% select(-ts, -point) %>% pairs()

#### Engineer features ========================================================
# Group weather stations in same countries and take simple average of
# temperatures, wind speeds, etc.
weatherMean = inner_join(weather, locations) %>% 
  mutate(Country = ifelse(Country=="Portugal", "P", Country),
         Country = ifelse(Country=="Spain", "S", Country))
# TODO: calculate the difference between the two country averages and maybe avg countries to get one average temp value and one difference between countries value. Only do this if there is strong correlation between the two country average temperatures - CHECK!
weatherMean = weatherMean %>% 
  group_by(ts, Country) %>% 
  summarise(temperature = mean(temperature, na.rm=TRUE),
            wind_speed_100m = mean(wind_speed_100m, na.rm=TRUE),
            #air_density = mean(air_density, na.rm=TRUE), # removed because strongly correlated with temperature (see pairs plot)
            pressure = mean(pressure, na.rm=TRUE),
            precipitation = mean(precipitation, na.rm=TRUE)) %>% 
  gather(WeatherVar, Measurement, -c(ts, Country)) %>% 
  unite(temp, Country, WeatherVar) %>% 
  spread(temp, Measurement) %>% 
  ungroup()

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
    Weekend = ifelse(DoW %in% c("Sun", "Sat"), TRUE, FALSE),
    DoW2 = ifelse(Weekend == TRUE, DoW, "Weekday"),
    DoY = yday(ts),
    Date = floor_date(ts, "day")
  )

# Add hourly lags for weather variables
# TODO: Need to add all the NAs in so that lags work properly. Just getting last
# value rather than last time period.
price = price %>%
  mutate(P_temperature_l1 = lag(P_temperature, 1),
         P_temperature_l1 = ifelse(is.na(P_temperature_l1), P_temperature, 
                                   P_temperature_l1),
         P_temperature_l2 = lag(P_temperature, 2),
         P_temperature_l2 = ifelse(is.na(P_temperature_l2), P_temperature, 
                                   P_temperature_l2),
         S_temperature_l1 = lag(S_temperature, 1),
         S_temperature_l1 = ifelse(is.na(S_temperature_l1), S_temperature, 
                                   S_temperature_l1),
         S_temperature_l2 = lag(S_temperature, 2),
         S_temperature_l2 = ifelse(is.na(S_temperature_l2), S_temperature, 
                                   S_temperature_l2),
         # Price_l24 = lag(Price, 24),
         # Price_l24 = ifelse(is.na(Price_l24), Price, 
         #                    Price_l24),
         # Price_l48 = lag(Price, 48),
         # Price_l48 = ifelse(is.na(Price_l48), Price, 
         #                    Price_l48),
         Price_l168 = lag(Price, 168),
         Price_l168 = ifelse(is.na(Price_l168), Price, 
                             Price_l168)
  )


#### Plots ====================================================================
price %>% 
  ggplot(aes(x=DoY, y=Price, colour=Hour)) + 
  geom_point() +
  geom_smooth(method="lm", formula=y~ns(x, 4), se=F) +
  ggtitle("Seasonality in prices for each hour of the day.")

price %>% 
  select(Price, Hour, P_temperature, S_temperature) %>% 
  gather(Country, Temperature, -c(Price, Hour)) %>% 
  ggplot(aes(x=Temperature, y=Price, colour=Hour)) + 
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", formula=y~ns(x, 4), se=F) +
  facet_wrap(~Country) +
  ggtitle("Relationship between price and temperature for each hour of the day.")

price %>% 
  select(Price, P_temperature, S_temperature, Weekend) %>% 
  gather(Country, Temperature, -c(Price, Weekend)) %>% 
  ggplot(aes(x=Temperature, y=Price, colour=Weekend)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", formula=y~ns(x,3)) +
  facet_wrap(~Country) +
  ggtitle("Relationship between price and temperature for weekends and weekdays.")

price %>% 
  ggplot(aes(x=DoW, y=Price)) + 
  geom_boxplot()

# Check relationship between lagged temperatures and price
price %>% 
  select(Price, P_temperature_l1, P_temperature_l2, S_temperature_l1, S_temperature_l2) %>% 
  gather(Lag, Temperature, -Price) %>% 
  ggplot(aes(x=Temperature, y=Price)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Lag)


#### Fit models ===============================================================
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 10,
#   repeats = 10,
#   summaryFunction = maeSummary)
fitControl <- trainControl(
  method = "timeslice",
  initialWindow = 150,
  horizon=14,
  summaryFunction = maeSummary)

# Linear models
model_lmBL <- train(Price ~ Price_l168,
                    data = price,
                    method="lm",
                    metric="MAE",
                    trControl = fitControl
)
summary(model_lmBL)
model_lmBL


model_lm1 <- train(Price ~ ns(P_temperature, 3) + ns(S_temperature, 3) + 
                     Hour + DoW2 + ns(DoY, 4),
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm1)
model_lm1

model_lm2 <- train(Price ~ ns(P_temperature, 3) + ns(S_temperature, 3) + 
                     Hour + DoW2 + ns(DoY, 4) + P_temperature_l1 + 
                     P_temperature_l2 + S_temperature_l1 + S_temperature_l2,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm2)
model_lm2

model_lm3 <- train(Price ~ ns(P_temperature, 3) + ns(S_temperature, 3) + 
                     Hour + DoW2 + ns(DoY, 4) + P_temperature_l1 + 
                     P_temperature_l2 + S_temperature_l1 + S_temperature_l2 +
                     P_precipitation + P_pressure + P_wind_speed_100m + 
                     S_precipitation + S_pressure + S_wind_speed_100m,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm3)
model_lm3


model_lm4 <- train(Price ~ Price_l168 + DoW2 + ns(DoY, 4) + Hour,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm4)
model_lm4

#Note: removed S_precipitation because this is the same as P_precipitation for
#all times.
# TODO: Check why this has happened. Same for all weather stations?
model_lm5 <- train(Price ~ ns(P_temperature, 3) + ns(S_temperature, 3) + 
                     Hour + DoW2 + ns(DoY, 4) + P_temperature_l1 + 
                     P_temperature_l2 + S_temperature_l1 + S_temperature_l2 +
                     P_precipitation + P_pressure + P_wind_speed_100m + 
                     S_pressure + S_wind_speed_100m + Price_l168,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm5)
model_lm5


model_lm6 <- train(Price ~ Price_l168 + DoW2 + Hour,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm6)
model_lm6

# TODO:try adding ns(DoY, 4), got an improved result with Month driver included
model_lm7 <- train(Price ~ Price_l168 + DoW2 + Hour + S_wind_speed_100m,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm7)
model_lm7

# Interesting comparison to lm7. Including Portugal wind speed makes forecast
# worse. TODO: Check if an average of all weather stations in portugal and spain
# improves MAE even more.
model_lm7b <- train(Price ~ Price_l168 + DoW2 + Hour + P_wind_speed_100m +
                     S_wind_speed_100m,
                   data = price,
                   method="lm",
                   metric="MAE",
                   trControl = fitControl
)
summary(model_lm7)
model_lm7


# model_rf <- train(Price ~ P_temperature + S_temperature + Hour + DoW2 + Month,
#                   data = price %>% sample_n(1000),
#                   method="rf")


#### Evaluation metrics =======================================================
price <- price %>% 
  mutate(Price_lm1 = predict(model_lm1, newdata = price),
         r_lm1 = Price - Price_lm1,
         Price_lm2 = predict(model_lm2, newdata = price),
         r_lm2 = Price - Price_lm2,
         Price_lm3 = predict(model_lm3, newdata = price),
         r_lm3 = Price - Price_lm3,
         Price_lm4 = predict(model_lm4, newdata = price),
         r_lm4 = Price - Price_lm4,
         Price_lm5 = predict(model_lm5, newdata = price),
         r_lm5 = Price - Price_lm5,
         Price_lm6 = predict(model_lm6, newdata = price),
         r_lm6 = Price - Price_lm6,
         Price_lm7 = predict(model_lm7, newdata = price),
         r_lm7 = Price - Price_lm7)

for(i in 1:12) {
  p <- price %>% 
    filter(month(ts)==i) %>% 
    select(ts, Price, Price_lm7, r_lm7) %>% 
    gather(var, value, -ts) %>% 
    ggplot(aes(x=ts, y=value, colour=var)) +
    geom_line() +
    ggtitle(paste("Price actuals and predictions for", month.name[i], "2015"))
  print(p)
}



#### Choose final model and save ==============================================
finalModel <- model_lm7

dir.create("./cache", F, T)
save(finalModel, file="./cache/finalModel.RData")
