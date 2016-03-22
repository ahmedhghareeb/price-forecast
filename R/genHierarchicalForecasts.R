# Name: Generate hierarchical forecasts
# 
# Description: Script generates forecasts using daily weather data files and
# hierarchical model produced by testModels_heirrchical.R.
# 
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(caret)

subDate = ymd("2016-03-22", tz="CET")

#### Load data ================================================================
load("./cache/HierarchicalModel.RData")

# TODO: I may have been using the wrong weather file!!!! Should not have
# -days(1). Unfortunately, this means that I won't have a weather fcst value for
# hour 23 on the first forecast day, so need to use previous day's forecast to
# fill in weather for this hour.
weatherFcst <- read.csv(paste0("./data/FcstWeather/",
                               strftime(subDate - days(1), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = ymd_hms(available_date),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date)

# TODO: Not convinced that I am doing the lag properly. Might be out by one day.
# Double check when I have a clear mind or eating spaghetti.

# Load last week of prices for lagged price variable
pricesLastWeek <- NULL
for(i in 6:0) {
  priceDate = subDate-days(i)
  priceFileName <- paste0("INT_PBC_EV_H_1_",
                          strftime(priceDate, "%d_%m_%Y_"),
                          strftime(priceDate, "%d_%m_%Y"),
                          ".txt")
  price_tmp <- read.csv(paste0("./data/PricesLastWeek/", priceFileName),
                        skip=2, sep=";") %>% 
    slice(1) %>%
    select(-X) %>% 
    gather(Hour, Price) %>% 
    na.omit() %>% 
    mutate(Hour = as.numeric(str_extract(Hour, "[[:digit:]]+")) - 1,
           Price = as.numeric(str_replace(Price, ",", ".")),
           ts = priceDate - days(1) + hours(Hour))
  pricesLastWeek = bind_rows(pricesLastWeek, price_tmp)
}
pricesLagged = pricesLastWeek %>% 
  select(ts, Price_l168 = Price) %>% 
  mutate(ts = ts + days(7))
rm(pricesLastWeek)

# TOOD: Load holidays



#### Add features to forecast data ============================================
# Get averages for weather data
weather = weatherFcst %>% 
  group_by(ts) %>% 
  summarise(temperature_mean = mean(temperature, na.rm=TRUE),
            wind_speed_100m_mean = mean(wind_speed_100m, na.rm=TRUE),
            pressure_mean = mean(pressure, na.rm=TRUE),
            precipitation_mean = mean(precipitation, na.rm=TRUE),
            temperature_sd = sd(temperature, na.rm=TRUE),
            wind_speed_100m_sd = sd(wind_speed_100m, na.rm=TRUE),
            pressure_sd = sd(pressure, na.rm=TRUE),
            precipitation_sd = sd(precipitation, na.rm=TRUE),
            temperature_diff = max(temperature, na.rm=TRUE) - 
              min(temperature, na.rm=TRUE),
            wind_speed_100m_diff = max(wind_speed_100m, na.rm=TRUE) -
              min(wind_speed_100m, na.rm=TRUE),
            pressure_diff = max(pressure, na.rm=TRUE) -
              min(pressure, na.rm=TRUE),
            precipitation_diff = max(precipitation, na.rm=TRUE) - 
              min(precipitation, na.rm=TRUE)) %>% 
  gather(WeatherVar, Measurement, -c(ts)) %>% 
  unite(temp, WeatherVar) %>% 
  spread(temp, Measurement) %>% 
  ungroup()

# Add calendar variables to price dataframe. Period of day, day of week,
# weekends, month, season, etc.
weather <- weather %>% 
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
weather = weather %>%
  mutate(temperature_mean_l1 = lag(temperature_mean, 1),
         temperature_mean_l1 = ifelse(is.na(temperature_mean_l1), temperature_mean, 
                                      temperature_mean_l1),
         temperature_mean_l2 = lag(temperature_mean, 2),
         temperature_mean_l2 = ifelse(is.na(temperature_mean_l2), temperature_mean, 
                                      temperature_mean_l2),
         temperature_mean_l24 = lag(temperature_mean, 24),
         temperature_mean_l24 = ifelse(is.na(temperature_mean_l2), temperature_mean, 
                                       temperature_mean_l2)
         )


# Join lagged prices
weather <- inner_join(weather, pricesLagged)


#### Predict forecasts ========================================================
price_pred <- NULL
for (i in 0:23) {
  predictions <- predict(finalModel[[i+1]], newdata = filter(weather, Hour==i))
  price_tmp <- weather %>% 
    filter(Hour==i) %>% 
    mutate(predictions = predictions)
  
  price_pred <- bind_rows(price_pred, price_tmp)
}
price_pred <- price_pred %>% 
  arrange(ts)


#### UTC adjustment for CUT =================================================== 
#It seems as though I should be forecasting from 00:00 CET onwards (for 5 days),
#but everything so far has been done in UTC
price_pred <- price_pred %>% 
  mutate(ts = with_tz(ts, tz="CET")) %>% 
  select(-c(Date, Hour, DoY, DoW, DoW2, Weekend, Year, Month)) %>% 
  filter(ts >= subDate + days(1),
         ts < subDate + days(6))


# Can now convert back to UTC
price_pred <- price_pred %>% 
  mutate(ts = with_tz(ts, tz="UTC"))


#### Plots ====================================================================
ggplot(price_pred, aes(x=ts, y=predictions)) + geom_line()



#### Output ===================================================================
outputForecasts <- data.frame(
  forecaster = "1639e1334b2ec40805be8dedc132764d",
  availabledate = strftime(subDate, "%d-%m-%Y"),
  predictiondate = strftime(price_pred$ts, "%d-%m-%Y", tz="UTC"),
  hour = hour(price_pred$ts),
  value = price_pred$predictions
)

dir.create("./cache", F, T)
filename = paste0("1639e1334b2ec40805be8dedc132764d_",
                  strftime(subDate, "%Y-%m-%d"))
write.csv(outputForecasts, file = paste0("./cache/", filename, ".csv"),
          row.names=F, quote=F)
