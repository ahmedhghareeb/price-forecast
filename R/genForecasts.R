# Name: Generate forecasts
# 
# Description: Script generates forecasts using daily weather data files and
# model selected from testModels.R.
# 
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(caret)

fcstStartDay = today()

#### Load data ================================================================
load("./cache/finalModel.RData")

weatherFcst <- read.csv(paste0("./data/FcstWeather/",
                               strftime(fcstStartDay - days(1), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = ymd_hms(available_date),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date)
locations = read.csv("./data/HistWeather/locations.csv", stringsAsFactors = FALSE)

# TOOD: Load holidays



#### Add features to forecast data ============================================
# Get averages for weather data
weatherMean = inner_join(weatherFcst, locations) %>% 
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



# Add calendar variables to weatherMean dataframe. Period of day, day of week,
# weekends, month, season, etc.
weatherMean <- weatherMean %>% 
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
weatherMean = weatherMean %>%
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
                                   S_temperature_l2)
  )


#### Predict forecasts ========================================================
weatherMean$predictions <- predict(finalModel, newdata = weatherMean)

weatherMean <- weatherMean %>% 
  filter(as.Date(ts) <= fcstStartDay + days(4))

ggplot(weatherMean, aes(x=ts, y=predictions)) + geom_line()
ggplot(weatherMean, aes(x=ts)) + 
  geom_line(aes(y=S_temperature), colour="blue") + 
  geom_line(aes(y=P_temperature), colour="red")


#### Output ===================================================================
outputForecasts <- data.frame(
  forecaster = "1639e1334b2ec40805be8dedc132764d",
  availabledate = strftime(fcstStartDay, "%d-%m-%Y"),
  predictiondate = strftime(weatherMean$ts, "%d-%m-%Y"),
  hour = hour(weatherMean$ts),
  value = weatherMean$predictions
)

dir.create("./cache", F, T)
filename = paste0("1639e1334b2ec40805be8dedc132764d_",
                  strftime(fcstStartDay, "%Y-%m-%d"))
write.csv(outputForecasts, file = paste0("./cache/", filename, ".csv"),
          row.names=F, quote=F)
