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

fcstStartDay = ymd(today(), tz="CET")

#### Load data ================================================================
load("./cache/finalModel.RData")

weatherFcst <- read.csv(paste0("./data/FcstWeather/",
                               strftime(fcstStartDay - days(1), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = ymd_hms(available_date),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date)
locations = read.csv("./data/HistWeather/locations.csv", stringsAsFactors = FALSE)

# Load last week of prices for lagged price variable
pricesLastWeek <- NULL
for(i in 6:0) {
  priceDate = fcstStartDay-days(i)
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
           ts = priceDate + hours(Hour))
  pricesLastWeek = bind_rows(pricesLastWeek, price_tmp)
}
pricesLagged = pricesLastWeek %>% 
  select(ts, Price_l168 = Price) %>% 
  mutate(ts = ts + days(7))
rm(pricesLastWeek)

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

# Join lagged prices
weatherMean <- inner_join(weatherMean, pricesLagged)


#### Predict forecasts ========================================================
weatherMean$predictions <- predict(finalModel, newdata = weatherMean)



#### UTC adjustment for CUT =================================================== 
#It seems as though I should be forecasting from 00:00 CET onwards (for 5 days),
#but everything so far has been done in UTC
weatherMean <- weatherMean %>% 
  mutate(ts = with_tz(ts, tz="CET")) %>% 
  select(-c(Date, Hour, DoY, DoW, DoW2, Weekend, Year, Month)) %>% 
  filter(ts >= fcstStartDay + days(1),
         ts < fcstStartDay + days(6))

# # Add an extra row which is just a duplicate of the first prediction for earlier
# # hours than weather forecasts
# missingFcstHour <- weatherMean[1,]
# weatherMean <- bind_rows(
#   mutate(missingFcstHour, ts = ts-hours(1)),
#   weatherMean
# )

# Can now convert back to UTC
weatherMean <- weatherMean %>% 
  mutate(ts = with_tz(ts, tz="UTC"))


#### Plots ====================================================================
ggplot(weatherMean, aes(x=ts)) + 
  geom_line(aes(y=S_temperature), colour="blue") + 
  geom_line(aes(y=P_temperature), colour="red")
ggplot(weatherMean, aes(x=ts, y=predictions)) + geom_line()



#### Output ===================================================================
outputForecasts <- data.frame(
  forecaster = "1639e1334b2ec40805be8dedc132764d",
  availabledate = strftime(fcstStartDay, "%d-%m-%Y"),
  predictiondate = strftime(weatherMean$ts, "%d-%m-%Y", tz="UTC"),
  hour = hour(weatherMean$ts),
  value = weatherMean$predictions
)

dir.create("./cache", F, T)
filename = paste0("1639e1334b2ec40805be8dedc132764d_",
                  strftime(fcstStartDay, "%Y-%m-%d"))
write.csv(outputForecasts, file = paste0("./cache/", filename, ".csv"),
          row.names=F, quote=F)
