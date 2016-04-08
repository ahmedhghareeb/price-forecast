#Name: Generate hourly model forecasts
#
#Description: Script generates forecasts using daily weather data files and 
#hourly price models produced by testModels_HourModels.R.
#
#Author: Cameron Roach
#
#TODO: Shift all data loading chunks into functions. Multiple instances of the 
#same bit of code is sometimes causing errors because they aren't all
#updated/consistent.


rm(list=ls())

require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(caret)
require(splines)

subDate <- ymd("2016-04-08", tz="UTC")

#### Run model with latest data ===============================================
source("./R/testModels_HourModels_adjHrs.R")
finalModel <- genHourPriceModel(subDate)

#### Load data ================================================================
# Load yesterday's weather forecast to get hours 23 and 24 for submission date.
weatherFcst <- read.csv(paste0("./data/FcstWeather/",
                               strftime(subDate - days(1), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = ymd_hms(available_date),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date) %>% 
  filter(floor_date(ts, "day") == floor_date(subDate, "day"))
weatherFcst <- read.csv(paste0("./data/FcstWeather/",
                               strftime(subDate, "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = ymd_hms(available_date),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date) %>% 
  bind_rows(weatherFcst)


# Load last week of prices for lagged price variable
pricesLastWeek <- NULL
for(i in 7:0) {
  priceDate = subDate-days(i)
  priceFileName <- paste0("INT_PBC_EV_H_1_",
                          strftime(priceDate, "%d_%m_%Y_"),
                          strftime(priceDate, "%d_%m_%Y"),
                          ".txt")
  price_tmp <- read.csv(paste0("./data/PricesLastWeek/", priceFileName),
                        skip=2, sep=";") %>% 
    slice(2) %>% # 2nd row has portugal prices
    select(-X) %>% 
    gather(Hour, Price) %>% 
    na.omit() %>% 
    mutate(Hour = as.numeric(str_extract(Hour, "[[:digit:]]+")) - 1,
           Price = as.numeric(str_replace(Price, ",", ".")),
           ts = priceDate + hours(Hour)) %>% 
    select(-Hour) %>% 
    #TODO: Hardcoded year as 2016 for DST fix. Create set of days and months
    #instead.
    mutate(ts = ifelse(between(floor_date(ts, "day"), 
                               dmy("28/3/2016"), 
                               dmy("30/10/2016")),
                       ts - hours(2), # convert from CEST to UTC
                       ts - hours(1)), # convert from CET to UTC
           ts = as.POSIXct(ts, origin="1970-01-01", tz="UTC"))
  pricesLastWeek = bind_rows(pricesLastWeek, price_tmp)
}
pricesLagged = pricesLastWeek %>% 
  select(ts, Price_l168 = Price) %>% 
  mutate(ts = ts + days(7))
rm(pricesLastWeek)



holidays <- read.csv("./data/holidays.csv", header = F, 
                     col.names = c("Date", "Date2", "DoW", "Holiday",
                                   "Description", "Country")) %>% 
  mutate(Date = dmy(Date, tz="UTC")) %>% 
  select(Date) %>% 
  distinct()



#### Add features to forecast data ============================================
# Get averages for weather data
weather = weatherFcst %>% 
  group_by(ts) %>% 
  summarise(temperature_mean = mean(temperature, na.rm=TRUE),
            wind_speed_100m_mean = mean(wind_speed_100m, na.rm=TRUE),
            wind_speed_mean = mean(wind_speed, na.rm=TRUE),
            pressure_mean = mean(pressure, na.rm=TRUE),
            precipitation_mean = mean(precipitation, na.rm=TRUE),
            temperature_sd = sd(temperature, na.rm=TRUE),
            wind_speed_100m_sd = sd(wind_speed_100m, na.rm=TRUE),
            wind_speed_sd = sd(wind_speed, na.rm=TRUE),
            pressure_sd = sd(pressure, na.rm=TRUE),
            precipitation_sd = sd(precipitation, na.rm=TRUE),
            temperature_diff = max(temperature, na.rm=TRUE) - 
              min(temperature, na.rm=TRUE),
            wind_speed_100m_diff = max(wind_speed_100m, na.rm=TRUE) -
              min(wind_speed_100m, na.rm=TRUE),
            wind_speed_diff = max(wind_speed, na.rm=TRUE) -
              min(wind_speed, na.rm=TRUE),
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
    DoW = as.character(wday(ts, label=TRUE)),
    Weekend = ifelse(DoW %in% c("Sun", "Sat"), TRUE, FALSE),
    DoY = yday(ts),
    Date = floor_date(ts, "day"),
    Holiday = ifelse(Date %in% holidays$Date, TRUE, FALSE),
    DoW2 = ifelse(Weekend == TRUE, DoW, "Weekday"),
    DoW3 = ifelse(Holiday == TRUE, "Holiday", DoW2),
    DoW4 = ifelse(Weekend == TRUE, "Weekend",
                  ifelse(Holiday == TRUE, "Holiday", "Weekday"))
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



#### Filter for forecast period ===============================================
#It seems as though I should be forecasting from 00:00 CET onwards (for 5 days),
#but everything so far has been done in UTC

price_pred <- price_pred %>% 
  filter(ts < subDate + days(6) - hours(2),
         ts >= subDate + days(1) - hours(2))

ggplot(price_pred, aes(x=ts, y=predictions)) + 
  geom_line()




#### Checks ====================================================================

if (FALSE) {
  pred_test <- price_pred %>% select(ts, predictions)
  
  pricesFuture <- NULL
  n_days_future <- 1
  for(i in -n_days_future:0) {
    priceDate = subDate-days(i)
    priceFileName <- paste0("INT_PBC_EV_H_1_",
                            strftime(priceDate, "%d_%m_%Y_"),
                            strftime(priceDate, "%d_%m_%Y"),
                            ".txt")
    price_tmp <- read.csv(paste0("./data/PricesLastWeek/", priceFileName),
                          skip=2, sep=";") %>% 
      slice(2) %>% # 2nd row has portugal prices
      select(-X) %>% 
      gather(Hour, Price) %>% 
      na.omit() %>% 
      mutate(Hour = as.numeric(str_extract(Hour, "[[:digit:]]+")) - 1,
             Price = as.numeric(str_replace(Price, ",", ".")),
             ts = priceDate + hours(Hour)) %>% 
      select(-Hour) %>% 
      #TODO: Hardcoded year as 2016 for DST fix. Create set of days and months
      #instead.
      mutate(ts = ifelse(between(floor_date(ts, "day"), 
                                 dmy("28/3/2016"), 
                                 dmy("30/10/2016")),
                         ts - hours(2), # convert from CEST to UTC
                         ts - hours(1)), # convert from CET to UTC
             ts = as.POSIXct(ts, origin="1970-01-01", tz="UTC"))
    pricesFuture = bind_rows(pricesFuture, price_tmp)
  }
  
  pred_test %>% 
    inner_join(pricesFuture) %>% 
    mutate(Date = floor_date(ts, "day")) %>% 
    mutate(ae = abs(predictions-Price)) %>% 
    summarise(mean(ae))
  pred_test %>% 
    inner_join(pricesFuture) %>% 
    mutate(Date = floor_date(ts + hours(2), "day")) %>% # CEST
    mutate(ae = abs(predictions-Price)) %>% 
    group_by(Date) %>% 
    summarise(mean(ae), n())
  pred_test %>% 
    inner_join(pricesFuture) %>% 
    mutate(error = predictions-Price) %>% 
    gather(var, val, -ts) %>% 
    ggplot(aes(x=ts, y=val, colour=var)) + 
    geom_line()
  
  
  # Check problem periods against good periods
  weather %>% 
    filter(Hour %in% 1:4) %>% 
    select(-c(ts, Year, Month, DoW, Weekend, DoY, Holiday, DoW2, DoW3)) %>% 
    gather(var, val, -c(Hour, Date)) %>% 
    ggplot(aes(x=Date, y=val, fill=factor(Hour))) + 
    geom_boxplot() +
    facet_wrap(~var, scales="free_y") +
    scale_colour_discrete(name="Hour")
  
}




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
