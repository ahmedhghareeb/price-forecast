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
require(forecast)

fcstStartDay = ymd(today(), tz="CET")

#### Load data ================================================================
weatherFcst <- read.csv(paste0("./data/FcstWeather/",
                               strftime(fcstStartDay - days(1), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = ymd_hms(available_date),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date)

# Load last week of prices for lagged price variable
pricesLastWeek <- NULL
for(i in 8:0) {
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
# pricesLagged = pricesLastWeek %>% 
#   select(ts, Price_l168 = Price) %>% 
#   mutate(ts = ts + days(7))
# rm(pricesLastWeek)
price_ts <- ts(pricesLastWeek$Price)

price_aa <- auto.arima(price_ts)

plot(forecast(price_aa, h = 24*5))
