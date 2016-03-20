# Name: Check weather fcsts
# 
# Description: Checks the difference between forecast at end of day, to next
# day's forcast at begging of day.
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
weatherFcst1 <- read.csv(paste0("./data/FcstWeather/",
                               strftime(fcstStartDay - days(1), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = strftime(ymd_hms(available_date), "%d-%m-%Y"),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date)
weatherFcst2 <- read.csv(paste0("./data/FcstWeather/",
                               strftime(fcstStartDay - days(2), "%Y-%m-%d"),
                               "_06-00-00.csv")) %>% 
  mutate(available_date = strftime(ymd_hms(available_date), "%d-%m-%Y"),
         prediction_date = ymd_hms(prediction_date)) %>% 
  rename(ts = prediction_date)
locations = read.csv("./data/HistWeather/locations.csv", stringsAsFactors = FALSE)


weatherFcst <- bind_rows(weatherFcst1, weatherFcst2)



#### Plot =====================================================================
weatherFcst %>% 
  filter(point==5) %>% 
  ggplot(aes(x=ts, y=temperature, colour=available_date)) +
  geom_line()