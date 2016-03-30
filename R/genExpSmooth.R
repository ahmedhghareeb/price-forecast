# Name: Test exponential smoothing
# 
# Description: Applies exponential smoothing for each hour.
#
# Author: Cameron Roach

rm(list=ls())

require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(forecast)

subDate = ymd("2016-03-29", tz="CET")

#### Load data ================================================================
# Load last week of prices for lagged price variable
prices <- NULL
for(i in 17:0) {
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
  prices = bind_rows(prices, price_tmp)
}
prices <- na.omit(prices)




#### Predict forecasts ========================================================
price_pred <- NULL
for (i in 0:23) {
  price_ts <- ts(prices$Price[prices$Hour==i])
  price_tmp <- ses(price_ts, alpha = 0.5, h=5, initial="optimal")
  #price_tmp <- hw(price_ts, seasonal = "additive")
  price_tmp <- data.frame(
    Hour = i,
    predictions = data.frame(price_tmp)$`Point.Forecast`,
    Date = subDate + days(1:5))
  price_pred <- bind_rows(price_pred, price_tmp)
}
price_pred <- price_pred %>% 
  mutate(ts = Date + hours(Hour)) %>% 
  arrange(ts)


#### UTC adjustment for CUT =================================================== 
#It seems as though I should be forecasting from 00:00 CET onwards (for 5 days),
#but everything so far has been done in UTC
price_pred <- price_pred %>% 
  mutate(ts = with_tz(ts, tz="CET")) %>% 
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

