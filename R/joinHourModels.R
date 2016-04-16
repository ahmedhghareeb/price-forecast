rm(list=ls())

require(dplyr)
require(lubridate)
require(ggplot2)
require(tidyr)
require(stringr)

source("./R/genHourModelsForecasts.R")
source("./R/genHourModelsForecasts_1day.R")

rm(list=ls())

subDate <- ymd("2016-04-16", tz="UTC")



filename = paste0("1639e1334b2ec40805be8dedc132764d_",
                  strftime(subDate, "%Y-%m-%d"))



load(paste0("./cache/5_day_", filename, ".RData"))
outputForecasts5day <- outputForecasts %>% 
  slice(-c(1:24))

load(paste0("./cache/1_day_", filename, ".RData"))

# ggplot() +
#   geom_line(data=outputForecasts5day, aes(x=1:120, y=value),
#             colour="blue") +
#   geom_line(data=outputForecasts, aes(x=1:24, y=value),
#             colour="red")

outputForecasts <- bind_rows(outputForecasts, outputForecasts5day)

write.csv(outputForecasts, file = paste0("./cache/", filename, ".csv"),
          row.names=F, quote=F)




#### Checks ====================================================================

if (FALSE) {
  pred_test <- outputForecasts %>% 
    mutate(ts = dmy(predictiondate) + hours(hour)) %>% 
    select(ts, predictions = value)
  
  pricesFuture <- NULL
  n_days_future <- 5
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
