# Name: testModels_heirarchical
# 
# Description: This script is a conversion of the testModels.py script for R.
# Fits a seperate model for each hour of day.
#
# Author: Cameron Roach



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
# pricePT_old = read.csv("./data/HistData/price_PT.csv", sep=";") %>% 
#   rename(ts = `date..UTC.`) %>% 
#   mutate(ts = dmy_hm(ts))
# weather_old = read.csv("./data/HistWeather/weather_hist.csv") %>% 
#   rename(ts = prediction_date) %>% 
#   mutate(ts = dmy_hm(ts))


genHourPriceModel <- function(subDate) {
  dataStart <- ymd("2016-02-12", tz="UTC")
  n_data <- round(as.numeric(subDate - dataStart))
  
  weather <- NULL
  for(i in n_data:0) {
    # Load first day from each day's weather forecast
    weatherDate = subDate-days(i)
    weather_tmp <- read.csv(paste0("./data/FcstWeather/",
                                   strftime(weatherDate, "%Y-%m-%d"),
                                   "_06-00-00.csv")) %>% 
      mutate(available_date = ymd_hms(available_date, tz="UTC"),
             prediction_date = ymd_hms(prediction_date, tz="UTC")) %>% 
      rename(ts = prediction_date) %>% 
      filter(floor_date(ts, "day") == weatherDate + days(1)) %>% 
      select(-available_date)
    
    weather <- bind_rows(weather, weather_tmp)
  }
  
  
  
  pricePT <- NULL
  for(i in n_data:0) {
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
    pricePT = bind_rows(pricePT, price_tmp)
  }
  
  
  holidays <- read.csv("./data/holidays.csv", header = F, 
                       col.names = c("Date", "Date2", "DoW", "Holiday",
                                     "Description", "Country")) %>% 
    mutate(Date = dmy(Date, tz="UTC")) %>% 
    select(Date) %>% 
    distinct()
  
  
  
  #### Engineer features ========================================================
  # Group weather stations in same countries and take simple average of
  # temperatures, wind speeds, etc.
  weather = weather %>% 
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
  
  # Merge data frames
  price = inner_join(pricePT, weather)
  
  # Add calendar variables to price dataframe. Period of day, day of week,
  # weekends, month, season, etc.
  price <- price %>% 
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
      DoW3 = ifelse(Holiday == TRUE, "Holiday", DoW2)
    )
  
  # Add hourly lags for weather variables
  # TODO: Need to add all the NAs in so that lags work properly. Just getting last
  # value rather than last time period.
  price = price %>%
    mutate(temperature_mean_l1 = lag(temperature_mean, 1),
           temperature_mean_l1 = ifelse(is.na(temperature_mean_l1), temperature_mean, 
                                        temperature_mean_l1),
           temperature_mean_l2 = lag(temperature_mean, 2),
           temperature_mean_l2 = ifelse(is.na(temperature_mean_l2), temperature_mean, 
                                        temperature_mean_l2),
           temperature_mean_l24 = lag(temperature_mean, 24),
           temperature_mean_l24 = ifelse(is.na(temperature_mean_l2), temperature_mean, 
                                         temperature_mean_l2),
           Price_l24 = lag(Price, 24),
           Price_l24 = ifelse(is.na(Price_l24), Price, 
                              Price_l24),
           Price_l168 = lag(Price, 168),
           Price_l168 = ifelse(is.na(Price_l168), Price, 
                               Price_l168)
    )
  
  
  
  #### Fit models ===============================================================
  fitControl <- trainControl(
    method = "timeslice",
    initialWindow = 20,
    horizon=5,
    fixedWindow=FALSE,
    summaryFunction = maeSummary)
  
  model_h <- list()
  mae <- rep(NA, 24)
  # Morning models
  for (i in 0:3) {
    cat(paste("Fitting hour", i, "...\n"))
    model_h[[i+1]] <- train(Price ~ Price_l168 + DoW3 + poly(wind_speed_mean, 2) +
                              temperature_sd,
                            data = filter(price, Hour == i),
                            method="lm",
                            metric="MAE",
                            trControl = fitControl
    )
    mae[i+1] <- model_h[[i+1]]$results$MAE
    print(model_h[[i+1]])
  }
  mean(mae[1:4]) #4.78 #4.6323
  
  #Midday models
  for (i in 4:15) {
    cat(paste("Fitting hour", i, "...\n"))
    model_h[[i+1]] <- train(Price ~ Price_l168 + DoW3 + poly(wind_speed_mean, 2),
                            data = filter(price, Hour == i),
                            method="lm",
                            metric="MAE",
                            trControl = fitControl
    )
    mae[i+1] <- model_h[[i+1]]$results$MAE
    print(model_h[[i+1]])
  }
  mean(mae[5:16])
  
  #Evening models
  for (i in 16:23) {
    cat(paste("Fitting hour", i, "...\n"))
    model_h[[i+1]] <- train(Price ~ Price_l168 + DoW3 + poly(wind_speed_mean, 2),
                            data = filter(price, Hour == i),
                            method="lm",
                            metric="MAE",
                            trControl = fitControl
    )
    mae[i+1] <- model_h[[i+1]]$results$MAE
    print(model_h[[i+1]])
  }
  mean(mae[17:24])
  
  print(paste0("MAE during morning: ", mean(mae[1:8])))
  print(paste0("MAE during midday: ", mean(mae[9:16])))
  print(paste0("MAE during evening: ", mean(mae[17:24])))
  print(paste0("Daily MAE: ", mean(mae)))
  
  #### Evaluation metrics =======================================================
  price_pred <- NULL
  for (i in 0:23) {
    predictions <- predict(model_h[[i+1]], newdata = filter(price, Hour==i))
    price_tmp <- price %>% 
      filter(Hour==i) %>% 
      mutate(Price_h = predictions,
             r_h = Price_h - Price)
    
    price_pred <- bind_rows(price_pred, price_tmp)
  }
  price_pred <- price_pred %>% 
    arrange(ts)
  
  for(i in unique(months(price_pred$ts))) {
    p <- price_pred %>% 
      filter(month(ts, label=T, abbr=F)==i) %>% 
      select(ts, Price, Price_h, r_h) %>% 
      gather(var, value, -ts) %>% 
      ggplot(aes(x=ts, y=value, colour=var)) +
      geom_line() +
      ggtitle(paste("Price actuals and predictions for", i, "2015"))
    print(p)
  }
  
  
  
  #### Choose final model and save ==============================================
  finalModel <- model_h
  
  return(finalModel)
}
#dir.create("./cache", F, T)
#save(finalModel, file="./cache/HierarchicalModel.RData")
