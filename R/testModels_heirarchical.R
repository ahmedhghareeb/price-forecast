# Name: testModels_heirarchical
# 
# Description: This script is a conversion of the testModels.py script for R.
# Fits a seperate model for each hour of day.
#
# Author: Cameron Roach


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

#merge(pricePT, weather[weather$point==5,]) %>% select(-ts, -point) %>% pairs()

#### Engineer features ========================================================
# Group weather stations in same countries and take simple average of
# temperatures, wind speeds, etc.
weather = weather %>% 
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

# Merge data frames
price = inner_join(pricePT, weather)

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
  mutate(temperature_mean_l1 = lag(temperature_mean, 1),
         temperature_mean_l1 = ifelse(is.na(temperature_mean_l1), temperature_mean, 
                                      temperature_mean_l1),
         temperature_mean_l2 = lag(temperature_mean, 2),
         temperature_mean_l2 = ifelse(is.na(temperature_mean_l2), temperature_mean, 
                                      temperature_mean_l2),
         temperature_mean_l24 = lag(temperature_mean, 24),
         temperature_mean_l24 = ifelse(is.na(temperature_mean_l2), temperature_mean, 
                                       temperature_mean_l2),
         Price_l168 = lag(Price, 168),
         Price_l168 = ifelse(is.na(Price_l168), Price, 
                             Price_l168)
  )



#### Fit models ===============================================================
fitControl <- trainControl(
  method = "timeslice",
  initialWindow = 150,
  horizon=14,
  summaryFunction = maeSummary)

# TODO: Check which linear model tends to work best for individual hours
model_h <- list()
mae <- rep(NA, 24)
for (i in 0:23) {
  cat(paste("Fitting hour", i, "...\n"))
  model_h[[i+1]] <- train(Price ~ Price_l168 + DoW2 + wind_speed_100m_mean + 
                            ns(temperature_mean, 2) +
                            pressure_mean,
                          data = filter(price, Hour == i),
                          method="lm",
                          metric="MAE",
                          trControl = fitControl
  )
  mae[i+1] <- model_h[[i+1]]$results$MAE
  print(model_h[[i+1]])
}
mean(mae)

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

for(i in 1:12) {
  p <- price_pred %>% 
    filter(month(ts)==i) %>% 
    select(ts, Price, Price_h, r_h) %>% 
    gather(var, value, -ts) %>% 
    ggplot(aes(x=ts, y=value, colour=var)) +
    geom_line() +
    ggtitle(paste("Price actuals and predictions for", month.name[i], "2015"))
  print(p)
}



#### Choose final model and save ==============================================
finalModel <- model_h

dir.create("./cache", F, T)
save(finalModel, file="./cache/HierarchicalModel.RData")
