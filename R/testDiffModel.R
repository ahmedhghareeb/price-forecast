# Name: Difference model
# 
# Description: This script is a conversion of the hierarchical model script for
# R. Instead of predicting prices, will predict difference in price from last
# week.
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
# 
# TODO: Need to add all the NAs in so that lags work properly. Just getting last
# value rather than last time period.
# 
# TODO: Instead of filtering out first week (to get rid of differences == 0), 
# maybe set each missing lagged price to the average for that hour within the 
# current week. That serves as a decent approximation for what the price might
# have been.
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
                             Price_l168),
         Price_d168 = Price - Price_l168,
         Price_d168_l168 = lag(Price_d168, 168)
  )

# TODO: Remove this and do to do above
price <- price %>% 
  filter(Date > min(Date) + days(6))


#### Fit models ===============================================================
fitControl <- trainControl(
  method = "timeslice",
  initialWindow = 40,
  horizon=5,
  fixedWindow=FALSE,
  summaryFunction = maeSummary)

# TODO: Check which linear model tends to work best for individual hours
model_diff <- list()
mae <- rep(NA, 24)
for (i in 0:23) {
  cat(paste("Fitting hour", i, "...\n"))
  model_diff[[i+1]] <- train(Price_d168 ~ Price_d168_l168 + 
                               wind_speed_100m_mean + ns(temperature_mean, 2) +
                            pressure_mean,
                          data = filter(price, Hour == i),
                          method="lm",
                          metric="MAE",
                          trControl = fitControl
  )
  mae[i+1] <- model_diff[[i+1]]$results$MAE
  print(model_diff[[i+1]])
}
mean(mae)

#### Evaluation metrics =======================================================
price_pred <- NULL
for (i in 0:23) {
  predictions <- predict(model_diff[[i+1]], newdata = filter(price, Hour==i))
  price_tmp <- price %>% 
    filter(Hour==i) %>% 
    mutate(Price_diff = predictions + Price_l168,
           r_diff = Price_diff - Price)
  
  price_pred <- bind_rows(price_pred, price_tmp)
}
price_pred <- price_pred %>% 
  arrange(ts)

for(i in 1:10) {
  p <- price_pred %>% 
    filter(month(ts)==i) %>% 
    select(ts, Price, Price_diff, r_diff) %>% 
    gather(var, value, -ts) %>% 
    ggplot(aes(x=ts, y=value, colour=var)) +
    geom_line() +
    ggtitle(paste("Price actuals and predictions for", month.name[i], "2015"))
  print(p)
}



#### Choose final model and save ==============================================
finalModel <- model_diff

dir.create("./cache", F, T)
save(finalModel, file="./cache/DifferenceModel.RData")
