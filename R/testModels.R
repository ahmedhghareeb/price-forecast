# Name: Forecast electricity prices
# 
# Description: This script is a conversion of the testModels.py script for R.
# Additional functionality will be added. Tests models and predicts prices.
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
locations = read.csv("./data/HistWeather/locations.csv")



#### Engineer features ========================================================
# Group weather stations in same countries and take simple average of
# temperatures, wind speeds, etc.
weatherMean = inner_join(weather, locations)
# TODO: calculate the difference between the two country averages and maybe avg countries to get one average temp value and one difference between countries value. Only do this if there is strong correlation between the two country average temperatures - CHECK!
weatherMean = weatherMean %>% 
  group_by(ts, Country) %>% 
  summarise(temperature = mean(temperature, na.rm=TRUE)) %>% 
  spread(Country, temperature) %>% 
  rename(P_temp = Portugal, S_temp = Spain)


# Merge data frames
price = inner_join(pricePT, weatherMean)

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
  mutate(P_temp_l1 = lag(P_temp, 1),
         P_temp_l1 = ifelse(is.na(P_temp_l1), P_temp, P_temp_l1),
         P_temp_l2 = lag(P_temp, 2),
         P_temp_l2 = ifelse(is.na(P_temp_l2), P_temp, P_temp_l2),
         S_temp_l1 = lag(S_temp, 1),
         S_temp_l1 = ifelse(is.na(S_temp_l1), S_temp, S_temp_l1),
         S_temp_l2 = lag(S_temp, 2),
         S_temp_l2 = ifelse(is.na(S_temp_l2), S_temp, S_temp_l2)
         )

#### Plots ====================================================================
price %>% 
  ggplot(aes(x=DoY, y=Price, colour=Hour)) + 
  geom_point() +
  geom_smooth(method="lm", formula=y~ns(x, 4), se=F) +
  ggtitle("Seasonality in prices for each hour of the day.")

price %>% 
  select(Price, Hour, P_temp, S_temp) %>% 
  gather(Country, Temperature, -c(Price, Hour)) %>% 
  ggplot(aes(x=Temperature, y=Price, colour=Hour)) + 
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", formula=y~ns(x, 4), se=F) +
  facet_wrap(~Country) +
  ggtitle("Relationship between price and temperature for each hour of the day.")

price %>% 
  select(Price, P_temp, S_temp, Weekend) %>% 
  gather(Country, Temperature, -c(Price, Weekend)) %>% 
  ggplot(aes(x=Temperature, y=Price, colour=Weekend)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", formula=y~ns(x,3)) +
  facet_wrap(~Country) +
  ggtitle("Relationship between price and temperature for weekends and weekdays.")

price %>% 
  ggplot(aes(x=DoW, y=Price)) + 
  geom_boxplot()

# Check relationship between lagged temperatures and price
price %>% 
  select(Price, P_temp_l1, P_temp_l2, S_temp_l1, S_temp_l2) %>% 
  gather(Lag, Temperature, -Price) %>% 
  ggplot(aes(x=Temperature, y=Price)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Lag)


#### Fit models ===============================================================
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  summaryFunction = maeSummary)

# Linear models
model_lm1 <- train(Price ~ ns(P_temp, 3) + ns(S_temp, 3) + Hour + DoW2 + ns(DoY, 4),
                  data = price,
                  method="lm",
                  metric="MAE",
                  trControl = fitControl
                  )
summary(model_lm1)

model_lm2 <- train(Price ~ ns(P_temp, 3) + ns(S_temp, 3) + Hour + DoW2 + ns(DoY, 4) +
                     P_temp_l1 + P_temp_l2 + S_temp_l1 + S_temp_l2,
                   data = price,
                   method="lm",
                   trControl = fitControl
)
summary(model_lm1)



# model_rf <- train(Price ~ P_temp + S_temp + Hour + DoW2 + Month,
#                   data = price %>% sample_n(1000),
#                   method="rf")


#### Evaluation metrics =======================================================
price <- price %>% 
  mutate(Price_lm1 = predict(model_lm1, newdata = price),
         r_lm1 = Price - Price_lm1,
         Price_lm2 = predict(model_lm2, newdata = price),
         r_lm2 = Price - Price_lm2)

price %>% 
  filter(month(ts)==1) %>% 
  select(ts, Price, Price_lm2, r_lm2) %>% 
  gather(var, value, -ts) %>% 
  ggplot(aes(x=ts, y=value, colour=var)) +
  geom_line()



#### Choose final model and save ==============================================
finalModel <- model_lm2

dir.create("./cache", F, T)
save(finalModel, file="./cache/finalModel.RData")
