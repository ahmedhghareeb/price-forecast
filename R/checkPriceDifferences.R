# Name: 
# 
# Description: 
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
require(tseries)


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

price = pricePT %>%
  mutate(Price_d1 = Price - lag(Price, 1),
         Price_d24 = Price - lag(Price, 24),
         Price_d168 = Price - lag(Price, 168),
         Price_d168d1 = Price_d168 - lag(Price_d168, 1)
         #Price_l168 = lag(Price, 168),
         #Price_l168 = ifelse(is.na(Price_l168), Price, 
         #                    Price_l168),
         #Price_d168 = Price - Price_l168,
         #Price_d168_l168 = lag(Price_d168, 168)
  )

price %>% 
  filter(ts>=dmy("1/5/2015") &
           ts<=dmy("5/5/2015")) %>% 
  gather(PriceVar, Value, -ts) %>% 
  ggplot(aes(x=ts, y=Value)) +
  geom_line() +
  facet_wrap(~PriceVar)



#### Tests ====================================================================
adf.test(pricePT$Price, alternative="stationary", k=1)
adf.test(pricePT$Price, alternative="stationary", k=24)
adf.test(pricePT$Price, alternative="stationary", k=168)

kpss.test(pricePT$Price)
