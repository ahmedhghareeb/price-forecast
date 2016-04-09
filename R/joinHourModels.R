source("./R/genHourModelsForecasts.R")
source("./R/genHourModelsForecasts_1day.R")

rm(list=ls())

subDate <- ymd("2016-04-09", tz="UTC")



filename = paste0("1639e1334b2ec40805be8dedc132764d_",
                  strftime(subDate, "%Y-%m-%d"))



load(paste0("./cache/5_day_", filename, ".RData"))
outputForecasts5day <- outputForecasts %>% 
  slice(-c(1:24))

load(paste0("./cache/1_day_", filename, ".RData"))

outputForecasts <- bind_rows(outputForecasts, outputForecasts5day)

write.csv(outputForecasts, file = paste0("./cache/", filename, ".csv"),
          row.names=F, quote=F)