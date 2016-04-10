# PriceForecast

## Description

This code is for the COMPLATT SmartWatt electricity price forecasting competition.

## Logs

4/4/2016 - Used testModels_HourModels.R

5/4/2016 - Used testModels_HourModels.R

6/4/2016 - Used testModels_HourModels.R

7/4/2016 - Switched to testModels_HourModels_adjHrs.R. Uses adjacent hours when fitting. Added windspeed_sd for midday and evening models. Added temperature_sd for morning models. Combined saturday and sunday into weekend variable because was not significant and weekend categorical variable improved MAE in CV.

8/4/2016 - As above.

9/4/2016 - Started using genHourModelsForecasts_1day.R for one day ahead forecasts and old script for days 2 to 5.

10/4/2016 - As above