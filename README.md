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

11/4/2016 - As above

12/4/2016 - As above

13/4/2016 - Fixed up one day forecast to use better DoW variables. Now using DoW5. In the 5 day forecast script, switched the training day for weather data forecasts from day 4 (produced smallest MAE for 5 day horizon). Really should train a separate model for each forecast day, but not enough time today. Will do tomorrow.