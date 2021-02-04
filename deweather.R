library(remotes)
remotes::install_github("energyandcleanair/creadeweather", upgrade="never")

library(creadeweather)
library(tidyverse)

source('data.R')
source('utils.R')
source('plot.R')
source('calc.deweather.R')

readRenviron(".Renviron")

dir.create("results/data", showWarnings = F, recursive = T)
dir.create("results/plots", showWarnings = F, recursive = T)

meas <- data.meas()
plot.meas(meas)
# meas.clean <- meas %>% data.clean_meas()
meas.clean <- meas



meas.dew.trend <- calc.deweather(meas.clean, mode="trend",fire_mode=NULL,  use_cache=T)
meas.dew.anomaly <- calc.deweather(meas.clean, mode="anomaly",fire_mode=NULL, use_cache=T)
meas.dew.trend.fire.oriented <- calc.deweather(meas.clean, mode="trend",fire_mode="oriented", use_cache=T)
meas.dew.trend.fire.trajectory <- calc.deweather(meas.clean, mode="trend", fire_mode="trajectory", use_cache=T)


change.trend <- utils.table_change(meas.clean, meas.dew.trend)
change.anomaly <- utils.table_change(meas.clean, meas.dew.anomaly)
# change.fire  <- utils.table_change(meas.clean, meas.dew.fire)

plot.trend(meas.dew, change, filename="trend.png")
# plot.trend(meas.dew.fire, change.fire, filename="trend_fire.png")

write_csv(change, file.path("results","data","change.csv"))
# write_csv(change.fire, file.path("results","data","change_fire.csv"))


# Plot changes
plot.change(change, c("trend","observed"))
plot.change(change, c("trend"))


# Plot weather
weather.fire <- readRDS("results/data/weather_fire_oriented.RDS")
plot.weather.fire(weather.fire)
