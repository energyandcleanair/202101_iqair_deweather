library(remotes)
remotes::install_github("energyandcleanair/creadeweather", upgrade="never")

library(creadeweather)
library(tidyverse)

source('data.R')
source('utils.R')
source('plot.R')

readRenviron(".Renviron")

dir.create("results/data", showWarnings = F, recursive = T)
dir.create("results/plots", showWarnings = F, recursive = T)

meas <- data.meas()
ggplot(meas) +geom_line(aes(date,value)) + facet_wrap(~location_id, scales="free_y") + rcrea::theme_crea()

# meas.clean <- meas %>% data.clean_meas()
# ggplot(meas.clean) +geom_line(aes(date,value)) + facet_wrap(~location_id, scales="free_y") + rcrea::theme_crea()
meas.clean <- meas

meas.dew <- calc.deweather(meas.clean, fire_mode=NULL, use_cache=T)
meas.dew.fire <- calc.deweather(meas.clean, fire_mode="oriented", use_cache=T)
meas.dew.fire.trajectory <- calc.deweather(meas.clean %>% filter(location_id==c("Singapore","Melbourne")), fire_mode="trajectory", use_cache=T)

change <- utils.table_change(meas.clean, meas.dew)
change.fire  <- utils.table_change(meas.clean, meas.dew.fire)

plot.trend(meas.dew, change, filename="trend.png")
plot.trend(meas.dew.fire, change.fire, filename="trend_fire.png")

write_csv(change, file.path("results","data","change.csv"))
write_csv(change.fire, file.path("results","data","change_fire.csv"))


# Plot changes
plot.change(change, c("trend","observed"))
plot.change(change, c("trend"))


# Plot weather
weather.fire <- readRDS("results/data/weather_fire_oriented.RDS")
plot.weather.fire(weather.fire)
