# library(remotes)
# remotes::install_github("energyandcleanair/creadeweather", upgrade="never")

library(creadeweather)
library(tidyverse)

source('data.R')
source('utils.R')
source('plot.R')
source('calc.deweather.R')

readRenviron(".Renviron")

dir.create("results/data", showWarnings = F, recursive = T)
dir.create("results/plots", showWarnings = F, recursive = T)


# Read data ---------------------------------------------------------------


meas <- data.meas()
plot.meas(meas)
# meas.clean <- meas %>% data.clean_meas()
meas.clean <- meas



meas.dew.trend <- calc.deweather(meas.clean, mode="trend", lag=0, fire_mode=NULL,  use_cache=T)
meas.dew.anomaly <- calc.deweather(meas.clean, mode="anomaly", lag=2, fire_mode=NULL, use_cache=T)
meas.dew.trend.fire.trajectory <- calc.deweather(meas.clean, mode="trend", lag=0, fire_mode="trajectory", use_cache=T)
meas.dew.anomaly.fire.trajectory <- calc.deweather(meas.clean, mode="anomaly", lag=2, fire_mode="trajectory", use_cache=T)


change.trend <- utils.table_change(meas.clean, "trend", meas.dew.trend)
change.anomaly <- utils.table_change(meas.clean, "anomaly", meas.dew.anomaly)
change.anomaly.lockdown <- utils.table_change_lockdown("anomaly", meas.dew.anomaly)
change.trend.fire.trajectory  <- utils.table_change(meas.clean, "trend", meas.dew.trend.fire.trajectory)


weather.fire.circular <- readRDS("results/data/weather.trend.fire.circular.RDS")
weather.fire.oriented <- readRDS("results/data/weather.trend.fire.oriented.RDS")
weather.fire.trajectory <- readRDS("results/data/weather.trend.fire.trajectory.RDS")



# Tables ------------------------------------------------------------------
write_csv(change.trend, file.path("results","data","change.trend.csv"))
write_csv(change.anomaly, file.path("results","data","change.anomaly.csv"))
write_csv(change.trend.fire.trajectory, file.path("results","data","change.trend.fire.trajectory.csv"))


# Plots -------------------------------------------------------------------


# Time series -------------------------------------------------------------
plot.trend(meas.dew.trend, change.trend, filename="ts.trend.png")
plot.trend(meas.dew.trend.fire.trajectory, change.trend.fire.trajectory, filename="ts.trend.fire.trajectory.png")
plot.observed_vs_predicted(meas.dew.anomaly, change=change.anomaly, filename="ts.anomaly.full.png")

plot.ts(meas.dew = meas.dew.anomaly, mode="anomaly")

plot.ts(meas.dew = meas.dew.anomaly, mode="anomaly", weather.fire=weather.fire.trajectory, fire_mode="trajectory")

cities_short <- c("Bangkok",
                  "Delhi",
                  "Johannesburg",
                  "Kathmandu",
                  "Los Angeles",
                  "Paris")

plot.ts(meas.dew = meas.dew.anomaly %>% filter(location_id %in% cities_short),
        mode="anomaly", weather.fire=weather.fire.trajectory, fire_mode="trajectory",
        ncol=1,
        height=10,
        width=8,
        facet_scales = "free_y",
        basename="ts.anomaly.trajectory.fire.short")

plot.ts(meas.dew = meas.dew.anomaly %>% filter(location_id %in% cities_short),
        mode="anomaly", weather.fire=weather.fire.trajectory, fire_mode="trajectory",
        ncol=1,
        height=12,
        width=8,
        facet_scales = "free_y",
        basename="ts.anomaly.trajectory.fire.short.nolabs")

for(location_id in unique(meas.dew.anomaly$location_id)){
  print(location_id)
  plot.ts(meas.dew = meas.dew.anomaly %>% filter(location_id==!!location_id),
          mode="anomaly", weather.fire=weather.fire.trajectory, fire_mode="trajectory",
          ncol=1,
          height=3,
          width=8,
          facet_scales = "free_y",
          add_legend = F,
          add_labs=F,
          basename=paste0("/cities/ts.anomaly.trajectory.fire.short.nolabs.",tolower(location_id)))
}

plot.weather.fire(weather.fire.trajectory, fire_mode="trajectory", running_days=30)
plot.weather.fire(weather.fire.oriented, fire_mode="oriented", running_days=30)
plot.weather.fire(weather.fire.circular, fire_mode="circular", running_days=30)

# Bar ---------------------------------------------------------------------
plot.change(change.trend, modes=c("dew","observed"), filename="bar.trend.observed.png")
plot.change(change.trend, modes=c("dew"), filename="bar.trend.png")
plot.change(change.anomaly, modes=c("dew","observed"), filename="bar.anomaly.observed.png")
plot.change(change.anomaly, modes=c("dew"), filename="bar.anomaly.png")


# Change lockdown not lockdown --------------------------------------------


