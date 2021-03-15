
library(creadeweather)
library(tidyverse)

source('data.R')
source('utils.R')
source('plot.R')
source('calc.deweather.R')

readRenviron(".Renviron")



meas <- data.meas()
plot.meas(meas)
# meas.clean <- meas %>% data.clean_meas()
meas.clean <- meas


meas.dew.trend.fire <- calc.deweather(meas.clean, mode="trend", lag=0, fire_mode="trajectory", use_cache=T)
meas.dew.trend <- calc.deweather(meas.clean, mode="trend", lag=0, fire_mode=NULL,  add_fire=F, use_cache=T)
