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
meas.dew.fire.circular <- calc.deweather(meas.clean, fire_mode="circular", use_cache=T)
meas.dew.fire.oriented <- calc.deweather(meas.clean, fire_mode="oriented", use_cache=T)

f.fire.oriented <- file.path("results","data", "meas_dew_fire_oriented.RDS")
if(!file.exists(f.fire.oriented)){
  lag <- 0
  meas.dew.fire.oriented <-  deweather(meas=meas.clean %>% filter(date>="2017-01-01"), #prevent loading 2016 data, saves a bit of time
                                       poll="pm25",
                                       output=c("trend"),
                                       add_pbl=T,
                                       upload_results=F,
                                       add_fire=T,
                                       fire_mode="oriented",
                                       save_weather_filename="results/data/weather_fire_oriented.RDS",
                                       lag=lag
  ) %>%
    mutate(lag=!!lag)
  saveRDS(meas.dew.fire.oriented, f.fire.oriented)
>>>>>>> ece65942ef0727c9455240a17e1c3702323fae1e

}else{
  meas.dew.fire.oriented <- readRDS(f.fire.oriented)
}


meas.dew <- meas.dew %>% filter(lag==0)
meas.dew.fire <- meas.dew.fire %>% filter(lag==0)

change <- utils.table_change(meas.clean, meas.dew)
change.fire  <- utils.table_change(meas.clean, meas.dew.fire)

plot.trend(meas.dew, change, filename="trend.png")
plot.trend(meas.dew.fire, change.fire, filename="trend_fire.png")

write_csv(change, file.path("results","data","change.csv"))
write_csv(change.fire, file.path("results","data","change_fire.csv"))


# Plot changes
# plot.change(change, c("anomaly","trend","observed"))
plot.change(change, c("trend","observed"))
plot.change(change, c("trend"))

plot.trend()
