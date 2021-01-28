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

f <- file.path("results","data", "meas_dew.RDS")

if(!file.exists(f)){
  # There was very little difference between lag of one or two days
  # although a bit more so in China (lag2<lag1)
  lags <- c(0)
  meas.dews <- lapply(lags, function(lag){
    deweather(meas=meas.clean,
              poll="pm25",
              output=c("trend"),
              add_pbl=T,
              upload_results=F,
              # training_end_anomaly = "2019-12-31",
              lag=lag
    ) %>%
      mutate(lag=!!lag)
  })

  meas.dew <- do.call("bind_rows",
                      meas.dews)

  saveRDS(meas.dew, f)

}else{
  meas.dew <- readRDS(f)
}

f.fire <- file.path("results","data", "meas_dew_fire.RDS")

if(!file.exists(f.fire)){
  # There was very little difference between lag of one or two days
  # although a bit more so in China (lag2<lag1)
  lags <- c(0)
  meas.dews <- lapply(lags, function(lag){
    deweather(meas=meas.clean,
              poll="pm25",
              output=c("trend"),
              add_pbl=T,
              upload_results=F,
              add_fire=T,
              # training_end_anomaly = "2019-12-31",
              lag=lag
    ) %>%
      mutate(lag=!!lag)
  })

  meas.dew.fire <- do.call("bind_rows",
                      meas.dews)

  saveRDS(meas.dew.fire, f.fire)

}else{
  meas.dew.fire <- readRDS(f.fire)
}


meas.dew <- meas.dew %>% filter(lag==0)

plot.trend(meas.dew, filename="trend.png")
plot.trend(meas.dew.fire, filename="trend_fire.png")


change <- utils.table_change(meas.clean, meas.dew) %>%
  filter(!location_id %in% c("Kano", "Krasnoyarsk"))
write_csv(change, file.path("results","data","change.csv"))

change.fire <- utils.table_change(meas.clean, meas.dew.fire) %>%
  filter(!location_id %in% c("Kano", "Krasnoyarsk"))
write_csv(change, file.path("results","data","change.csv"))

# Plot changes
# plot.change(change, c("anomaly","trend","observed"))
plot.change(change, c("trend","observed"))
plot.change(change, c("trend"))

plot.trend()
