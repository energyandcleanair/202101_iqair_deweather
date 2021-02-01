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

#f.fire.circular <- file.path("results","data", "meas_dew_fire_circular.RDS")
#if(!file.exists(f.fire.circular)){
#  lag <- 0
#  meas.dew.fire.circular <-  deweather(meas=meas.clean %>% filter(date>="2017-01-01"), #prevent loading 2016 data, saves a bit of time
#              poll="pm25",
#              output=c("trend"),
#              add_pbl=T,
#              upload_results=F,
#              add_fire=T,
#	            fire_mode="circular",
#	            save_weather_filename="results/data/weather_fire_circular.RDS",
#              lag=lag
#    ) %>%
#      mutate(lag=!!lag)
#  saveRDS(meas.dew.fire.circular, f.fire.circular)
#
#}else{
#  meas.dew.fire.circular <- readRDS(f.fire.circular)
#}

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

}else{
  meas.dew.fire.oriented <- readRDS(f.fire.oriented)
}


meas.dew <- meas.dew %>% filter(lag==0)

change <- utils.table_change(meas.clean, meas.dew) %>%
  filter(!location_id %in% c("Kano", "Krasnoyarsk"))
write_csv(change, file.path("results","data","change.csv"))

# Plot changes
# plot.change(change, c("anomaly","trend","observed"))
plot.change(change, c("trend","observed"))
plot.change(change, c("trend"))

# Plot observed vs counterfactual
ggplot(meas.dew %>%
         filter(output=="counterfactual") %>%
         unnest(normalised) %>%
         select(location_id, date, value) %>%
         mutate(type="predicted") %>%
         bind_rows(meas.clean %>% select(location_id, date, value) %>%
                     mutate(type="observed")) %>%
         left_join(change %>% select(location_id, change_str_anomaly)) %>%
         mutate(location_id=paste0(location_id," [",change_str_anomaly,"]")) %>%
         # filter(lubridate::year(date)==2020) %>%
         # mutate(observed=predicted+value) %>%
         # select(-c(value, unit, set)) %>%
         # gather("type","value", c(predicted,observed)) %>%
         rcrea::utils.running_average(30)
       ) +
  geom_line(aes(date, value, col=type)) +
  facet_wrap(~location_id, scales="free_y") +
  geom_vline(xintercept=as.POSIXct("2020-01-01"), col="orange",linetype="dashed") +
  annotate(geom = 'text',
           label = 'Training', x = as.POSIXct("2020-01-01"),
           y = Inf, hjust = 1.2, vjust = 2, col="orange", size=3) +
  annotate(geom = 'text',
           label = 'Predicting', x = as.POSIXct("2020-01-01"),
           y = Inf, hjust = -0.1, vjust = 2, col="orange", size=3) +
  theme_light() +
  rcrea::theme_crea() +
  ylim(0, NA) +
  labs(subtitle="Observed vs predicted",
       y="PM2.5 [µg/m3]",
       x=NULL, caption="30-day running average.")

ggsave(file.path("results/plots/observed_vs_predicted.png"), width=14, height=12)

ggplot(meas.dew %>%
         filter(output=="anomaly_vs_counterfactual") %>%
         unnest(normalised) %>%
         rcrea::utils.running_average(30)
) +
  geom_line(aes(date, value), col="darkred") +
  facet_wrap(~location_id) +
  scale_y_continuous(labels=scales::percent) +
  theme_light() +
  geom_hline(yintercept = 0, col="grey30") +
  scale_x_datetime(date_labels = "%b") +
  rcrea::theme_crea() +
  labs(subtitle="Anomaly vs predicted in 2020",
       y=NULL,
       x=NULL)
ggsave(file.path("results/plots/anomaly.png"), width=14, height=12)


# Trend analysis ----------------------------------------------------------


# Plot ts
plot.data <- meas.dew %>%
  filter(output=="trend") %>%
  unnest(normalised) %>%
  left_join(change %>% select(location_id, change_trend_str_lag0)) %>%
  mutate(location_id=paste0(location_id," [",change_trend_str_lag0,"]")) %>%
  rcrea::utils.running_average(30)

ggplot(plot.data) +
  geom_rect(data=tibble(location_id=unique(plot.data[["location_id"]]),
                        xmin=lubridate::date("2020-01-01"),
                        xmax=lubridate::date("2020-12-31"),
                        ymin=-Inf,
                        ymax=Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="orange",
            alpha=0.3) +
  geom_line(aes(date, value)) +
  facet_wrap(~location_id, scales="free_y") +
  theme_light() +
  rcrea::theme_crea() +
  theme(panel.grid.major.x = element_line("grey95")) +
  ylim(0, NA) +
  labs(subtitle="Deweathered trend",
       y="PM2.5 [µg/m3]",
       x=NULL,
       caption="30-day running average.")

ggsave(file.path("results/plots/trend.png"), width=14, height=12)

