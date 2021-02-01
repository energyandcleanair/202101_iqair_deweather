plot.change <- function(change, method_levels= c("trend","observed")){
  change.plot <- change %>% tidyr::pivot_longer(names_to="type", names_prefix = "change_str_", values_to="value",
                                                c(change_str_trend, change_str_observed)) %>%
    mutate(value=readr::parse_number(value, na = c("", "NA", "NANA"))/100) %>%
    filter(type %in% method_levels)

  change.plot$location_id <- factor(change.plot$location_id,
                                    levels(reorder(change.plot[change.plot$type==method_levels[1],]$location_id,
                                                   -change.plot[change.plot$type==method_levels[1],]$value)))

  change.plot$type <- factor(change.plot$type, method_levels)

  ggplot(change.plot) +
    geom_bar(aes(y=location_id, x=value, fill=type),
             stat="identity",
             position=position_dodge2(reverse = TRUE),
             show.legend = F
    ) +
    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(name="Method") +
    labs(x=NULL,y=NULL) +
    scale_x_continuous(labels=scales::percent) +
    labs(title="2020 vs 2019 PM2.5 levels in selected cities",
         subtitle=ifelse(all(method_levels=="trend"),"After adjusting for weather conditions",""))

  ggsave(file.path("results","plots",
  paste0("change.",method_levels[1],".png")), width=8, height=6)
}


plot.trend <- function(meas.dew, change, filename){


  # Plot ts
  plot.data <- meas.dew %>%
    filter(output=="trend") %>%
    unnest(normalised) %>%
    left_join(change %>% select(location_id, change_str_trend)) %>%
    mutate(location_id=paste0(location_id," [",change_str_trend,"]")) %>%
    rcrea::utils.running_average(30)

  (plt <- ggplot(plot.data) +
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
         caption="30-day running average."))

  ggsave(file.path("results/plots",filename), width=14, height=12)

  return(plt)
}


# Old ---------------------------------------------------------------------

plot.observed_vs_predicted <- function(meas.dew){

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
}


plot.anomaly <- function(meas.dew){


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
}

plot.weather.fire <- function(weather.fire){

  d.plot <- weather.fire %>%
    select(station_id, meas_weather) %>%
    tidyr::unnest(meas_weather)

  ggplot(d.plot) +
    geom_line(aes(date, fire_frp), col="darkred") +
    facet_wrap(~station_id, scales="free_y") +
    # scale_y_continuous(labels=scales::percent) +
    theme_light() +
    # geom_hline(yintercept = 0, col="grey30") +
    scale_x_date(date_labels = "%Y") +
    rcrea::theme_crea() +
    labs(subtitle="Fire Radiative Potential where wind is blowing from",
         y=NULL,
         x=NULL)
  ggsave(file.path("results/plots/weather_fire.png"), width=14, height=12)
}
