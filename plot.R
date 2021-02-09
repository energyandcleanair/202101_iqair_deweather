plot.meas <- function(meas){
  plt <- ggplot(meas) +
    geom_line(aes(date,value)) +
    facet_wrap(~location_id, scales="free_y") +
    rcrea::theme_crea()
  ggsave("results/plots/meas.png", plt, width=12, height=10)
  return(plt)
}

plot.change <- function(change, modes, filename){

  change.plot <- change %>%
    select(location_id, poll, change_dew, change_observed) %>%
    tidyr::pivot_longer(names_to="type", names_prefix = "change_", values_to="value",
                                                c(change_dew, change_observed)) %>%
    # mutate(value=readr::parse_number(value, na = c("", "NA", "NANA"))/100) %>%
    filter(type %in% modes)

  change.plot$location_id <- factor(change.plot$location_id,
                                    levels(reorder(change.plot[change.plot$type==modes[1],]$location_id,
                                                   -change.plot[change.plot$type==modes[1],]$value)))

  change.plot$type <- factor(change.plot$type, modes)

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
         subtitle=ifelse(all(modes=="dew"),"After adjusting for weather conditions",""))

  ggsave(file.path("results","plots",filename), width=8, height=6)
}


plot.trend <- function(meas.dew, change, filename){

  # Plot ts
  plot.data <- meas.dew %>%
    filter(output=="trend") %>%
    unnest(normalised) %>%
    left_join(change %>% select(location_id, change_str_dew)) %>%
    mutate(location_id=paste0(location_id," [",change_str_dew,"]")) %>%
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

plot.observed_vs_predicted <- function(meas.dew, change, filename="ts.anomaly.png"){

  # Plot observed vs counterfactual
  ggplot(meas.dew %>%
           filter(output=="counterfactual") %>%
           unnest(normalised) %>%
           select(location_id, date, value) %>%
           mutate(type="predicted") %>%
           bind_rows(meas.clean %>% select(location_id, date, value) %>%
                       mutate(type="observed")) %>%
           left_join(change %>% select(location_id, change_str_dew)) %>%
           mutate(location_id=paste0(location_id," [",change_str_dew,"]")) %>%
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

  ggsave(file.path("results","plots",filename), width=14, height=12)
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

plot.weather.fire <- function(weather.fire, fire_mode, running_days=30){

  min_date <- lubridate::date(as.POSIXct("2020-01-01", tz="UTC"))
  max_date <- lubridate::date(as.POSIXct("2021-01-01", tz="UTC"))

  d.plot <- weather.fire %>%
    select(station_id, meas_weather) %>%
    tidyr::unnest(meas_weather) %>%
    select(station_id, date, value=fire_frp) %>%
    rcrea::utils.running_average(running_days)

  ggplot(d.plot) +
    geom_line(aes(date, value), col="darkred") +
    facet_wrap(~station_id, scales="free_y") +
    # scale_y_continuous(labels=scales::percent) +
    theme_light() +
    # geom_hline(yintercept = 0, col="grey30") +
    scale_x_date(
      limits=c(min_date, max_date),
      breaks = seq(min_date,
                   max_date,
                   by="3 month"),
      minor_breaks = seq(min_date,
                         max_date,
                         by="1 month"),
      date_labels = "%b") +
    rcrea::theme_crea() +
    labs(title="Fire radiative potential affecting each city",
         subtitle=sprintf("%d-day running average", running_days),
         y=NULL,
         x=NULL,
         caption="Source: CREA analysis based on MODIS and HYSPLIT trajectories flowing into each city.")

  f <- file.path("results","plots",
                 paste0(c("weather",fire_mode,running_days,"png"),collapse="."))

  ggsave(f, width=15, height=15)
}


plot.ts <- function(meas.dew,
                    mode,
                    fire_mode=NULL,
                    weather.fire=NULL,
                    running_days=30,
                    folder=dir_results_plots,
                    width=15, height=15,
                    nrow=NULL, ncol=NULL, ...){

  poll <- "pm25"
  if(mode=="trend"){
    m <- meas.dew %>%
      tidyr::unnest(normalised)
    scales <- "free_y"
    color_by <- NULL
  }else if(mode=="anomaly"){
    scales <- NULL
    color_by <- "value"
    m <- meas.dew %>%
      filter(output=="anomaly_vs_counterfactual") %>%
      tidyr::unnest(normalised)
  }

  m <-  m %>%
    mutate(location_name=location_id) %>%
    mutate(date=lubridate::date(date))


  plt <- rcrea::plot_recents(meas_raw=m,
                               running_days=running_days,
                               process_id="day_station_mad",
                               source="airvisual",
                               subfile_by="poll",
                               color_by=color_by,
                             subplot_by="location_name",
                             poll=poll,
                             date_from="2020-01-01"
    ) + facet_wrap(~location_name,
                   scales=scales,
                   nrow=nrow,
                   ncol=ncol) +
    scale_fill_manual(name=NULL, values=c("orange"))


    # We want to add lockdown stages behind curves
    `-.gg` <- function(plot, layer) {
      if (missing(layer)) {
        stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
      }
      if (!is.ggplot(plot)) {
        stop('Need a plot on the left side')
      }
      plot$layers = c(layer, plot$layers)
      plot
    }


    # Adding fire if given
    if(!is.null(weather.fire)){

      d.fire.scale <- weather.fire %>%
        select(station_id, meas_weather) %>%
        tidyr::unnest(meas_weather) %>%
        filter(lubridate::year(date) %in% seq(2019,2020)) %>%
        group_by(location_name=station_id) %>%
        group_by(location_name=station_id,
                 date=lubridate::floor_date(date, "week")) %>%
        summarise(value=sum(fire_frp)) %>%
        summarise(value.max=max(value))

      d.fire <- weather.fire %>%
        select(station_id, meas_weather) %>%
        tidyr::unnest(meas_weather) %>%
        filter(lubridate::year(date)==2020) %>%
        group_by(location_name=station_id,
                 date=lubridate::floor_date(date, "week")) %>%
        summarise(value=sum(fire_frp)) %>%
        left_join(d.fire.scale) %>%
        mutate(value.rel=value/value.max)


      plt <- plt - geom_bar(data=d.fire,
                      aes(date, value.rel, fill="Fire radiative power"),
                      stat="identity",
                      color="transparent") +
        scale_fill_manual(name=NULL, values=c("grey70","orange")) +
        theme(legend.position = "bottom")
    }

    # Adding lockdown stages
    lockdown_stages <- rcrea::utils.lockdown_stages(unique(m$country))

    l <- m %>%
      distinct(location_name, country) %>%
      left_join(lockdown_stages)


    if(color_by=="year"){
      l <- l %>% mutate(
        date_from='year<-'(date_from,0),
        date_to='year<-'(date_to,0))
      min_date <- as.POSIXct("0000-01-01", tz="UTC")
      max_date <- as.POSIXct("0001-01-01", tz="UTC")
    }else{
      min_date <- lubridate::date(as.POSIXct("2020-01-01", tz="UTC"))
      max_date <- lubridate::date(as.POSIXct("2021-01-01", tz="UTC"))
    }

    plt2 <- plt - geom_rect(data=l %>%
                              filter(indicator=="lockdown", !is.na(date_from)),
                            aes(xmin=lubridate::date(date_from),
                                xmax=lubridate::date(date_to),
                                ymin=-Inf,
                                ymax=+Inf,
                                fill=tools::toTitleCase(indicator)),
                            inherit.aes = F,
                            alpha=0.3) +
      scale_x_date(
        limits=c(min_date, max_date),
        breaks = seq(min_date,
                     max_date,
                     by="3 month"),
        minor_breaks = seq(min_date,
                           max_date,
                           by="1 month"),
        date_labels = "%b") +
      scale_y_continuous(labels=scales::percent) +
      theme(panel.grid.minor = element_line("grey95"),
            panel.grid.major = element_line("grey60"),
            legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 1)) +
      labs(caption=paste0(
      "Source: CREA based on AirVisual, Oxford COVID-19 Government Response Tracker",
      ifelse(!is.null(weather.fire), ", MODIS and HYSPLIT.","."),
      "\nLockdown is defined as periods when people are required not to leave house (C6 >=2).",
      ifelse(!is.null(weather.fire),"\nFire radiative power refers to the sum of weekly fires' radiative power along air trajectories leading to each city.",
             "")))


    print(plt2)
    fire_str <- if(is.null(weather.fire)) NULL else "fire"
    ggsave(filename=file.path("results","plots",
                              paste0(c("ts",mode,fire_mode,fire_str,
                                       "png"),collapse=".")),
           plot=plt2,
           width=width,
           height=height)
}


