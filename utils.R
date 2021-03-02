utils.table_change <- function(meas, mode, meas.dew){

  meas <- meas %>%
    filter(!is.na(value))

  (changes.observed <- meas %>%
     filter(lubridate::year(date) %in% c(2019,2020)) %>%
     group_by(location_id, poll, year=lubridate::year(date)) %>%
     summarise(value=mean(value)) %>%
     tidyr::spread("year","value") %>%
     mutate(change=(`2020`-`2019`)/`2019`) %>%
     arrange(change) %>%
     mutate(change_str=paste0(ifelse(change>0,"+",""),scales::percent(change))))


  if(mode=="anomaly"){
    changes.dew <- meas.dew %>% filter(output==mode) %>%
       unnest(normalised) %>%
       filter(lubridate::year(date)==2020) %>%
       group_by(location_id, poll, lag) %>%
       summarise(observed=mean(predicted+value),
                 predicted=mean(predicted)) %>%
       mutate(change=(observed-predicted)/predicted) %>%
       arrange(change) %>%
       mutate(change_str=paste0(ifelse(change>0,"+",""), scales::percent(change)))
  }else if(mode=="trend"){
    changes.dew <- meas.dew %>% filter(output==mode) %>%
       unnest(normalised) %>%
       filter(lubridate::year(date) %in% c(2019,2020)) %>%
       group_by(location_id, poll, year=lubridate::year(date)) %>%
       summarise(value=mean(value)) %>%
       tidyr::spread("year","value") %>%
       mutate(change=(`2020`-`2019`)/`2019`) %>%
       arrange(change) %>%
       mutate(change_str=paste0(ifelse(change>0,"+",""), scales::percent(change)))
  }


  bind_rows(
    changes.observed %>% select(location_id, poll, change, change_str) %>% mutate(method="observed"),
    changes.dew %>% select(location_id, poll, change, change_str) %>% mutate(method="dew")
  ) %>% tidyr::pivot_wider(names_from="method", values_from = c(change, change_str)) %>%
    arrange(change_dew)
}

utils.lockdown_regions <- function(m){
  # Adding lockdown stages
  lockdown_region_id <- m$country
  # State level for US cities
  lockdown_region_id[m$location_id=="Los Angeles"] <- "US_CA"
  lockdown_region_id[m$location_id=="Chicago"] <- "US_IL"
  lockdown_region_id
}

utils.table_change_lockdown <- function(mode, meas.dew){


  meas.dew$lockdown_region_id <- utils.lockdown_regions(meas.dew)

  lockdown_stages <- rcrea::utils.lockdown_stages(unique(meas.dew$lockdown_region_id))
  lockdown_stages <- lockdown_stages %>%
    filter(date_from<="2020-12-31") %>%
    mutate(date_to=pmin(as.POSIXct("2020-12-31"), date_to)) %>%
    filter(indicator=="lockdown") %>%
    rowwise() %>%
    mutate(date=list(lubridate::date(seq(date_from,date_to,by="day")))) %>% tidyr::unnest(date)




  if(mode=="anomaly"){
    changes.dew <- meas.dew %>%
      filter(output==mode) %>%
      unnest(normalised) %>%
      select(location_id, lockdown_region_id, date, poll, value, predicted) %>%
      mutate(date=lubridate::date(date)) %>%
      filter(lubridate::year(date)==2020) %>%
      left_join(lockdown_stages %>% select(region_id, indicator, level, date),
                by=c("lockdown_region_id"="region_id", "date"="date")) %>%
      mutate(level=tidyr::replace_na(level, 0)) %>%
      group_by(location_id, poll, level) %>%
      summarise(observed=mean(predicted+value),
                predicted=mean(predicted),
                n_days=n()) %>%
      mutate(change=(observed-predicted)/predicted) %>%
      arrange(change) %>%
      mutate(change_str=paste0(ifelse(change>0,"+",""), scales::percent(change)))

  }else if(mode=="trend"){
    changes.dew <- meas.dew %>%
      filter(output==mode) %>%
      unnest(normalised) %>%
      select(location_id, lockdown_region_id, date, poll, value) %>%
      mutate(date=lubridate::date(date)) %>%
      filter(lubridate::year(date)%in%c(2019,2020)) %>%
      left_join(lockdown_stages %>% select(region_id, indicator, level, date),
                by=c("lockdown_region_id"="region_id", "date"="date")) %>%
      mutate(level=tidyr::replace_na(level, 0)) %>%
      group_by(location_id, poll, year=lubridate::year(date), level) %>%
      summarise(value=mean(value),
                n_days=n()) %>%
      tidyr::spread("year","value") %>%
      mutate(change=(`2020`-`2019`)/`2019`) %>%
      arrange(change) %>%
      mutate(change_str=paste0(ifelse(change>0,"+",""), scales::percent(change)))
  }


  changes.dew %>%
    select(location_id, poll, level, n_days, change, change_str) %>%
    arrange(location_id, level)

}
