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
