#' Title
#'
#' @param mes
#' @param fire_mode NULL for no accounting for fire, or 'circular', 'oriented', or 'trajectory'
#'
#' @return
#' @export
#'
#' @examples
calc.deweather <- function(m, mode, fire_mode, lag, use_cache=F){

  if(is.null(fire_mode)){
    fire_mode_str <- NULL
  }else{
    fire_mode_str <- paste0("fire.",fire_mode)
  }

  f <- file.path("results","data",
                 paste0(c("meas.dew",
                          mode,
                          fire_mode_str,
                          paste0("lag",lag),
                          "RDS"),
                        collapse="."))

  if(!use_cache | !file.exists(f)){
    # There was very little difference between lag of one or two days
    # although a bit more so in China (lag2<lag1)
    meas.dew <- deweather(
      meas=m,
      lag=lag,
      poll="pm25",
      output=mode,
      add_pbl=T,
      upload_results=F,
      add_fire=!is.null(fire_mode),
      fire_mode=fire_mode,
      save_weather_filename=file.path("results","data",
                                      paste0(c("weather",mode,fire_mode_str,"RDS"), collapse="."))) %>%
        mutate(lag=!!lag)

    saveRDS(meas.dew, f)

  }else{
    meas.dew <- readRDS(f)
  }
  return(meas.dew %>%
           left_join(meas %>% distinct(location_id, country, country_name)))
}

