#' Title
#'
#' @param mes
#' @param fire_mode NULL for no accounting for fire, or `circular` or `oriented`
#'
#' @return
#' @export
#'
#' @examples
calc.deweather <- function(mes, fire_mode, use_cache=T){

  if(is.null(fire_mode)){
    mode_str <- ""
  }else{
    mode_str <- paste0("_fire_",fire_mode)
  }

  f <- file.path("results","data",
                 paste0("meas_dew",mode_str,".RDS"))

  if(!use_cache | !file.exists(f)){
    # There was very little difference between lag of one or two days
    # although a bit more so in China (lag2<lag1)
    lags <- c(0)
    meas.dews <- lapply(lags, function(lag){
      deweather(meas=meas.clean,
                poll="pm25",
                output=c("trend"),
                add_pbl=T,
                upload_results=F,
                add_fire=!is.null(fire_mode),
                fire_mode=fire_mode,
                save_weather_filename=file.path("results","data",
                                                paste0("weather_",mode_str,".RDS")),
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
  return(meas.dew)
}

