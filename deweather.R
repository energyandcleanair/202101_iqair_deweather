library(creadeweather)
library(tmap)
library(lutz)
library(tidyverse)

source('data.R')
readRenviron(".Renviron")

meas <- data.meas()
meas.dew <- deweather(meas=meas,
                      poll="pm25",
                      output="anomaly"
                      add_pbl=T,
                      upload_results=F
                      )
