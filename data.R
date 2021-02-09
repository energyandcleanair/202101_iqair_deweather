
data.meas <- function(){

  f.cities <- file.path("data","cities_filled.csv")


  d <- readr::read_csv("data/data.csv")

  # Get cities and location
  if(!file.exists(f.cities)){
    library(tmap)
    library(lutz)
    cities <- readxl::read_xlsx("data/cities.xlsx", skip = 2)
    cities$address <- paste(cities$City, cities$Country, sep=", ")
    coords <- tmaptools::geocode_OSM(cities$address)
    cities$lon <- coords$lon
    cities$lat <- coords$lat
    cities$timezone <- tz_lookup_coords(coords$lat, coords$lon, method = "accurate")
    write.csv(cities, f.cities, row.names = F)
  }else{
    cities <- read.csv(f.cities)
  }

  # d
  meas <- d %>%
    select(location_id=city, date=ts, pm25) %>%
    tidyr::pivot_longer(cols=pm25, names_to="poll") %>%
    left_join(cities %>% select(location_id=City, country=Country, timezone, lat, lon)) %>%
    mutate(sf::st_as_sf(., coords = c("lon", "lat"))) %>%
    mutate(process_id="day_station_mad",
           unit="µg/m3",
           source="airvisual"
           )

  return(meas)

}

data.countries <- function(meas){
  meas %>%
    distinct(location_id, country) %>%
    mutate(country_name=country,
           country=countrycode::countrycode(country, "country.name", "iso2c",
                                            custom_match = c("Columbia"="CO")))
}

data.clean_meas <- function(meas){

  # Removing peaks in Los Angeles, Melbourne, probably corresponding to wildfires
  # cut_levels <- tibble(
  #   location_id=c("Los Angeles", "Melbourne", "Tel Aviv-Yafo", "Singapore", "Kano", "Dubai", "Bogota", "Buenos Aires", "Bangkok"),
  #   cut_level=c(40, 75, 150, 40, 500, 200, 100,100,150),
  #   cut_date_from=c("2020-01-01", "2019-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01")
  # )
  cut_levels <- tibble(
    location_id=c("Melbourne", "Tel Aviv-Yafo", "Singapore", "Kano", "Dubai", "Bogota", "Buenos Aires", "Bangkok"),
    cut_level=c(75, 150, 60, 500, 200, 100, 100, 150),
    cut_date_from=c("2019-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01", "2017-01-01"),
    cut_date_to="2019-12-31"
  )

  meas %>%
    left_join(cut_levels) %>%
    filter(
      is.na(cut_level) |
        (date < as.POSIXct(cut_date_from)) |
        (date > as.POSIXct(cut_date_to)) |
        (value<cut_level)
    )
}
