
data.meas <- function(){

  f.cities <- file.path("data","cities_filled.csv")


  d <- readr::read_csv("data/data.csv")

  # Get cities and location
  if(!file.exists(f.cities)){
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
