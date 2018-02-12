station_id <- "CAW00064757"


get_station_data <- function(station_id){
  station_url <- paste0(
    "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/products/auxiliary/station/",
    station_id,
    ".normals.txt"
  )
  file_lines <- readLines(url(station_url))
  
  loc_of_temp <- file_lines %>% str_detect('mly-tmax-normal' ) %>% which()
  
  months <- c('Jan', 'Feb', "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  file_lines[loc_of_temp] %>% 
    strsplit('\\s+') %>% 
    .[[1]] %>% 
    .[-c(1,2)] %>% {
      data_frame(temp = ., month = months, station_id = station_id)
    }
}

stations <- read.csv('average_temp_max_by_season.csv')$station_id


stations_monthly <- stations %>% 
  purrr::map_df(get_station_data)


