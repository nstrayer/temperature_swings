library(rvest)
library(tidyverse)
library(purrr)


# load in zip codes
zip_codes <- read_csv('us_postal_codes.csv') %>%  
  # we only want the 5 digit zip codes for our API
  filter(nchar(as.character(`Zip Code`)) == 5) %>% 
  # Some places may be big enough for two zips, we don't need these so filter out
  # by grouping and taking the first entry
  group_by(`Place Name`, State, County) %>% 
  summarise(
    zip = first(`Zip Code`),
    lat = first(Latitude),
    lon = first(Longitude)
  ) %>% 
  ungroup() %>% 
  # clean up column names to be consistantly lowercase
  rename(place = `Place Name`, state = State, county = County)


# take random sample of 10 zips to test 
weather_by_zip <- zip_codes %>% 
  sample_n(10) %>% 
  .$zip %>% 
  map_df(get_average_by_zip)

zip_list <- zip_codes %>% sample_n(2) %>% .$zip

get_average_by_zip(54128)

all_data <- get_average_by_zip(49240)
for (zip_code in zip_list){
  print(zip_code)
  try({
    all_data <- all_data %>% bind_rows(get_average_by_zip(zip_code))
  })
}
