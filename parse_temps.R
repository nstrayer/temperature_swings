library(tidyverse)

extract_temps_by_period <- function(period){
  paste0(
    "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/products/temperature/",
    period, 
    "-tmax-normal.txt") %>% 
  read_table(col_names = F) %>% 
  mutate(
  quality = str_extract(X2, '[A-Z]'),
  temp = as.numeric(str_replace(X2, '[A-Z]', '')),
  period = period
  ) %>% 
  select(station_id = X1, quality, temp, period)
}

periods <- c('djf', 'mam', 'jja', 'son')

average_max_by_period <- periods %>% 
  map_df(extract_temps_by_period)

average_max_by_period %>% write_csv('average_temp_max_by_season.csv')


# load stations info file to get locations for averages

column_names <- c('ID',           
'LATITUDE',     
'LONGITUDE',    
'ELEVATION',    
'STATE',        
'NAME',         
'GSNFLAG',      
'HCNFLAG',      
'WMOID',        
'METHOD')

station_info <- read_table('allstations.txt', col_names = column_names) %>% 
  select(station_id = ID, lat = LATITUDE, lon = LONGITUDE, state = STATE)


# for each station find the temp delta between the largest and smallest average temp
swing_by_station <- average_max_by_period %>% 
  group_by(station_id) %>% 
  summarise(swing = max(temp) - min(temp)) %>% 
  right_join(station_info, by = 'station_id') %>% 
  filter(!(is.na(swing) | is.na(lat))) %>% 
  filter(lon < -60 & lon > -130 & lat > 20) %>% 
  group_by(lat, lon) %>% 
  summarise(swing = mean(swing))
  
write_csv(swing_by_station, 'swing_by_station.csv')

ggplot(swing_by_station, aes(x = lon, y = lat, color = swing)) +
  geom_point(size = 5, alpha = 0.1) +
  scale_colour_distiller(palette = 8, direction = 1) 
# +
#   theme_void()

ggplot(swing_by_station, aes(x = lon, y = lat, z = swing)) +
  geom_tile(aes(fill = swing)) +
  stat_contour() 


library(akima)

grid_dim <- 150

gridded <- swing_by_station %$%
  interp(
    x = lon, y = lat, z = swing, 
    nx = grid_dim, ny = grid_dim) 

contourplot(z ~ x+y, data=gridded)


interpolated <- gridded$z %>% 
  as_data_frame() %>% {
    colnames(.) <- gridded$x
    .
  } %>% 
  gather(lon, swing) %>% 
  mutate(
    lat = rep(gridded$y, times = grid_dim),
    lon = as.numeric(lon)
  ) %>% 
  filter(!is.na(swing))

ggplot(interpolated, aes(x = lon, y = lat, color = swing)) +
  scale_colour_distiller(palette = 8, direction = 1) +
  geom_point()+
  borders("state") 
  

ggplot(interpolated, aes(x = lon, y = lat, z = swing)) +
  #geom_tile(aes(fill = swing)) +
  scale_colour_distiller(palette = 8, direction = 1) +
  stat_contour() 

