
library(here)
library(lubridate)
library(tidyverse)

get_distances <- function(data) {
  # browser()
  data %>% 
    mutate(dist = map_dbl(.x = .$geometry, .f = ~sort(st_distance(.x, data))[2])) %>% 
    st_drop_geometry()
}

scored <- readRDS(here("data", "processed_data", "scored_tracks.rds"))


d <- scored %>% 
  filter(between(lon, revilla_bbox[1] - 10, revilla_bbox[3] + 10),
         between(lat, revilla_bbox[2] - 10, revilla_bbox[4] + 10)) %>% 
  mutate(date = lubridate::date(datetime)) %>% 
  filter(kmeans_fishing) %>% 
  mutate(hour = lubridate::hour(datetime)) %>% 
  filter(hour == 13) %>% 
  select(vessel_rnpa, date, lat, lon) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat")) %>% 
  group_by(date) %>% 
  nest() %>% 
  mutate(dist = map(data, get_distances)) %>% 
  unnest(dist) %>% 
  mutate(year = lubridate::year(date))

d %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = date, y = dist, group = date)) + 
  stat_summary(geom = "point", fun = "mean") +
  geom_vline(xintercept = lubridate::ymd(c("2017-11-27"))) +
  facet_wrap(~year, ncol = 3, scales = "free_x")
  


sort(st_distance(sp[1,], sp))[2]
