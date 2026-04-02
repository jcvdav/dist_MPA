######################################################
#title#
######################################################
# 
# Scores poitns as fishing / not fishing
#
######################################################

library(here)
library(tidyverse)

raw_tracks <- readRDS(here("data", "raw", "raw_tracks.rds")) |> 
  drop_na(implied_speed_knots, course, distance_to_last_m)

mat <- raw_tracks |> 
  select(implied_speed_knots, course, distance_to_last_m) |> 
  as.matrix()
  
k_means <- kmeans(x = mat, centers = 2, nstart = 10)

scored <- raw_tracks |> 
  mutate(kmeans_fishing = fitted(k_means, "class") == 2,
         speed_fishing = between(implied_speed_knots, 1, 12),
         fishing = kmeans_fishing & speed_fishing,
         year = lubridate::year(datetime),
         month = lubridate::month(datetime),
         inside = between(lon, lon_range[1], lon_range[2]) &
           between(lat, lat_range[1], lat_range[2]),
         after = year > 2017,
         before = !after)

saveRDS(object = scored,
        file = here("data", "processed", "scored_tracks.rds"))
