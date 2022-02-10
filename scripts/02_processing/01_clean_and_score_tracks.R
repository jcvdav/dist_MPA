######################################################
#title#
######################################################
# 
# Scores poitns as fishing / not fishing
#
######################################################

library(here)
library(tidyverse)

raw_tracks <- readRDS( here("data", "raw_data", "raw_tracks.rds"))

mat <- raw_tracks %>% 
  select(year, speed)
  
k_means <-  kmeans(x = mat, centers = 2)

scored <- raw_tracks %>% 
  mutate(inside = (between(lon, -115.471415, -110.078093) & between(lat, 17.655231, 20.008631)),
         before = datetime < "2017-11-27",
         before = ifelse(is.na(before) & year >= 2017 & month >= 11, F, before),
         before = ifelse(is.na(before) & year <= 2017 & month <= 10, T, before)) %>% 
  mutate(kmeans_fishing = fitted(k_means, "class") == 2,
         speed_fishing = between(speed, 1, 12))

count(scored, kmeans_fishing, speed_fishing) %>% 
  spread(speed_fishing, n, fill = 0)

saveRDS(object = scored,
        file = here("data", "processed_data", "scored_tracks.rds"))
