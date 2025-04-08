######################################################
#title#
######################################################
# 
# Scores poitns as fishing / not fishing
#
######################################################

library(here)
library(tidyverse)

raw_tracks <- readRDS(here("data", "raw_data", "raw_tracks.rds")) |> 
  drop_na(implied_speed_knots, course, distance_to_last_m)

mat <- raw_tracks |> 
  select(implied_speed_knots, course, distance_to_last_m) |> 
  as.matrix()
  
k_means <- kmeans(x = mat, centers = 2, nstart = 10)

scored <- raw_tracks |> 
  mutate(inside = (between(lon, -115.471415, -110.078093) & between(lat, 17.655231, 20.008631)),
         before = datetime < "2017-11-27",
         before = ifelse(is.na(before) & ((year >= 2018) | (year == 2017& month >= 11)), F, before),
         before = ifelse(is.na(before) & (year <= 2016 | year <= 2017 & month <= 10), T, before)) |> 
  mutate(kmeans_fishing = fitted(k_means, "class") == 1,
         speed_fishing = between(implied_speed_knots, 1, 12)) |> 
  arrange(kmeans_fishing)

ggplot(scored |> filter(inside, year == 2015)) +
  geom_point(aes(x = lon, y = lat, color = kmeans_fishing), pch = ".") +
  coord_equal() +
  facet_grid()

saveRDS(object = scored,
        file = here("data", "processed_data", "scored_tracks.rds"))
