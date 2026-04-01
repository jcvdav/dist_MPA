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
         speed_fishing = between(implied_speed_knots, 1, 12)) |> 
  arrange(kmeans_fishing)

ggplot(scored |> filter(lubridate::year(datetime) > 2020, vessel_rnpa == "00043018")) +
  geom_point(aes(x = lon, y = lat, color = kmeans_fishing), pch = ".") +
  coord_equal() +
  facet_wrap(~lubridate::year(datetime))

saveRDS(object = scored,
        file = here("data", "processed_data", "scored_tracks.rds"))
