

library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Tabular
scored <- readRDS(here("data", "processed_data", "scored_tracks.rds"))

# Spatial
mex <- ne_countries(country = "Mexico", returnclass = "sf", scale = "medium")
mex_eez <- st_read(here("data", "processed_data", "mex_ees.gpkg"))
old_revilla <- st_read(dsn =  here("data", "processed_data", "revilla_old.gpkg"))
new_revilla <- st_read(dsn = here("data", "processed_data", "revilla_new.gpkg"))
ports <- st_read(dsn = here("data", "processed_data", "ports.gpkg"))

revilla_bbox <- st_bbox(new_revilla)

before <- scored %>% 
  filter(lat < 90) %>% 
  filter(before)

after <- scored %>% 
  filter(lat < 90) %>% 
  filter(!before)

## PROCECSSING #################################################################

## MAP OF ALL EFFORT BEFORE THE EXPANSION ______________________________________

# Prep data
bined_before <- before %>% 
  mutate(lon_bin = (round(lon / 0.5) + 0.25) * 0.5,
         lat_bin = (round(lat / 0.5) + 0.25) * 0.5) %>% 
  filter(speed_fishing) %>% 
  count(lon_bin, lat_bin)

# Make figure
total_hours_before <- ggplot() + 
  geom_tile(data = bined_before,
            mapping = aes(x = lon_bin, y = lat_bin, fill = n)) +
  geom_sf(data = mex_eez, fill = "transparent", color = "black", size = 0.3) +
  geom_sf(data = mex, color = "black", size = 0.3) +
  geom_sf(data = ports, color = "black", fill = "steelblue", shape = 21, size = 3) +
  # geom_sf(data = old_revilla, fill = "transparent", color = "red") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  theme_void() +
  scale_fill_continuous(trans = "log10") +
  labs(fill = "Total Hours")

# Export figure
ggsave(plot = total_hours_before,
       filename = here("results", "img", "total_hours_before_map.png"),
       width = 6,
       height = 4)

## ZOOMED IN MAP OF EFFORT FISHING/NOT FISHING _________________________________

most <- "00000778"
never <- "00041632"

most_before_zoom <- before %>% 
  filter(between(lon, revilla_bbox[1] - 1, revilla_bbox[3] + 1),
         between(lat, revilla_bbox[2] - 1, revilla_bbox[4] + 1),
         vessel_rnpa == most)

unclassified <- ggplot() +
  geom_point(data = most_before_zoom,
             mapping = aes(x = lon, y = lat),
             size =  0.2, color = "steelblue") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  theme_void()

classified <- ggplot() +
  geom_point(data = most_before_zoom,
             mapping = aes(x = lon, y = lat, color = factor(kmeans_fishing)),
             size = 0.2) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  theme_void() +
  theme(legend.position = "None")

ggsave(plot = unclassified,
       filename = here("results", "img", "most_unclassified_before.png"),
       width = 6,
       height = 4)

ggsave(plot = classified,
       filename = here("results", "img", "most_classified_before.png"),
       width = 6,
       height = 4)

## FISHING EFFORT MAP __________________________________________________________

fishing_before <- before %>% 
  filter(kmeans_fishing)


ggplot() +
  geom_hex(data = fishing_before,
             mapping = aes(x = lon, y = lat), binwidth = 0.5, color = "transparent") +#,
             # pch = ".",
             # color = "black") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  scale_fill_continuous(trans = "log10") +
  theme_void() +
  theme(legend.position = "None")






