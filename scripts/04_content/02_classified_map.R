

library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Tabular
scored <- readRDS(here("data", "processed", "scored_tracks.rds"))

# Spatial
mex <- ne_countries(country = "Mexico", returnclass = "sf", scale = "medium")
mex_eez <- st_read(here("data", "processed", "mex_eez.gpkg"))
old_revilla <- st_read(dsn =  here("data", "processed", "revilla_old.gpkg"))
new_revilla <- st_read(dsn = here("data", "processed", "revilla_new.gpkg"))
ports <- st_read(dsn = here("data", "processed", "ports.gpkg"))

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
  count(year, lon_bin, lat_bin) %>% 
  group_by(lon_bin, lat_bin) %>% 
  summarize(n = mean(n))

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
  labs(title = "Average activity",
       fill = "Hours")

# Export figure
ggsave(plot = total_hours_before,
       filename = here("results", "img", "total_hours_before_map.png"),
       width = 6,
       height = 3)

## ZOOMED IN MAP OF EFFORT FISHING/NOT FISHING _________________________________

most <- "00000778"
never <- "00100164"

two_tracks_before <- before %>% 
  filter(vessel_rnpa %in% c(most, never),
         kmeans_fishing)

two_tracks_after <- after %>% 
  filter(vessel_rnpa %in% c(most, never),
         kmeans_fishing)

before_pct_inside <- two_tracks_before %>% 
  mutate(inside = between(lon, revilla_bbox[1], revilla_bbox[3]) & between(lat, revilla_bbox[2], revilla_bbox[4])) %>% 
  group_by(name, inside) %>% 
  summarize(h = sum(hours)) %>% 
  pivot_wider(names_from = inside,
              values_from = h) %>% 
  mutate(pct = (`TRUE` / (`TRUE` + `FALSE`)) * 100)

tracks_before <- ggplot() +
  geom_point(data = two_tracks_before,
             mapping = aes(x = lon, y = lat, color = name),
             shape = ".") +
  geom_sf(data = new_revilla, fill = "transparent", color = "black", size = 1) +
  geom_sf(data = mex, fill = "gray", color = "black", size = 0.3) +
  theme_void() +
  theme(legend.position = "None") +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_color_brewer(palette = "Set1")


tracks_after <- ggplot() +
  geom_point(data = two_tracks_after,
             mapping = aes(x = lon, y = lat, color = name),
             shape = ".") +
  geom_sf(data = new_revilla, fill = "transparent", color = "black", size = 1) +
  geom_sf(data = mex, fill = "gray", color = "black", size = 0.3) +
  theme_void() +
  theme(legend.position = "None") +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_color_brewer(palette = "Set1")

cowplot::plot_grid(tracks_before, tracks_after, ncol = 2, labels = c("Before", "After"))

# Classified map

before_zoom <- before %>% 
  filter(between(lon, revilla_bbox[1] - 1, revilla_bbox[3] + 1),
         between(lat, revilla_bbox[2] - 1, revilla_bbox[4] + 1),
         vessel_rnpa == most)

unclassified <- ggplot() +
  geom_point(data = before_zoom,
             mapping = aes(x = lon, y = lat),
             shape = ".") +
  geom_sf(data = new_revilla, fill = "transparent", color = "black", size = 1) +
  theme_minimal() +
  theme(legend.position = "None") +
  labs(title = "Vessel activity by Madeira before the Revilla expansion",
       x = "Longitude",
       y = "Latitude") +
  scale_color_brewer(palette = "Set1")

classified <- ggplot() +
  geom_point(data = before_zoom,
             mapping = aes(x = lon, y = lat, color = factor(kmeans_fishing)),
             size = 0.2) +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", linewidth = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Vessel activity before the area was protected",
       x = "Longitude",
       y = "Latitude",
       color = "Likely fishing") +
  scale_color_manual(values = c("gray", "red"))

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
  filter(k2means_fishing)


ggplot() +
  geom_hex(data = fishing_before,
             mapping = aes(x = lon, y = lat), binwidth = 0.5, color = "transparent") +#,
             # pch = ".",
             # color = "black") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  scale_fill_continuous(trans = "log10") +
  theme_void() +
  theme(legend.position = "None")

fishing_after <- after %>% 
  filter(k2means_fishing)


ggplot() +
  geom_hex(data = fishing_after,
           mapping = aes(x = lon, y = lat), binwidth = 0.5, color = "transparent") +#,
  # pch = ".",
  # color = "black") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  scale_fill_continuous(trans = "log10") +
  theme_void() +
  theme(legend.position = "None")





