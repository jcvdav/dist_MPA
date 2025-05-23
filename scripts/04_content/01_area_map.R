######################################################
#title#
######################################################
# 
# Purpose
#
######################################################


library(rnaturalearth)
library(sf)
library(here)
library(tidyverse)

# Spatial
mex_eez <- st_read(here("data", "processed", "mex_eez.gpkg"))
old_revilla <- st_read(dsn =  here("data", "processed", "revilla_old.gpkg"))
new_revilla <- st_read(dsn = here("data", "processed", "revilla_new.gpkg"))
ports <- st_read(dsn = here("data", "processed", "ports.gpkg"))
buffer <- st_buffer(new_revilla, dist = units::set_units(100, "nautical_miles"))

land <- ne_countries(continent = "North America", scale = "medium") |> 
  st_crop(st_buffer(mex_eez, dist = 50e3))

map_old <- ggplot() +
  geom_sf(data = mex_eez, fill = "transparent", color = "black", size = 0.3) +
  geom_sf(data = land, color = "black", size = 0.3) +
  geom_sf(data = ports, color = "black", fill = "steelblue", shape = 21, size = 3) +
  geom_sf(data = old_revilla, fill = "transparent", color = "red") +
  theme_void()

map_old

map_new <- map_old +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", linetype = "dashed")

map_new


# Map --------------------------------------------------------------------------
map <- ggplot() +
  geom_sf(data = mex_eez, fill = "transparent", color = "black", linewidth = 0.5) +
  geom_sf(data = land) +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_sf(data = buffer, fill = "transparent", color = "steelblue", linetype = "dotted", linewidth = 0.5) +
  theme_bw() +
  ggspatial::annotation_scale()

## EXPORT ######################################################################
ggsave(plot = map_old,
       filename = here("results", "img", "map_old_revilla.png"),
       width = 6,
       height = 4)

ggsave(plot = map_new,
       filename = here("results", "img", "map_new_revilla.png"),
       width = 6,
       height = 4)

ggsave(plot = map,
       filename = here("results", "img", "map.png"),
       width = 6,
       height = 4)

