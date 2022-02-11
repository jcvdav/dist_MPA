######################################################
#title#
######################################################
# 
# Purpose
#
######################################################


library(rnaturalearth)
library(tidyverse)

# Spatial
mex <- ne_countries(country = "Mexico", returnclass = "sf", scale = "medium")
mex_eez <- st_read(here("data", "processed_data", "mex_ees.gpkg"))
old_revilla <- st_read(dsn =  here("data", "processed_data", "revilla_old.gpkg"))
new_revilla <- st_read(dsn = here("data", "processed_data", "revilla_new.gpkg"))
ports <- st_read(dsn = here("data", "processed_data", "ports.gpkg"))

map_old <- ggplot() +
  geom_sf(data = mex_eez, fill = "transparent", color = "black", size = 0.3) +
  geom_sf(data = mex, color = "black", size = 0.3) +
  geom_sf(data = ports, color = "black", fill = "steelblue", shape = 21, size = 3) +
  geom_sf(data = old_revilla, fill = "transparent", color = "red") +
  theme_void()

map_old

map_new <- map_old +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", linetype = "dashed")

map_new

ggsave(plot = map_old,
       filename = here("results", "img", "map_old_revilla.png"),
       width = 6,
       height = 4)

ggsave(plot = map_new,
       filename = here("results", "img", "map_new_revilla.png"),
       width = 6,
       height = 4)
