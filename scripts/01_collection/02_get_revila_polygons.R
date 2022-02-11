######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(sf)
library(mregions)
library(tidyverse)

mpa <- st_read(dsn = "/Volumes/GoogleDrive/Shared drives/emlab/data/mpa-atlas/mpatlas_20201223_clean",
               layer = "mpatlas_20201223_clean")
  

revilla_old <- mpa %>% 
  filter(mpa_id == 5344) %>% 
  group_by(name) %>% 
  summarize(a = 1) %>% 
  select(-a)

# New polygon
# Coordinates com from the DOF decree at:
# http://www.dof.gob.mx/nota_detalle.php?codigo=5505736&fecha=27/11/2017
revilla_p <- tibble(x = c(-115.471415, -115.471415, -110.078093, -110.078093, -115.471415),
                    y = c(17.655231, 20.008631, 20.008631, 17.655231, 17.655231)) %>% 
  as.matrix() %>% 
  list() %>% 
  st_polygon() %>%
  st_sfc()

revilla_new <- tibble(name = "Revillagigedo") %>% 
  mutate(geom = revilla_p) %>% 
  st_as_sf(crs = 4326)




# Export
st_write(revilla_old, dsn = here("data", "processed_data", "revilla_old.gpkg"), delete_dsn = T)
st_write(revilla_new, dsn = here("data", "processed_data", "revilla_new.gpkg"), delete_dsn = T)




