################################################################################
# title
################################################################################
# 
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################
library(rmapshaper)
library(sf)
library(tidyverse)

papa <- st_read(dsn = "/Users/juancarlosvillasenorderbez/GitHub/dist_MPA/data/raw_data/WDPA_WDOECM_Oct2022_Public_555670040_shp/WDPA_WDOECM_Oct2022_Public_555670040_shp_0",
                layer = "WDPA_WDOECM_Oct2022_Public_555670040_shp-polygons") %>% 
  select(wdpa_id = WDPAID)

mpas <- st_read(dsn = "/Users/juancarlosvillasenorderbez/Library/CloudStorage/GoogleDrive-juancarlos@ucsb.edu/Shared drives/emlab/data/mpa-atlas/mpatlas_20201223_clean",
                layer = "mpatlas_20201223_clean")

focal <- c("revilla" = 555629385,
           "pipa" = 309888,
           "palau" = 555622118,
           "papa" = 555670040)

selected <- mpas %>% 
  filter(wdpa_id %in% focal) %>% 
  group_by(wdpa_id) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  select(-a) %>% 
  rbind(papa) %>% 
  ms_simplify() %>% 
  startR::st_rotate()

countries <- ne_coastline(returnclass = "sf") %>% 
  startR::st_rotate() 

ggplot() +
  geom_sf(data = countries, size = 0.2, color = "black") +
  geom_sf(data = selected, size = 0.1, color = "black", fill = "steelblue") +
  cowplot::theme_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
