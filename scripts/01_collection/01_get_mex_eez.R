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

eez <- mr_shp(key = "MarineRegions:eez_iho_union_v2", read = TRUE, maxFeatures = 2e3) %>% 
  st_as_sf() %>% 
  filter(country == "Mexico") %>% 
  group_by(country) %>% 
  summarize(a = 1) %>% 
  dplyr::select(-a)

st_write(eez, dsn = here("data", "processed_data", "mex_ees.gpkg"))
