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

# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
raw_tracks <- readRDS( here("data", "raw", "raw_tracks.rds")) %>% 
  rename(longitude = lon, latitude = lat) %>% 
  select(-contains("distance"), sea, eez)

wrap_preprocessing_vms <- function(vms){
  mx_inland <- dafishr::mx_inland
  mx_ports <- dafishr::mx_ports
  all_mpas <- dafishr::all_mpas
  
  vms %>% 
    clean_land_points(mx_inland) %>% 
    join_ports_locations(mx_ports)
  
}

## PROCESSING ##################################################################

plan(multicore, workers = 15)

tic()
tracks <- raw_tracks %>% 
  group_by(year, month) %>% 
  nest() %>% 
  mutate(data = future_map(data, wrap_preprocessing_vms))
toc()

# Save progress just in case
saveRDS(object = tracks,
        file = here("data", "processed", "preprocessed_tracks.rds"))

# Assign info to each ping -----------------------------------------------------
clean_tracks <- tracks %>% 
  unnest(data) %>% 
  rename(lon = longitude,
         lat = latitude) %>% 
  mutate(inside = 1 * ((between(lon, lon_range[1], lon_range[2]) &
                          between(lat, lat_range[1], lat_range[2]))),
         after = 1 * (!lubridate::ym(paste(year, month)) < "2017-11-27"),
         fishing = 1 * (speed <= 5)) %>% 
  mutate(aft = ifelse(after == 0, "Before", "After"),
         aft = fct_reorder(aft, after))

## EXPORT ######################################################################

# Clean tracks -----------------------------------------------------------------
saveRDS(object = clean_tracks,
        file = here("data", "processed", "clean_tracks.rds"))
