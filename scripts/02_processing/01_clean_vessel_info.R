# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
raw_tracks <- readRDS(file = here("data", "raw", "raw_tracks.rds"))

vessel_info <- readRDS(file = here("data", "raw", "vessel_info.rds"))

## PROCESSING ##################################################################

# Assign treated / control  ----------------------------------------------------
# These are vessels that fishied within Revilla before it was implemented
treated <- raw_tracks %>%
  filter((
    between(lon, lon_range[1], lon_range [2]) &
      between(lat, lat_range[1], lat_range[2])
  ),
  lubridate::ym(paste(year, month)) < "2017-11-27") %>%
  pull(vessel_rnpa) %>%
  unique()

# Identify first appearance of the vessel
first <- tracks %>% 
  group_by(vessel_rnpa) %>% 
  summarize(first_year = min(year)) %>% 
  arrange(first_year) %>% 
  filter(first_year <= 2017)

clean_vessel_info <- vessel_info %>%
  select(eu_rnpa,
         vessel_rnpa,
         home_port,
         construction_year,
         preservation_system,
         detection_gear,
         vessel_length_m,
         engine_power_hp) %>%
  mutate(displaced = 1 * (vessel_rnpa %in% treated)) %>% 
  mutate(disp = ifelse(displaced == 1, "Displaced", "Not displaced"),
         disp = fct_reorder(disp, displaced)) %>% 
  inner_join(first, by = "vessel_rnpa")

## EXPORT ######################################################################

# Export vessel info -----------------------------------------------------------
saveRDS(object = clean_vessel_info,
        file = here("data", "processed", "clean_vessel_info.rds"))
