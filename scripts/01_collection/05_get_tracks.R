######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## Set up ######################################################################
# Load packages
library(here)
library(DBI)
library(bigrquery)
library(tidyverse)

# Define some info
ports <- c("ENSENADA",
           "SAN BLAS",
           "MAZATLAN",
           "EL SAUZAL",
           "PUERTO MADERO",
           "MANZANILLO",
           "CHIAPAS",
           "SAN CARLOS")

# Authenticate using local token 
bq_auth("juancarlos.villader@gmail.com")

# Establish a connection to BigQuery
mex_fisheries <- dbConnect(
  drv = bigquery(),
  project = "mex-fisheries",
  dataset = "mex_vms",
  billing = "mex-fisheries",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

vessel_info <- tbl(mex_fisheries, "vessel_info_v_20230803") %>% 
  filter(tuna == 1,
         shrimp == 0,
         sardine == 0,
         home_port %in% ports,
         str_detect(gear_type, "CERCO")) %>% 
  select(eu_rnpa, vessel_rnpa, owner_rnpa, hull_identifier, home_port, contains("num"), engine_power_hp)


tracks <- tbl(mex_fisheries, "mex_vms_processed_v_20250319") %>% 
  select(vessel_rnpa, seg_id, point_in_seg, datetime, lon, lat, implied_speed_knots, course, distance_to_last_m, hours) %>%
  inner_join(vessel_info, by = "vessel_rnpa")

local_tracks <- tracks %>% 
  collect(page_size = 17e3)

# Export
saveRDS(object = local_tracks,
        file = here("data", "raw", "raw_tracks.rds"))
