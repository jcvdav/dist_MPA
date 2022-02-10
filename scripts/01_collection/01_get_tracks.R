######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

## Set up #############################################################################################################################################################################
# Load packages
library(here)
library(connections)
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
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery
mex_fisheries <- connection_open(
  bigquery(),
  project = "emlab-gcp",
  dataset = "mex_fisheries",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

vessel_info <- tbl(mex_fisheries, "vessel_info") %>% 
  filter(tuna == 1,
         home_port %in% ports,
         str_detect(gear_type, "CERCO")) %>% 
  select(eu_rnpa, vessel_rnpa, owner_rnpa, hull_identifier, tuna, sardine, shrimp, home_port, contains("num"), engine_power_hp)


tracks <- tbl(mex_fisheries, "mex_vms") %>% 
  filter(speed > 0) %>% 
  select(name, vessel_rnpa, year, month, datetime, lon, lat, speed) %>%
  inner_join(vessel_info, by = "vessel_rnpa")

local_tracks <- tracks %>% 
  collect(page_size = 17e3)

# Export
saveRDS(object = local_tracks,
        file = here("data", "raw_data", "raw_tracks.rds"))
