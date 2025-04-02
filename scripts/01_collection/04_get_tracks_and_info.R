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
library(DBI)
library(bigrquery)
library(tidyverse)

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
mex_fisheries <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "mex_fisheries",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

## PROCESSING ##################################################################

# Define tables ----------------------------------------------------------------

# Vessel info table
vessel_info <- tbl(mex_fisheries, "vessel_info_v_20221104") %>%
  filter(tuna == 1,
         str_detect(gear_type, "CERCO")) %>%
  select(eu_rnpa,
         vessel_rnpa,
         owner_rnpa,
         hull_identifier,
         tuna,
         sardine,
         shrimp,
         home_port,
         construction_year,
         preservation_system,
         detection_gear,
         vessel_length_m,
         vessel_gross_tonnage,
         engine_power_hp,
         imputed_engine_power)

# Tracks table
tracks <- tbl(mex_fisheries, "mex_vms_processed_v_20221104") %>%
  inner_join(vessel_info, by = "vessel_rnpa") %>%
  filter(year <= 2021) %>% 
  select(seg_id,
         name,
         vessel_rnpa,
         year,
         month,
         datetime,
         lon,
         lat,
         contains("distance"),
         sea,
         eez,
         speed,
         course,
         hours)

# Collect tables ---------------------------------------------------------------

# Vessel info
local_info <- vessel_info %>%
  collect()

# Tracks
local_tracks <- tracks %>%
  collect()

## EXPORT ######################################################################

# Vessel info ------------------------------------------------------------------
saveRDS(object = local_info,
        file = here("data", "raw", "vessel_info.rds"))

# Tracks -----------------------------------------------------------------------
saveRDS(object = local_tracks,
        file = here("data", "raw", "raw_tracks.rds"))
