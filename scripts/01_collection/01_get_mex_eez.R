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
library(sf)
library(mregions)
library(tidyverse)

# Load data --------------------------------------------------------------------
eez <-
  mr_shp(key = "MarineRegions:eez_iho_union_v2",
         read = TRUE,
         maxFeatures = 2e3) %>%
  st_as_sf()

## PROCESSING ##################################################################

# Get EEZ ----------------------------------------------------------------------
eez_filtered <- eez %>%
  filter(country == "Mexico") %>%
  group_by(country) %>%
  summarize(a = 1) %>%
  dplyr::select(-a)

## EXPORT ######################################################################

# Export geopackage ------------------------------------------------------------
st_write(eez_filtered,
         dsn = here("data", "processed", "mex_eez.gpkg"))
