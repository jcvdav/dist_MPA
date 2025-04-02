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
library(fasterize)
library(raster)
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------
rev <- st_read(dsn = here("data", "processed", "revilla_new.gpkg"))

## PROCESSING ##################################################################

# Reference raster -------------------------------------------------------------
reference_raster <-
  raster(
    xmn = -160,
    xmx = -70,
    ymn = -25,
    ymx = 40,
    resolution = 0.5
  )

# Rasterize MPA ----------------------------------------------------------------
mpa_raster <- fasterize(
  sf = rev,
  raster = reference_raster,
  background = NA
)

# Calculate distance in kilometers ---------------------------------------------
dist_to_mpa_raster <- distance(mpa_raster) / 1e3

# Convert to data.frame --------------------------------------------------------
dist_df <- as.data.frame(dist_to_mpa_raster, xy = T) %>% 
  drop_na() %>% 
  rename(lon = x, lat = y, dist = layer) %>% 
  filter(dist > 0)

## EXPORT ######################################################################

# Export the data --------------------------------------------------------------
saveRDS(object = dist_df,
        file = here("data", "processed", "distance_to_MPA.rds"))