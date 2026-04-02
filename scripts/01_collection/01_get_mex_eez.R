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
library(mregions2)
library(tidyverse)

## PROCESSING ##################################################################

# Get EEZ ----------------------------------------------------------------------
eez <- gaz_geometry(x = 8429, format = "sf") |> 
  st_make_valid() |> 
  st_simplify(preserveTopology = T, dTolerance = 500) |> 
  st_make_valid()

# Export geopackage ------------------------------------------------------------
st_write(obj = eez,
         dsn = here("data", "processed", "mex_eez.gpkg"),
         delete_dsn = T)
