######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(sf)
library(mregions2)
library(tidyverse)

eez <- gaz_geometry(x = 8429, format = "sf") |> 
  st_make_valid() |> 
  st_simplify(preserveTopology = T, dTolerance = 500) |> 
  st_make_valid()

st_write(obj = eez,
         dsn = here("data", "processed_data", "mex_eez.gpkg"),
         delete_dsn = T)
