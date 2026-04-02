######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(sf)
library(tidyverse)

# WDPAID is 902308
revilla_old <- st_read(dsn = here("data", "raw", "revilla_old", "WDPA_WDOECM_Feb2025_Public_902308_shp_0"),
               layer = "WDPA_WDOECM_Feb2025_Public_902308_shp-polygons")
  
# New polygon
# Coordinates com from the DOF decree at:
# http://www.dof.gob.mx/nota_detalle.php?codigo=5505736&fecha=27/11/2017
# WDPAID is 555629385
revilla_new <- st_read(dsn = here("data", "raw", "revilla_new", "WDPA_WDOECM_Feb2025_Public_555629385_shp_0"),
                       layer = "WDPA_WDOECM_Feb2025_Public_555629385_shp-polygons")


# Export
st_write(revilla_old, dsn = here("data", "processed", "revilla_old.gpkg"), delete_dsn = T)
st_write(revilla_new, dsn = here("data", "processed", "revilla_new.gpkg"), delete_dsn = T)




