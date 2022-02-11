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

ports <- tibble(home_port = c("ENSENADA",
                              "SAN BLAS",
                              "MAZATLAN",
                              "EL SAUZAL",
                              "PUERTO MADERO",
                              "CHIAPAS",
                              "MANZANILLO",
                              "SAN CARLOS"),
                lon = c(-116.623542, -105.3753656, -106.4572965, -116.7088028, -92.4405662, -92.4405662, -104.3704452, -111.8274082),
                lat = c(31.843450, 21.7336208, 23.2468188, 31.8907039, 14.7230243, 14.7230243, 19.0776825, 25.3470472)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_write(ports, dsn = here("data", "processed_data", "ports.gpkg"), delete_dsn = T)
