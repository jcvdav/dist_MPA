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
library(readxl)
library(tidyverse)

# Load data --------------------------------------------------------------------
sets <- read_excel("/Users/juancarlosvillasenorderbez/Documents/Proyectos/Tunidos/info.xlsx",
                   col_names = c("year", "month", "day", "lat", "lon", "event"))

fleet <- read_delim("/Users/juancarlosvillasenorderbez/Documents/Proyectos/Atún/Tallas/Programas/flota.txt",
                    col_names = c("fecha", "crucero", "lance", "dia", "mes", "ano", "lat", "lon"))

latlon <- read_delim("/Users/juancarlosvillasenorderbez/Documents/Proyectos/Atún/Tallas/Programas/LatLonDes.txt",
                     col_names = c("lat", "lon", "Capt"))

coast <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")

bd <- read_excel("data/raw_data/BD_ATÚN_2013_JCVD150220.xlsx") %>% 
  janitor::clean_names() %>% 
  rename(longitud = longitud_10,
         talla = longitud_13) %>% 
  select(crucero, dia, mes, ano, lance, tipo, latitud, longitud, captura_total) %>% 
  distinct() %>% 
  filter(between(longitud, 80, 180))

## PROCESSING ##################################################################

tracks_2013 <- scored %>% 
  filter(year == 2013,
         speed_fishing) %>% 
  filter(between(lat, 0, 90))

ggplot() +
  geom_sf(data = coast) +
  geom_point(data = tracks_2013,
             mapping = aes(x = lon, y = lat), size = 0.1) +
  geom_point(data = bd, 
             mapping = aes(x = -longitud, y = latitud, size = captura_total),
             color = "red",
             shape = 21,
             fill = "transparent") +
  geom_sf(data = new_revilla, color = "blue", fill = "transparent") +
  theme_bw()

# X ----------------------------------------------------------------------------
filter(sets, between(event, 9, 12)) %>% 
  


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------