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
revilla <- st_read(here("data/processed_data/revilla_new.gpkg"))

scored <- readRDS(file = here("data", "processed_data", "scored_tracks.rds"))

bd <- read_excel("data/raw/BD_ATÚN_2013_JCVD150220.xlsx") %>% 
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

m <- 8

ggplot() +
  geom_sf(data = coast) +
  geom_point(data = bd |> filter(mes == m), 
             mapping = aes(x = -longitud, y = latitud, size = captura_total),
             color = "black",
             shape = 21,
             fill = "black") +
  geom_point(data = tracks_2013 |> filter(month == m),
             mapping = aes(x = lon, y = lat, color = kmeans_fishing)) +
  geom_sf(data = revilla, color = "blue", fill = "transparent") +
  theme_bw() +
  lims(x = c(-115.5, -110), y = c(17.5, 20))

scored |> 
  mutate(day = day(datetime)) |> 
  left_join(bd |> mutate(ano = 2000 + ano), by = join_by("day" == "dia", "month" == "mes", "year" == "ano")) |>
  drop_na(captura_total) |> 
  group_by(year, month, day) |> 
  mutate(n = n_distinct(crucero)) |> 
  ungroup() |> 
  filter(n == max(n)) |> 
  ggplot() +
  geom_point(aes(x = lon, y = lat, color = kmeans_fishing)) +
  geom_point(aes(x = -longitud, y = latitud), color = "red", size = 4, shape = 21) +
  geom_sf(data = revilla, color = "blue", fill = "transparent")

# X ----------------------------------------------------------------------------

  


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------