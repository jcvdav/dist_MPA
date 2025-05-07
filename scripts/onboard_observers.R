################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# I have multiple data sets that may be useful for trainign models.

# Data for classifying fishing / no fishing: the data in
# fleet.txt contain 1373 records for the location of tuna purse seine sets ocurring in 2013 and 2014.
# The data contain date, lat, lon, and trip. The same data is available in flota.txt, which als has
# information on catch
#
# The data called BD_ATUN_2013_... contains latitue, longitude, date, type of set, and catch
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(sf)
library(readxl)
library(tidyverse)

# Load data --------------------------------------------------------------------
sets <- read_excel(here("data/raw/tuna_trust/null_sets/info.xlsx"),
                   col_names = c("year", "month", "day", "lat", "lon", "event"))

fleet <- read_delim(here("data/raw/tuna_trust/tallas/flota.txt"),
                    col_names = c("fecha", "crucero", "lance", "dia", "mes", "ano", "lat", "lon")) |> 
  mutate(set_id = paste(crucero, lance, sep = "-")) |> 
  select(trip = crucero, set_id, date = fecha, lat, lon) |> 
  distinct()

latlon <- read_delim(here("data/raw/tuna_trust/tallas/LatLonDes.txt"),
                     col_names = c("lat", "lon", "Capt")) |> 
  select(lat, lon, catch = Capt) |> 
  distinct()

bd <- read_excel(here("data/raw/tuna_trust/BD_ATÚN_2013_JCVD150220.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(longitud = longitud_10,
         talla = longitud_13) %>% 
  select(crucero, dia, mes, ano, lance, tipo, lat = latitud, lon = longitud, captura_total) %>% 
  distinct() |> 
  filter(between(lon, 80, 180)) |> 
  mutate(lon = -1 * lon)


coast <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
revilla <- st_read(here("data/processed/revilla_new.gpkg"))

raw_tracks <- readRDS(here("data", "raw", "raw_tracks.rds"))



## PROCESSING ##################################################################
combined <- inner_join(fleet, latlon, by = join_by(lat, lon), relationship = "many-to-many") |> # We have some sets that ocurred at the exact same location in different dates, but we don't know which catch matches which date
  # This pipeline identifies coordinates with more than one record and removes them
  group_by(lat, lon) |> 
  add_count() |> 
  arrange(desc(n)) |> 
  ungroup() |> 
  filter(n == 1) |> 
  select(-n) |> 
  mutate(date = dmy(date))



# We only have observer data for 2013 and 2014, so lets keep only those tracks
tracks <- raw_tracks %>% 
  filter(year(datetime) %in% c(2013, 2014)) |> 
  mutate(date = date(datetime)) |> 
  select(seg_id, point_in_seg, vessel_rnpa, eu_rnpa, date, lat, lon, implied_speed_knots, course, distance_to_last_m, hours)

## VISUALIZE ###################################################################
# Quickly look at the sete-level data
ggplot() +
  geom_point(data = combined, aes(x = lon, y = lat), color = "blue") +
  geom_point(data = bd, aes(x = lon, y = lat), color = "orange") +
  geom_sf(data = coast) 

# All pings, all sets
ggplot() +
  geom_point(data = tracks,
             mapping = aes(x = lon, y = lat),
             alpha = 0.5,
             pch = ".") +
  geom_point(data = combined, 
             mapping = aes(x = lon, y = lat, size = catch),
             color = "red",
             shape = 21,
             fill = "transparent") +
  geom_sf(data = coast) +
  geom_sf(data = revilla, fill = "transparent", color = "blue", linewidth = 1) +
  theme_void() +
  theme(legend.position = "None")

ggplot() +
  geom_sf(data = coast) +
  geom_point(data = tracks,
             mapping = aes(x = lon, y = lat),
             # alpha = 0.5,
             pch = ".") +
  geom_point(data = combined, 
             mapping = aes(x = lon, y = lat, size = catch),
             color = "red",
             shape = 21,
             fill = "transparent") +
  geom_sf(data = revilla, fill = "transparent", color = "blue", linewidth = 1) +
  theme_void() +
  theme(legend.position = "None") +
  lims(x = c(-115.45, -110.1), y = c(17.65, 20))

sets_sf <- combined |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  rename(set_date = date)

tracks_sf <- tracks |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  rename(vms_date = date)

# Build buffers around each set
buffered_sets <- st_buffer(sets_sf, dist = 1e4)


# These are ll vms tracks that are within 10 km of a set.
vms_within_sets <- st_join(tracks_sf, buffered_sets, left = F) |> 
  filter(vms_date == set_date)

################################################################################
# Are there any sets associated with more than one vessel?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip, set_id) |> 
  summarize(n = n_distinct(vessel_rnpa)) |> 
  arrange(desc(n))

# Which ones, and how many pings each?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip, set_id) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n > 1) |> 
  count(trip, set_id, vessel_rnpa) |> 
  arrange(set_id, n) 

################################################################################
# Are there any trips associated with more than one vessel?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  summarize(n = n_distinct(vessel_rnpa)) |> 
  arrange(desc(n))

# Which ones, and how many pings each?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n > 1) |> 
  count(trip, vessel_rnpa)

################################################################################
# From the subset of trip-vessel and set-vessel identified above, are there any that don't make sense?
# For example, do I see set A1 and vessel 1, but then see that vessel 1 was part of trip B?
# This identifies the vessel with the most pings as part of a trip and removes the other one
trip_vessel_pairs <- vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n > 1) |> 
  count(trip, vessel_rnpa) |> 
  arrange(trip, vessel_rnpa, n) |> 
  group_by(trip) |> 
  slice_max(n) |> 
  select(-n)

# This identifies all trips consistently associated with just one vessel
trips_with_one_vessel <- vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n == 1) |> 
  select(trip, vessel_rnpa) |> 
  distinct()

# And this combines trips with just one vessel and trips that we deem should be associated with just one vessel
valid_trip_vessel_pairs <- bind_rows(trips_with_one_vessel,
                                     trip_vessel_pairs)

# This is my table of whether a given ping is considered fishing
vms_likely_fishing <- vms_within_sets |> 
  inner_join(valid_trip_vessel_pairs, by = join_by(trip, vessel_rnpa)) |> 
  select(seg_id, point_in_seg, trip, set_id) |> 
  mutate(likely_fishing = T) |> 
  st_drop_geometry()

# This table tells me which VMS positions are within the range of known onboard observer, and thus no fishing is a true "no"
onboard_coverage_by_vessel <- vms_within_sets |> 
  inner_join(valid_trip_vessel_pairs, by = join_by(vessel_rnpa, trip)) |> 
  group_by(vessel_rnpa, trip) |> 
  mutate(first_set = min(set_date, na.rm = T),
            last_set = max(set_date, na.rm = T),
            .groups = "drop") |> 
  filter(between(vms_date, first_set, last_set)) |> 
  select(vessel_rnpa, trip, first_set, last_set) |> 
  st_drop_geometry() |> 
  distinct()

vms_activity_with_onboard_overage <- tracks |> 
  left_join(onboard_coverage_by_vessel,
             by = join_by(vessel_rnpa, between(date, first_set, last_set))) |> 
  drop_na(trip) |> 
  left_join(vms_likely_fishing, by = join_by(seg_id, point_in_seg, trip)) |> ## <- This left join is adding 12 duplicate observations. This is happening because some points are part of more than one set. See {ASDF} for diagnosis
  replace_na(replace = list(likely_fishing = 0))


#{ASDF}
vms_known_fishing |> count(seg_id, point_in_seg, trip) |> arrange(desc(n)) |> filter(n == 2)
# Notes on this:
# I tried changing the buffer (making it smaller), but it didn't work


# We now use valid trip-vessel pairs remove cases where a vessel's ping was incorrectly associated with a set not part of its trip
# Perform intersection again, but now retain non-matched positions too (i.e. not fishing, but part of a trip)
# Identify a list of vessels that had on-board observers during 2013 and 2014
# Further refine to keep all positions between the first associated set and the last associated set. This is to make sure that we have true positives and true negatives.

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------