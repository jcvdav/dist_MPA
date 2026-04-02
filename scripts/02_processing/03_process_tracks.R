################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Processes scored tracks by assigning each vessel a treatment group
# (displaced vs. not displaced) based on whether they fished inside the
# Revillagigedo MPA before the 2017 expansion. Displaced vessels are those
# with >1% of their pre-expansion fishing effort inside the new MPA boundary.
# Exports processed_tracks.rds for downstream analysis scripts.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  rnaturalearth,
  mregions2,
  sf
)

# Load data --------------------------------------------------------------------
tracks <- readRDS(file = here("data", "processed", "scored_tracks.rds")) |> 
  filter(kmeans_fishing, speed_fishing)
mex <- ne_countries(country = "Mexico")
revilla <- st_read(here("data/processed/revilla_new.gpkg"))

revilla_buffer <- st_buffer(revilla, dist = units::as_units(100, "nautical_miles"))
  
## PROCESSING ##################################################################

# Assign treatment groups ------------------------------------------------------
all_before <- tracks |> 
  filter(before, between(year, 2013, 2017)) |>
  pull(vessel_rnpa) |> 
  unique()

length(all_before)

displaced <- tracks |>
  filter(before, between(year, 2013, 2017)) |>
  group_by(vessel_rnpa, inside) |>
  summarize(h = sum(hours)) |>
  group_by(vessel_rnpa) |>
  mutate(hr = h / sum(h)) |>
  filter(inside,
         hr > 0.01) |> 
  pull(vessel_rnpa) |> 
  unique()

length(displaced)

not_displaced <- tracks |> 
  filter(before, between(year, 2013, 2017), !inside) |>
  filter(!(vessel_rnpa %in% displaced)) |> 
  pull(vessel_rnpa) |> 
  unique()

length(not_displaced)

processed_tracks <- tracks |> 
  filter(between(year, 2013, 2023)) |>
  mutate(displaced = vessel_rnpa %in% displaced) 

## EXPORT ######################################################################

saveRDS(processed_tracks, file = here("data", "processed", "processed_tracks.rds"))
