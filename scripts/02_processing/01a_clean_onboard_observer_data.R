################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
# These data come from the tuna total length papers
# Fleet has info on the trip and set number
fleet <- read_delim(here("data/raw/tuna_trust/tallas/flota.txt"),
                    col_names = c("fecha", "crucero", "lance", "dia", "mes", "ano", "lat", "lon")) |> 
  mutate(set_id = paste(crucero, lance, sep = "-")) |> 
  select(trip = crucero, set_id, date = fecha, lat, lon) |> 
  distinct()

# Catch data associated with each set
latlon <- read_delim(here("data/raw/tuna_trust/tallas/LatLonDes.txt"),
                     col_names = c("lat", "lon", "Capt")) |> 
  select(lat, lon, catch = Capt) |> 
  distinct()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- inner_join(fleet, latlon, by = join_by(lat, lon), relationship = "many-to-many") |> # We have some sets that ocurred at the exact same location in different dates, but we don't know which catch matches which date
  # This pipeline identifies coordinates with more than one record and removes them
  group_by(lat, lon) |> 
  add_count() |> 
  arrange(desc(n)) |> 
  ungroup() |> 
  filter(n == 1) |> 
  select(-n) |> 
  mutate(date = dmy(date))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = combined, file = here("data", "processed", "onboard_observer_data.rds"))
