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
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  readxl,
  tidyverse
)

# Load data --------------------------------------------------------------------
# These data come from the tuna total length papers
sets <- readRDS(file = here("data", "processed", "onboard_observer_data.rds"))
# Vessel tracks from VMS
raw_tracks <- readRDS(here("data", "raw", "raw_tracks.rds"))

## PROCESSING ##################################################################
# Filter data down -------------------------------------------------------------
# We only have observer data for 2013 and 2014, so lets keep only those tracks
tracks <- raw_tracks %>% 
  filter(year(datetime) %in% c(2013, 2014)) |> 
  mutate(date = date(datetime)) |> 
  select(seg_id, point_in_seg, vessel_rnpa, eu_rnpa, date, datetime, lat, lon, implied_speed_knots, course, distance_to_last_m, hours)


# Build spatial objects --------------------------------------------------------
# For sets
sets_sf <- sets |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  rename(set_date = date)

# For tracks
tracks_sf <- tracks |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  rename(vms_date = date)

# Spatial operations -----------------------------------------------------------
# Build a 10 km buffer around each set (they record things to the neares 0.1 degree)
buffered_sets <- st_buffer(sets_sf, dist = 1e4)

# Identify vms messages tracks that are within 10 km of a set.
vms_within_sets <- st_join(tracks_sf, buffered_sets, left = F) |> 
  filter(vms_date == set_date)

# The spatial intersection might not always get it right. There may be
# vms points that fall within two or more set buffers, vms points assigned to 
# more than one trip, and sets with more than one vessel associated with them. 
# The next chunks of code identify these types of missmatches, and then fixes them.

# Tests ------------------------------------------------------------------------
# These are purely for diangosis
## Test 1
# Are there any sets associated with more than one vessel?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip, set_id) |> 
  summarize(n = n_distinct(vessel_rnpa),
            .groups = "drop") |> 
  arrange(desc(n)) |> 
  dim()

# There seem to be some records associated with more than one vessel
# Which ones, and how many pings each?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip, set_id) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n > 1) |> 
  count(trip, set_id, vessel_rnpa) |>  #calculate n pings per set and vessel
  arrange(set_id, n)
# We identifed 534 set ids with more than one

## Test 2
# Are there any trips associated with more than one vessel?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  summarize(n = n_distinct(vessel_rnpa),
            groups = "drop") |> 
  arrange(desc(n)) |> 
  dim()

# Which ones, and how many pings each?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n > 1) |> 
  count(trip, vessel_rnpa) #calculate n pings per trip and vessel
# There are 311 trips that are associated with more than one vessel


## Test 3: Are there any vms positions associated with more than one set?
vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(seg_id, point_in_seg) |> 
  summarize(n = n_distinct(set_id),
            .groups = "drop") |> 
  filter(n > 1) |> 
  dim()
# There are 435 VMS pings associated with more than one set

# Begin fixing gremlins identified above ---------------------------------------
# We'll begin by finding the valid vessel-trip combinations, which should address
# most of the issues


## Fix # 1: trips with more than one vessel ====================================
# The idea is that the spatial intersection might have accidentally
# missassigned a trip to a vessel. The way I propose to fix it is to identify
# the vessel that was most frequently associated with a given trip (because all
# sets should match this vessel). This allows us to remove any other vessels
# that might have momentarily matched.

#This identifies the vessel with the most pings as part of a trip and removes any
# other vessels
trip_vessel_pairs <- vms_within_sets |> 
  # part 1 identifies trips with more than one vessel
  st_drop_geometry() |> 
  group_by(trip) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n > 1) |> 
  # part 2 identifies the vessel most frequently associated with a trip
  count(trip, vessel_rnpa) |> 
  arrange(trip, vessel_rnpa, n) |> 
  group_by(trip) |> 
  slice_max(n) |> 
  select(-n)

# This identifies all trips consistently associated with just one vessel, this are always clean
trips_with_one_vessel <- vms_within_sets |> 
  st_drop_geometry() |> 
  group_by(trip) |> 
  mutate(n = n_distinct(vessel_rnpa)) |> 
  ungroup() |> 
  filter(n == 1) |> 
  select(trip, vessel_rnpa) |> 
  distinct()

# And this combines trips with just one vessel and trips that we deem should be 
# associated with just one vessel
valid_trip_vessel_pairs <- bind_rows(trips_with_one_vessel,
                                     trip_vessel_pairs)


## Fix #2: VMS positions associated with more than one set =====================
# Even after identifying valid vessel-trip pairs, we now have cases where two 
# sets by the same vessel occurred closed to each other, so some vms pings
# are being attributed to more than one. There are just a few cases, and the
# fix is quite easy: For each GPS ping, I will asign it to whichever feature
# is closest.

# Let's first check how many mismatched we still have
vms_within_sets |> 
  st_drop_geometry() |> 
  inner_join(valid_trip_vessel_pairs, by = join_by(trip, vessel_rnpa)) |> 
  count(vessel_rnpa, seg_id, point_in_seg, trip) |>
  arrange(desc(n)) |>
  filter(n == 2) |> 
  dim()

# It looks like its now just two vessels (12 pings) that end up being 24 pings.
# Let's take the points that are associated with more than one set and assign
# them to the set closest to them.

# Step 1 build a table with all offending pings, and their corresponding sets.
# This is a table of things that need to be fixed.
conflicting_pings <- vms_within_sets |> 
  st_drop_geometry() |> 
  inner_join(valid_trip_vessel_pairs, by = join_by(trip, vessel_rnpa)) |> 
  group_by(vessel_rnpa, seg_id, point_in_seg, trip) |> 
  mutate(n = n_distinct(set_id)) |> 
  filter(n == 2) |> 
  ungroup() |> 
  select(vessel_rnpa, seg_id, point_in_seg, trip, set_id)

# Step 2: Get sf versions of conflicting sets and pings
# Identify all conflicting sets
conflicting_sets_sf <- sets_sf |> 
  filter(trip %in% unique(conflicting_pings$trip),
         set_id %in% unique(conflicting_pings$set_id))

# And all conflicting pings
conflicting_pings_sf <- tracks_sf |> 
  filter(vessel_rnpa %in% unique(conflicting_pings$vessel_rnpa),
         seg_id %in% unique(conflicting_pings$seg_id),
         point_in_seg %in% unique(conflicting_pings$point_in_seg))

# Step 3: For each ping, find the set closest to it
points_down_to_one_set <- st_join(conflicting_pings_sf, conflicting_sets_sf, join = st_nearest_feature) |> 
  st_drop_geometry() |> 
  select(seg_id, point_in_seg, trip, set_id) |> 
  distinct()

# Bring it all together -------------------------------------------------------
# We have now identified which pings are associated with sets. But we must also
# bring in all the non-fishing but important VMS pings (transiting, in port...)
# We want to make sure that, in doing so, we only retain VMS messages from vessels
# that have an observer on board. Otherwise we cant know for sure whether a position is
# "fishing" or "not fishing"


# Step 1: Identify a list of vessels that had on-board observers during 2013 and 2014
# This table tells me the dates of the first and last set of each trip that had onboard
# observers for each vessel
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

## Step 2: List of positions likely fishing ====================================
# This is my table of whether a given ping is considered fishing
vms_likely_fishing <- vms_within_sets |> 
  inner_join(valid_trip_vessel_pairs, by = join_by(trip, vessel_rnpa)) |> 
  select(seg_id, point_in_seg, trip, set_id) |> 
  mutate(likely_fishing = T) |> 
  st_drop_geometry()

## Step 3: Now take the raw tracks, and start adding information
vms_activity_with_onboard_coverage <- tracks |> 
  left_join(onboard_coverage_by_vessel,
            by = join_by(vessel_rnpa, between(date, first_set, last_set))) |> 
  drop_na(trip) |> 
  left_join(vms_likely_fishing, by = join_by(seg_id, point_in_seg, trip)) |>  ## <- This left join is adding 12 duplicate observations. This is happening because some points are part of more than one set. This has already been identified and is fixed in step 3a and 3b below
  replace_na(replace = list(likely_fishing = 0))

## 3a Account for the pings associated with more than one set
## First, remove pings associated with more than one set
single_obs <- vms_activity_with_onboard_coverage |> 
  anti_join(points_down_to_one_set, by = join_by(seg_id, point_in_seg, trip))

# 3b Then, find the pairs of pings and sets that have already been validates
# From the pings associated with more than one set, keep only the valid combinations
fixed_obs <- vms_activity_with_onboard_coverage |> 
  inner_join(points_down_to_one_set, by = join_by(seg_id, point_in_seg, trip, set_id))

# Combine them
final <- bind_rows(single_obs, fixed_obs)

nrow(final) == nrow()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("data", "processed", "labeled_tracks.rds"))


