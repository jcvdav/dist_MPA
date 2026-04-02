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

# Load data --------------------------------------------------------------------
vessel_info <- readRDS(file = here("data", "raw", "vessel_info.rds"))

landings <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_annual_landings_by_vessel.rds"
  )
)
## PROCESSING ##################################################################

# Tuna landings ----------------------------------------------------------------
tuna_landings <- landings %>% 
  filter(between(year, 2003, 2021),
         main_species_group == "ATUN") %>% 
  select(year, vessel_rnpa, landed_weight) %>% 
  inner_join(vessel_info, by = "vessel_rnpa") %>% 
  group_by(year, vessel_rnpa) %>% 
  summarize(landed_weight = sum(landed_weight)) %>% 
  mutate(ba = ifelse(year <= 2017, "Before", "After"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = tuna_landings,
        file = here("data", "processed", "tuna_landings.rds"))
