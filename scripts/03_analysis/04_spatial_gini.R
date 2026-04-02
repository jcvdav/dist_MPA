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
library(tidyverse)
library(sf)
library(fixest)
library(ggfixest)
library(wesanderson)
library(colorRamps)
library(rnaturalearth)
source(here(".Rprofile"))

# Load data --------------------------------------------------------------------
tracks <-
  readRDS(file = here("data", "processed", "scored_tracks.rds"))

dist_df <-
  readRDS(file = here("data", "processed", "distance_to_MPA.rds"))

revilla <- st_read(here("data/processed/revilla_new.gpkg"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
bin <- 0.5

st_remove <- function(a, b) {
  st_filter(a, st_union(b), .predicate = st_disjoint)
}

spatial_gini <- tracks %>%
  filter(fishing,
         # vessel_rnpa %in% (vessel_info %>% filter(displaced == 1) %>% pull(vessel_rnpa)),
         between(year, 2014, 2021),
         lon < -80) %>%
  mutate(lon = (as.integer(floor(lon / bin)) * bin) + bin / 2,
         lat = (as.integer(floor(lat / bin)) * bin) + bin / 2) %>%
  mutate(aft = ifelse(year <= 2017, "Before", "After"),
         aft = fct_reorder(aft, year),
         after = ifelse(year == 2017, 0, after)) %>% 
  group_by(year, after, aft, vessel_rnpa, lon, lat) %>%
  summarize(h = sum(hours, na.rm = T)) %>%
  filter(h > 0) %>%
  ungroup() %>%
  complete(vessel_rnpa, nesting(year, after, aft, lon, lat), fill = list(h = 0)) %>%
  group_by(year, after, aft, lon, lat) %>%
  summarize(gini = gini(h),
            h = sum(h, na.rm = T),
            n = n_distinct(vessel_rnpa)) %>%
  ungroup() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
  mutate(dist = st_distance(., revilla),
         dist = units::set_units(dist, "nautical_miles"),
         dist = as.numeric(dist)) |> 
  st_remove(b = revilla) |> 
  filter(dist <= 200) |> 
  mutate(near = 1 * (dist <= 100),
         bin = (floor(dist / 100) * 100)) %>% 
  bind_cols(st_coordinates(.)) |> 
  st_drop_geometry() |> 
  rename(lon = X,
         lat = Y)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
pal <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")
div_pal <- colorRamps::blue2red(20)

mex <-
  rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")

spatial_gini_ba <- ggplot(data = spatial_gini) +
  geom_tile(aes(x = lon, y = lat, fill = gini)) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  facet_wrap( ~ aft) +
  scale_fill_gradientn(colours = pal, limits = c(NA, 1)) +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "Gini\nindex",
    frame.colour = "black",
    ticks.colour = "black"
  ))

spatial_gini_ba

delta_gini <- spatial_gini %>%
  drop_na(gini) %>%
  group_by(lon, lat, after) %>%
  summarize(gini = mean(gini, na.rm = T)) %>%
  ungroup() %>%
  select(after, lon, lat, gini) %>%
  spread(after, gini) %>%
  mutate(dif = (`1` - `0`)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = dif)) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "#E41A1C"
  ) +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "Change in\nGini index",
    frame.colour = "black",
    ticks.colour = "black"
  ))

delta_gini

# Change in hours
delta_hours <- spatial_gini %>%
  drop_na(h) %>%
  group_by(lon, lat, after) %>%
  summarize(h = mean(h, na.rm = T)) %>%
  ungroup() %>%
  spread(after, h, fill = 0) %>%
  mutate(dif = (`1` - `0`) / (`1` + `0`)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = dif)) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "#E41A1C"
  ) +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "Change in\nfishing hours",
    frame.colour = "black",
    ticks.colour = "black"
  ))

delta_hours

gini_violin <- ggplot(data = spatial_gini,
       mapping = aes(x = year, y = gini, fill = aft, group = year)) +
  geom_violin() +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "None") +
  labs(x = "Year",
       y = "Gini index")

gini_violin

## ANALYZE #####################################################################
reg_data <- spatial_gini %>%
  drop_na() |> 
  mutate(bin = as_factor(bin),
         bin = fct_reorder(.f = bin, .x = dist, .fun = mean, .desc = T)) |> 
  group_by(lon, lat) |> 
  mutate(n = n_distinct(after)) |> 
  ungroup() |> 
  filter(n == 2)
  

reg_data %>%
  group_by(after, near) %>%
  summarize(gini = mean(gini, na.rm = T)) %>% 
  spread(after, gini) %>%
  mutate(dif = `1` - `0`)

inset <- ggplot(data = reg_data) +
  geom_tile(aes(x = lon, y = lat, fill = bin)) +
  geom_rect(
    xmin = lon_range[1],
    xmax = lon_range[2],
    ymin = lat_range[1],
    ymax = lat_range[2],
    color = "black",
    fill = "transparent"
  ) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

model <- fixest::feols(
  gini ~ i(year, near, "2017") | near + year,
  data = reg_data,
  cluster = ~ lat + lon,
  panel.id = c("bin", "year")
)

summary(model)

gini_event_study <- ggfixest::ggiplot(model) +
  labs(x = "Year",
       y = "Effect on Gini (near vs. far)",
       title = "Spatial Gini event study")

gini_event_study

## EXPORT ######################################################################

ggsave(here("results", "figures", "spatial_gini_ba.png"), spatial_gini_ba,
       width = 10, height = 5, dpi = 300)
ggsave(here("results", "figures", "delta_gini.png"), delta_gini,
       width = 6, height = 5, dpi = 300)
ggsave(here("results", "figures", "delta_hours.png"), delta_hours,
       width = 6, height = 5, dpi = 300)
ggsave(here("results", "figures", "gini_violin.png"), gini_violin,
       width = 7, height = 4, dpi = 300)
ggsave(here("results", "figures", "gini_event_study.png"), gini_event_study,
       width = 6, height = 4, dpi = 300)
