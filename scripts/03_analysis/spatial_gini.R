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

# Load data --------------------------------------------------------------------
tracks <-
  readRDS(file = here("data", "processed", "clean_tracks.rds"))

dist_df <-
  readRDS(file = here("data", "processed", "distance_to_MPA.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
spatial_gini <- tracks %>%
  filter(location == "at_sea",
         fishing == 1,
         # vessel_rnpa %in% (vessel_info %>% filter(displaced == 1) %>% pull(vessel_rnpa)),
         between(year, 2014, 2021),
         lon < -80) %>%
  mutate(lon = (as.integer(floor(lon / 0.5)) * 0.5) + 0.25,
         lat = (as.integer(floor(lat / 0.5)) * 0.5) + 0.25) %>%
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
  left_join(dist_df, by = c("lon", "lat")) %>%
  mutate(bin = (floor(dist / 500) * 500))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
pal <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")
div_pal <- colorRamps::blue2red(20)

mex <-
  rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")

ggplot(data = spatial_gini) +
  geom_tile(aes(x = lon, y = lat, fill = gini)) +
  geom_rect(
    xmin = lon_range[1],
    xmax = lon_range[2],
    ymin = lat_range[1],
    ymax = lat_range[2],
    color = "black",
    fill = "transparent"
  ) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  facet_wrap( ~ aft) +
  scale_fill_gradientn(colours = pal, limits = c(NA, 1)) +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "Gini\nindex",
    frame.colour = "black",
    ticks.colour = "black"
  ))

spatial_gini %>%
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
  geom_rect(
    xmin = lon_range[1],
    xmax = lon_range[2],
    ymin = lat_range[1],
    ymax = lat_range[2],
    color = "black",
    fill = "transparent"
  ) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "#E41A1C",
    # limits = c(-0.3, 0.3)
  ) +
  theme_bw() +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "Change in\nGini index",
    frame.colour = "black",
    ticks.colour = "black"
  ))

# Change in hours
spatial_gini %>%
  drop_na(h) %>% 
  group_by(lon, lat, after) %>%
  summarize(h = mean(h, na.rm = T)) %>% 
  ungroup() %>% 
  spread(after, h, fill = 0) %>% 
  mutate(dif = (`1` - `0`) / (`1` + `0`)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = dif)) +
  geom_rect(
    xmin = lon_range[1],
    xmax = lon_range[2],
    ymin = lat_range[1],
    ymax = lat_range[2],
    color = "black",
    fill = "transparent"
  ) +
  geom_sf(data = mex, color = "black", size = 0.1) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "#E41A1C",
    # limits = c(-0.3, 0.3)
  ) +
  theme_bw() +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "Change in\nGini index",
    frame.colour = "black",
    ticks.colour = "black"
  ))

ggplot(data = spatial_gini,
       mapping = aes(x = year, y = gini, fill = aft, group = year)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "None") +
  labs(x = "Year",
       y = "Gini index")

## ANALYZE #####################################################################
reg_data <- spatial_gini %>%
  drop_na()

reg_data %>%
  group_by(after, bin) %>%
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
  scale_fill_viridis_c() +
  theme_void() +
  guides(fill = guide_colorsteps(
    title = "Distance bin",
    frame.colour = "black",
    ticks.colour = "black"
  )) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

model <- fixest::feols(
  gini ~ i(bin) + i(bin, after) -1 | year,
  data = reg_data#,
  # cluster = ~ lat + lon,
  # panel.id = c("bin", "year")
)

summary(model)

model %>%
  broom::tidy() %>%
  filter(str_detect(term, "[:digit:]:after")) %>%
  mutate(
    term = as.numeric(str_remove_all(term, "[:alpha:]|[:punct:]")),
    term2 = paste0("(", term, "-", term + 500, "]"),
    term2 = fct_reorder(term2, term)
  ) %>%
  ggplot(aes(
    x = term2,
    y = estimate,
    ymin = estimate - std.error,
    ymax = estimate + std.error
  )) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x = "Distance from MPA boundary (Km)",
       y = "Change in Gini index")


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
