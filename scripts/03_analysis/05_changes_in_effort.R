################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Changes in fishing effort around the 2017 Revillagigedo MPA expansion.
# Analyzes effort within the spillover ring, builds concave hulls for
# displaced vs. non-displaced fishing grounds, and runs event study models.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  rnaturalearth,
  sf,
  fixest,
  ggfixest,
  cowplot,
  modelsummary
)

# Load data --------------------------------------------------------------------
processed_tracks <- readRDS(file = here("data", "processed", "processed_tracks.rds"))
mex <- ne_countries(country = "Mexico")
revilla <- st_read(here("data/processed/revilla_new.gpkg"))
revilla_buffer <- st_buffer(revilla, dist = units::as_units(100, "nautical_miles"))

## PROCESSING ##################################################################

# Filter to spillover ring -----------------------------------------------------
effort_in_ring <- processed_tracks %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_filter(revilla_buffer) |>
  st_filter(st_union(revilla), .predicate = st_disjoint) %>%
  bind_cols(st_coordinates(.)) |>
  st_drop_geometry() |>
  rename(lon = X, lat = Y)

# Effort panel -----------------------------------------------------------------
panel <- effort_in_ring |>
  group_by(year, displaced, before, vessel_rnpa) %>%
  summarize(h = sum(hours, na.rm = T),
            .groups = "drop") %>%
  ungroup() |>
  mutate(after = !before,
         event = year - 2018)

## VISUALIZE ###################################################################

# Overall effort through time --------------------------------------------------
ggplot(panel, aes(x = year, y = h)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1, fill = "cadetblue") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Average fishing effort (hours)")

# By displacement status -------------------------------------------------------
p1 <- ggplot(panel, aes(x = year, y = h, fill = displaced)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1) +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Average fishing effort (hours)",
       fill = "Displaced fleet") +
  scale_fill_brewer(palette = "Set2")

# Normalized effort ------------------------------------------------------------
panel %>%
  group_by(year, vessel_rnpa, displaced) %>%
  summarize(h = sum(h, na.rm = T),
            .groups = "drop") %>%
  ungroup() |>
  group_by(vessel_rnpa) %>%
  mutate(hr = (h - mean(h)) / sd(h)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = hr, fill = displaced)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1) +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1,1),
        legend.background = element_blank()) +
  labs(x = "Year",
       y = "Normalized fishing effort ([h - mu] / sigma)",
       fill = "Status") +
  scale_fill_brewer(palette = "Set2")

## MODELS ######################################################################

m1 <- feols(log(h) ~ after * displaced,
            data = panel,
            cluster ~ vessel_rnpa)

m2 <- feols(log(h) ~ i(event, displaced, -1) | vessel_rnpa + year,
            data = panel)

p2 <- ggiplot(m2) +
  labs(x = "Years to closure",
       title = "Change in effort within spillover ring")

modelsummary(list("DiD" = m1, "Event study" = m2),
             stars = T,
             output = here("results", "tables", "effort_models.csv"))

# Spatial effort map -----------------------------------------------------------
spatial_effort <- ggplot(data = effort_in_ring |>
         mutate(when = ifelse(before, "Before", "After"),
                when = fct_relevel(when, "Before", "After"),
                lon = floor(lon / 0.1) * 0.1 + 0.05,
                lat = floor(lat / 0.1) * 0.1 + 0.05) |>
         group_by(when, lat, lon) |>
         summarize(h = sum(hours)),
       mapping = aes(x = lon, y = lat, fill = h)) +
  geom_tile() +
  geom_sf(data = revilla, inherit.aes = F) +
  scale_fill_viridis_c(trans = "log") +
  facet_wrap(~when) +
  theme_bw()

spatial_effort

effort_combined <- plot_grid(p1, p2, ncol = 1)
effort_combined

## EXPORT ######################################################################

ggsave(here("results", "figures", "effort_by_displacement.png"), p1,
       width = 7, height = 4, dpi = 300)
ggsave(here("results", "figures", "effort_event_study.png"), p2,
       width = 6, height = 4, dpi = 300)
ggsave(here("results", "figures", "effort_combined.png"), effort_combined,
       width = 7, height = 8, dpi = 300)
ggsave(here("results", "figures", "spatial_effort_ring.png"), spatial_effort,
       width = 10, height = 5, dpi = 300)

## CONCAVE HULLS ###############################################################

# Before -----------------------------------------------------------------------
A <- processed_tracks |>
  filter(before, displaced) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(a = 1) |>
  group_by(a) |>
  summarize(.groups = "drop") |>
  st_concave_hull(0.05) |>
  st_difference(mex) |>
  select(a)

B <- processed_tracks |>
  filter(before, !displaced) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(b = 1) |>
  group_by(b) |>
  summarize(.groups = "drop") |>
  st_concave_hull(0.05) |>
  st_difference(mex) |>
  select(b)

AB <- st_union(A, B) |>
  st_make_valid()

# After ------------------------------------------------------------------------
A_after <- processed_tracks |>
  filter(!before, displaced) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(a = 1) |>
  group_by(a) |>
  summarize(.groups = "drop") |>
  st_concave_hull(0.05) |>
  st_difference(mex) |>
  st_difference(revilla) |>
  select(a)

B_after <- processed_tracks |>
  filter(!before, !displaced) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(b = 1) |>
  group_by(b) |>
  summarize(.groups = "drop") |>
  st_concave_hull(0.05) |>
  st_difference(mex) |>
  st_difference(revilla) |>
  select(b)

AB_after <- st_union(A_after, B_after) |>
  st_make_valid()

# Visualize hulls --------------------------------------------------------------
ggplot() +
  geom_sf(data = A, fill = "transparent", color = "blue") +
  geom_sf(data = B, fill = "transparent", color = "red") +
  geom_sf(data = AB, fill = "transparent",  color = "purple")

ggplot() +
  geom_sf(data = A_after, fill = "transparent", color = "blue") +
  geom_sf(data = B_after, fill = "transparent", color = "red") +
  geom_sf(data = AB_after, fill = "transparent",  color = "purple")

plot(AB, reset = F, max.plot = 1)
plot(AB_after[,1], add = T, col = "transparent", alpha = 0.5)

# Area change ------------------------------------------------------------------
area_before <- st_area(AB) |> units::set_units(value = "km2")
area_after <- st_area(AB_after) |> units::set_units(value = "km2")

(area_after - area_before) / area_before

## SUMMARY STATS ###############################################################

# Used in the abstract
stats <- processed_tracks |>
  filter(year < 2024) |>
  mutate(post = !before) |>
  group_by(post, displaced) |>
  summarize(h = sum(hours) / n_distinct(year)) |>
  pivot_wider(names_from = displaced, values_from = h, names_prefix = "disp_") |>
  mutate(tot = disp_FALSE + disp_TRUE)

((stats$disp_FALSE[2]-stats$disp_FALSE[1]) / stats$disp_FALSE[1]) * 100
((stats$disp_TRUE[2]-stats$disp_TRUE[1]) / stats$disp_TRUE[1]) * 100
((stats$tot[2]-stats$tot[1]) / stats$tot[1]) * 100

# Vessels within the spillover area --------------------------------------------
bef_spill <- processed_tracks |>
  filter(before) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_filter(revilla_buffer) |>
  st_drop_geometry() |>
  pull(vessel_rnpa) |>
  unique()

after_spill <- processed_tracks |>
  filter(!before) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_difference(revilla) |>
  st_filter(revilla_buffer) |>
  st_drop_geometry() |>
  pull(vessel_rnpa) |>
  unique()

length(bef_spill)
length(after_spill)

sum(bef_spill %in% after_spill)
