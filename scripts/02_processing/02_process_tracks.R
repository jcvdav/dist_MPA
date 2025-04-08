################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Processing tracks involves two things:
#     - Assigning each vessel a treatment group
#     - Identifying area A, area B, and their union
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
tracks <- readRDS(file = here("data", "processed_data", "scored_tracks.rds")) |> 
  filter(kmeans_fishing & speed_fishing)
mex <- ne_countries(country = "Mexico")
revilla <- st_read(here("data/processed_data/revilla_new.gpkg"))

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
  filter(between(year, 2013, 2022)) |>
  mutate(displaced = vessel_rnpa %in% displaced)

## STUF BELOW SHOULD BE MOVED TO ITS OWN ANALYSIS SCRIPT AFTER EXPORTING THE PROCESSED TRACKS

# Effort through time
panel <- processed_tracks %>% 
  group_by(year, displaced, before, vessel_rnpa) %>% 
  summarize(h = sum(hours, na.rm = T) / 24,
            .groups = "drop") %>% 
  ungroup() |> 
  mutate(after = !before,
         event = year - 2018)

ggplot(panel, aes(x = year, y = h)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1, fill = "cadetblue") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Average fishing effort (days)")

ggplot(panel, aes(x = year, y = h, fill = displaced)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1) +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Average fishing effort (days)")

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


m1 <- fixest::feols(log(h) ~ after * displaced,
            data = panel,
            cluster ~ vessel_rnpa)

m2 <- fixest::feols(log(h) ~ i(event, displaced, -1) | vessel_rnpa + year,
                    data = panel)

iplot(m2)

# Build concave hulls
A <- processed_tracks |> 
  filter(before, displaced) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(a = 1) |> 
  group_by(a) |> 
  summarize(.groups = "drop") |> 
  st_concave_hull(0.1) |> 
  st_difference(mex) |> 
  select(a)

B <- processed_tracks |> 
  filter(before, !displaced) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(b = 1) |> 
  group_by(b) |> 
  summarize(.groups = "drop") |> 
  st_concave_hull(0.1) |> 
  st_difference(mex) |> 
  select(b)

AB <- st_union(A, B) |> 
  st_make_valid()

# Build concave hulls
A_after <- processed_tracks |> 
  filter(!before, displaced) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(a = 1) |> 
  group_by(a) |> 
  summarize(.groups = "drop") |> 
  st_concave_hull(0.1) |> 
  st_difference(mex) |> 
  st_difference(revilla) |> 
  select(a)

B_after <- processed_tracks |> 
  filter(!before, !displaced) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(b = 1) |> 
  group_by(b) |> 
  summarize(.groups = "drop") |> 
  st_concave_hull(0.1) |> 
  st_difference(mex) |> 
  st_difference(revilla) |> 
  select(b)

AB_after <- st_union(A_after, B_after) |> 
  st_make_valid()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = A, fill = "transparent", color = "blue") + 
  geom_sf(data = B, fill = "transparent", color = "red") + 
  geom_sf(data = AB, fill = "transparent",  color = "purple")

ggplot() +
  geom_sf(data = A_after, fill = "transparent", color = "blue") + 
  geom_sf(data = B_after, fill = "transparent", color = "red") + 
  geom_sf(data = AB_after, fill = "transparent",  color = "purple")

plot(AB, reset = F, max.plot = 1)
plot(AB_after[,1], add = T, col = "red")

# EXPORT ######################################################################

# X ----------------------------------------------------------------------------
# 
# 
# How big is Revillagigedo relative to the union of A and B?


# USed in the abstract
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


# Vessels within the spillover area
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
  

length(bef_inside)
length(after_spill)

sum(bef_spill %in% after_spill)
