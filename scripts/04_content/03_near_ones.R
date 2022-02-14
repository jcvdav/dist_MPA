
library(here)
library(sf)
library(ggrdiges)
library(tidyverse)

# Tablular
scored <- readRDS(here("data", "processed_data", "scored_tracks.rds"))

# Spatial
ports <- st_read(here("data", "processed_data", "ports.gpkg"))
new_revilla <- st_read(here("data", "processed_data", "revilla_new.gpkg"))

dist_to_mpa <- ports %>% 
  mutate(distance = st_distance(geom, new_revilla, by_element = T)) %>% 
  st_drop_geometry() %>% 
  units::drop_units()

disp <- scored %>%
  filter(before,
         kmeans_fishing) %>%
  count(home_port, year, eu_rnpa, owner_rnpa, vessel_rnpa, inside) %>%
  mutate(inside = ifelse(inside, "Inside", "Outside")) %>%
  group_by(year, vessel_rnpa) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>%
  filter(!inside == "Inside") %>% 
  mutate(pct = 1 - pct) %>% 
  select(-inside) %>% 
  left_join(dist_to_mpa, by = "home_port") %>% 
  mutate(id = factor(group_indices(., vessel_rnpa)))

dist_and_usage <- ggplot(data = disp) +
  geom_jitter(mapping = aes(x = distance / 1e3, y = pct, fill = eu_rnpa),
              height = 0, width = 20,
              shape = 21, color = "black") +
  geom_hline(yintercept = 0.023, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Distance from home-port to MPA border (km)",
       y = "Percent of total effort within MPA") +
  theme_bw() +
  theme(legend.position = "None")


ggsave(plot = dist_and_usage,
       filename = here("docs", "img", "distance_and_usage.png"),
       width = 6,
       height = 3)

## CLOSES ONES _________________________________________________________________

closest <- disp %>% 
  filter(home_port == "MAZATLAN")

closest_rnpa <- closest %>% 
  pull(vessel_rnpa) %>% 
  unique()

vessel_info <- scored %>% 
  filter(vessel_rnpa %in% closest_rnpa) %>%
  select(vessel_rnpa, eu_rnpa, owner_rnpa, tuna, sardine, shrimp, contains("num"), engine_power_hp) %>%
  distinct()

kernell <- ggplot(data = closest,
       mapping = aes(x = pct, y = id, fill = eu_rnpa)) +
  geom_density_ridges(bandwidth = 0.009, alpha = 0.5) +
  geom_vline(xintercept = 0.023, linetype = "dashed") +
  coord_flip() +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "None") +
  labs(y = "Vessel id", x = "Percent of annual fishing\neffort inside the MPA",
       subtitle = "Kernell density, k = 0.009",
       title = "Historical use, color by ownersip")

ggsave(plot = kernell,
       filename = here("docs", "img", "percent_effort_inside_closest_vessels.png"),
       width = 6,
       height = 4)


## FROM THE DISPLACED EFFORT, WHO WAS RESPONSIBLE FOR THE MOST?_________________

pct_displaced <- disp %>% 
  filter(year == 2017) %>%
  group_by(id, vessel_rnpa, eu_rnpa) %>%
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n)) %>%
  mutate(id = fct_reorder(id, pct, .desc = T)) %>% 
  arrange(id) %>% 
  mutate(nves = 1:nrow(.) / nrow(.),
         cum = cumsum(pct))


ggplot(data = pct_displaced, aes(x = id, y = pct, fill = eu_rnpa)) +
  geom_col(color = "black", alpha = 0.5) +
  theme_bw()

ggplot(data = pct_displaced, aes(x = nves, y = cum, fill = eu_rnpa)) +
  geom_point(color = "black", size = 3, shape = 21) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  theme_bw()


##############
# Determining fishing vs not fishing


local_tracks %>% 
  filter(vessel_rnpa == most) %>% 
  filter((between(lon, -116.471415, -109.078093) & between(lat, 16.655231, 21.008631))) %>% 
  group_by(inside, before) %>% 
  summarize(mean = mean(speed),
            median = median(speed),
            min = min(speed),
            max = max(speed))

local_tracks %>% 
  filter(vessel_rnpa == most) %>% 
  filter((between(lon, -116.471415, -109.078093) & between(lat, 16.655231, 21.008631))) %>% 
  mutate(inside = ifelse(inside, "Inside", "Outside"),
         before = ifelse(before, "Before", "After")) %>%
  ggplot(aes(x = speed)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(inside ~ before)

local_tracks %>% 
  filter(vessel_rnpa == most) %>% 
  filter((between(lon, -116.471415, -109.078093) & between(lat, 16.655231, 21.008631))) %>% 
  mutate(fishing = between(speed, 1, 10)) %>% 
  ggplot(aes(x = lon, y = lat, color = fishing)) +
  geom_point(pch = ".") +
  facet_wrap(vessel_rnpa~before) +
  coord_equal() +
  geom_rect(xmin = -115.471415, xmax = -110.078093, ymin = 17.655231, ymax = 20.008631, fill = "transparent", color = "black")


local_tracks %>% 
  filter(vessel_rnpa == most) %>%
  select(lat, lon, before, inside, speed) %>% 
  mutate(fishing = kmeans(x = select(., -c(lon, lat)), centers = 2)$cluster) %>% 
  mutate(inside = ifelse(inside, "Inside", "Outside"),
         before = ifelse(before, "Before", "After")) %>%
  filter((between(lon, -116.471415, -109.078093) & between(lat, 16.655231, 21.008631))) %>% 
  ggplot(aes(x = lon, y = lat, color = fishing)) +
  geom_point(pch = ".") +
  facet_wrap(~before) +
  coord_equal() +
  geom_rect(xmin = -115.471415, xmax = -110.078093, ymin = 17.655231, ymax = 20.008631, fill = "transparent", color = "black")


scored %>%
  filter(speed_fishing, !before, inside) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_point(pch = ".") +
  # facet_wrap(~before) +
  coord_equal() +
  geom_rect(xmin = -115.471415, xmax = -110.078093, ymin = 17.655231, ymax = 20.008631, fill = "transparent", color = "black")


pts <- scored %>% 
  filter(speed_fishing,
         vessel_rnpa == most) %>% 
  dplyr::select(lon, lat)

pts2 <- pts
coordinates(pts2) <- c("lon", "lat")

hull <- pts[chull(pts),]
mhull <- mcp(pts2, percent = 90)

ggplot(pts, aes(x = lon, y = lat)) +
  geom_point(data = hull) +
  geom_point(pch = ".")

