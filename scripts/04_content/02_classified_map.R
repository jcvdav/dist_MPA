

library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Tabular
scored <- readRDS(here("data", "processed_data", "scored_tracks.rds"))

# Spatial
mex <- ne_countries(country = "Mexico", returnclass = "sf", scale = "medium")
mex_eez <- st_read(here("data", "processed_data", "mex_ees.gpkg"))
old_revilla <- st_read(dsn =  here("data", "processed_data", "revilla_old.gpkg"))
new_revilla <- st_read(dsn = here("data", "processed_data", "revilla_new.gpkg"))
ports <- st_read(dsn = here("data", "processed_data", "ports.gpkg"))

revilla_bbox <- st_bbox(new_revilla)

mex_high <- ne_countries(country = "Mexico", returnclass = "sf", scale = "large") |> 
  st_crop(revilla_bbox)


before <- scored %>% 
  filter(lat < 90) %>% 
  filter(before)

after <- scored %>% 
  filter(lat < 90) %>% 
  filter(!before)

## PROCECSSING #################################################################

## MAP OF ALL EFFORT BEFORE THE EXPANSION ______________________________________

# Prep data
bined_before <- before %>% 
  mutate(lon_bin = (round(lon / 0.5) + 0.25) * 0.5,
         lat_bin = (round(lat / 0.5) + 0.25) * 0.5) %>% 
  count(year, lon_bin, lat_bin) %>% 
  group_by(lon_bin, lat_bin) %>% 
  summarize(n = mean(n))

# Make figure
total_hours_before <- ggplot() + 
  geom_tile(data = bined_before,
            mapping = aes(x = lon_bin, y = lat_bin, fill = n)) +
  geom_sf(data = mex_eez, fill = "transparent", color = "black", size = 0.3) +
  geom_sf(data = mex, color = "black", size = 0.3) +
  geom_sf(data = ports, color = "black", fill = "steelblue", shape = 21, size = 3) +
  # geom_sf(data = old_revilla, fill = "transparent", color = "red") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  theme_void() +
  scale_fill_continuous(trans = "log10") +
  labs(title = "Average activity",
       fill = "Hours")

# Export figure
ggsave(plot = total_hours_before,
       filename = here("results", "img", "total_hours_before_map.png"),
       width = 6,
       height = 3)

## ZOOMED IN MAP OF EFFORT FISHING/NOT FISHING _________________________________

most <- filter(before, kmeans_fishing, inside) |>
  group_by(vessel_rnpa) |>
  summarize(h = sum(hours),
            .groups = "drop") |>
  arrange(desc(h)) |>
  head(1) |> 
  pull(vessel_rnpa)
# never <- "00041632"

most_before_zoom <- before %>% 
  filter(year == 2016,
         inside,
         vessel_rnpa == most)


world <- ne_countries(scale = "large") |> 
  st_break_antimeridian(lon_0 = -90)

lsmpas <- st_read("data/raw_data/clean_lmpas.gpkg")

map1 <- ggplot() + 
  geom_sf(data = lsmpas, fill = "steelblue", color = "steelblue") +
  geom_sf(data = new_revilla, fill = "red", color = "red") +
  geom_sf(data = world) +
  theme_void() +
  # theme(panel.background = element_rect(fill = "#d6f1ff"),
        # panel.grid = element_blank()) +
  labs(x = "Longitude",
       y = "Latitude") +
  coord_sf(crs = "EPSG:8858")
  

map1

base_map <- ggplot() +
  geom_sf(data = mex_high, color = "black", linewidth = 0.5, fill = "gray") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", linewidth = 1) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#d6f1ff"),
        panel.grid = element_blank()) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Who used to fish here?",
       subtitle = "")

base_map

unclassified <- base_map +
  geom_point(data = most_before_zoom,
             mapping = aes(x = lon, y = lat),
             size =  0.2, color = "black") +
  labs(subtitle = "Hourly GPS locations of 1 vessel during 2016 but... When were they fishing?")

unclassified

classified <- base_map +
  geom_point(data = most_before_zoom,
             mapping = aes(x = lon, y = lat, alpha = kmeans_fishing),
             size = 0.2) +
  scale_alpha_manual(values = c(0.25, 1)) +
  theme(legend.position = "None") +
  labs(subtitle = "We can use machine learning to clasify fishing / no fishing")

classified

unclassified_classified <- base_map +
  geom_point(data = most_before_zoom |> 
               filter(kmeans_fishing),
             mapping = aes(x = lon, y = lat),
             size =  0.2, color = "black") +
  labs(subtitle = "During 2016, fishing vessel `Maria Antonieta` fished for 222 hours")

unclassified_classified


ggsave(plot = unclassified,
       filename = here("docs", "img", "most_unclassified_before.png"),
       width = 6,
       height = 4)

ggsave(plot = classified,
       filename = here("docs", "img", "most_classified_before.png"),
       width = 6,
       height = 4)


# Track Maria Antonieta right after closure
# Revilla closed on "2017-11-27"
p <- base_map +
  geom_point(data = scored |> 
               filter(name == "AZTECA 3",
                      between(date(datetime), ymd("2017-01-01"), ymd("2018-12-31"))) |> 
               mutate(period = ifelse(before, "Before", "After"),
                      period = fct_relevel(period, "Before", "After"),
                      t = yday(datetime)),
             aes(x = lon, y = lat, alpha = kmeans_fishing), size = 0.2) +
  scale_alpha_manual(values = c(0.25, 1)) +
  facet_wrap(~period) +
  transition_time(t) +
  shadow_wake(wake_length = 0.5) +
  theme(legend.position = "None")

animate(p, nframes = 365, fps = 10)

p <- base_map +
  geom_point(data = before |> 
               filter(between(datetime, ymd_hms("2016-05-05 14:40:00"), ymd_hms("2016-05-11 21:04:00")),
                      name %in% c("MARIA ANTONIETA")),
             mapping = aes(x = lon, y = lat, alpha = kmeans_fishing), size = 1) +
  transition_time(datetime) +
  scale_alpha_manual(values = c(0.25, 1)) +
  shadow_wake(wake_length = 0.5) +
  theme(legend.position = "None") +
  labs(title = "High-resolution fishing behavior")

animate(p, nframes = 144, fps = 10)

anim_save(animation = animate(p, nframes = 144, fps = 10),
          filename = "revilla_animation.gif")

## FISHING EFFORT MAP __________________________________________________________

fishing_before <- before %>% 
  filter(kmeans_fishing)


ggplot() +
  geom_hex(data = fishing_before,
             mapping = aes(x = lon, y = lat), binwidth = 0.5, color = "transparent") +#,
             # pch = ".",
             # color = "black") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  scale_fill_continuous(trans = "log10") +
  theme_void() +
  theme(legend.position = "None")

fishing_after <- after %>% 
  filter(k2means_fishing)


ggplot() +
  geom_hex(data = fishing_after,
           mapping = aes(x = lon, y = lat), binwidth = 0.5, color = "transparent") +#,
  # pch = ".",
  # color = "black") +
  geom_sf(data = new_revilla, fill = "transparent", color = "red", size = 0.3) +
  scale_fill_continuous(trans = "log10") +
  theme_void() +
  theme(legend.position = "None")





