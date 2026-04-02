library(here)
library(sf)
library(tidyverse)
source(here(".Rprofile"))


tracks <- readRDS(file = here("data", "processed", "scored_tracks.rds")) |> 
  filter(lat > 0)
         # year == 2021)

revilla <- st_read("data/processed/revilla_new.gpkg")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
get_gini <- function(bin = 1, data = data) {
 
  spatial_gini <- data %>%
    mutate(lon = (floor(lon / bin) * bin) + bin / 2,
           lat = (floor(lat / bin) * bin) + bin / 2) %>%
    group_by(year, lon, lat, vessel_rnpa) %>%
    summarize(h = sum(hours, na.rm = T),
              .groups = "drop") %>%
    filter(h > 0) %>%
    ungroup() %>%
    # complete(vessel_rnpa, nesting(year, lon, lat), fill = list(h = 0)) %>%
    group_by(year, lon, lat) %>%
    summarize(gini = gini(h),
              h = sum(h, na.rm = T),
              n = n_distinct(vessel_rnpa),
              .groups = "drop")
  
  res <- spatial_gini |> 
    mutate(bin = bin) |> 
    select(year, lat, lon, bin, gini, h, n)
  
  return(res)  
  
}


general <- tracks |> 
  group_by(year, vessel_rnpa) %>%
  summarize(h = sum(hours, na.rm = T),
            .groups = "drop") %>%
  filter(h > 0) %>%
  ungroup() %>%
  mutate(bin = "all") |> 
  group_by(bin, year) %>%
  summarize(gini = gini(h),
            h = sum(h, na.rm = T),
            n = n_distinct(vessel_rnpa),
            .groups = "drop")

bins <- c(0.01, 0.05, 0.1, 0.5, 1, 5)

data <- map_dfr(bins, get_gini, data = tracks)

viz_summary <- data |> 
  group_by(year, bin) |> 
  summarize(gini_mean = mean(gini),
            gini_sd = sd(gini),
            h_mean = mean(h),
            h_sd = sd(h),
            n_mean = mean(n),
            n_sd = sd(n))

pos <- position_jitter(width = 0.1, height = 0)

gini_resolution <- ggplot(viz_summary, aes(x = bin, y = gini_mean, color = year, group = year)) +
  geom_hline(data = general, aes(yintercept = mean(gini))) +
  geom_hline(data = general, aes(yintercept = mean(gini) + sd(gini)),
             linetype = "dashed") +
  geom_hline(data = general, aes(yintercept = mean(gini) - sd(gini)),
             linetype = "dashed") +
  geom_smooth(method = "loess") +
  geom_pointrange(aes(ymin = gini_mean - gini_sd,
                      ymax = gini_mean + gini_sd,
                      group = year),
                  position = pos) +
  scale_x_continuous(trans = "log10") +
  lims(y = c(0, NA))

gini_resolution

ggplot(data |> filter(bin == 1, n >= 2), aes(x = lon, y = lat, fill = gini)) +
  geom_tile() +
  facet_wrap(~bin) +
  coord_equal() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw()

## EXPORT ######################################################################

ggsave(here("results", "figures", "gini_vs_resolution.png"), gini_resolution,
       width = 7, height = 5, dpi = 300)












