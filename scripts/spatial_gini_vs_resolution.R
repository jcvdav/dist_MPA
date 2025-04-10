library(here)
library(sf)
library(tidyverse)


tracks <- readRDS(file = here("data", "processed", "clean_tracks.rds")) |> 
  filter(location == "at_sea",
         fishing == 1,
         # eez == 8429,
         lat > 0,
         year == 2021,
         hours < 2)

revilla <- st_read("data/processed_data/revilla_new.gpkg")

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
    select(lat, lon, bin, gini, h, n)
  
  return(res)  
  
}


general <- tracks |> 
  group_by(year, vessel_rnpa) %>%
  summarize(h = sum(hours, na.rm = T),
            .groups = "drop") %>%
  filter(h > 0) %>%
  ungroup() %>%
  mutate(bin = "all") |> 
  group_by(bin) %>%
  summarize(gini = gini(h),
            h = sum(h, na.rm = T),
            n = n_distinct(vessel_rnpa),
            .groups = "drop")

bins <- c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)

data <- map_dfr(bins, get_gini, data = tracks)

viz_summary <- data |> 
  group_by(bin) |> 
  summarize(gini_mean = mean(gini),
            gini_sd = sd(gini),
            h_mean = mean(h),
            h_sd = sd(h),
            n_mean = mean(n),
            n_sd = sd(n))

ggplot(viz_summary, aes(x = bin, y = gini_mean)) +
  geom_hline(data = general, aes(yintercept = gini),
             linetype = "dashed") +
  geom_smooth(method = "loess") +
  geom_pointrange(aes(ymin = gini_mean - gini_sd,
                      ymax = gini_mean + gini_sd)) +
  scale_x_continuous(trans = "log10") +
  lims(y = c(0, NA)) 


ggplot(data |> filter(bin == 1, n >= 2), aes(x = lon, y = lat, fill = gini)) +
  geom_tile() +
  facet_wrap(~bin) +
  coord_equal() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw()

# Next steps
# 1) Build plot with x = bin and y = gini == 1 / length(gini)
# 2) See why there is a cluster around clipperton, and see whether it was there before Revilla expansion
# 3) What if I calculate gini for vessels that use a pixel, rather than assuming zeroes for all fleet?












