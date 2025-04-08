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
library(fixest)
library(modelsummary)
library(broom)
library(lubridate)
library(tidyverse)

# Load data --------------------------------------------------------------------
tracks <-
  readRDS(file = here("data", "processed_data", "scored_tracks.rds")) 


## PROCESSING ##################################################################

# Create panel -----------------------------------------------------------------
panel <- processed_tracks %>% 
  filter(kmeans_fishing == 1) %>% 
  group_by(year, vessel_rnpa) %>% 
  summarize(h = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  complete(vessel_rnpa, year, fill = list(h = 0)) %>% 
  mutate(after = 1 * (year >= 2018)) |> 
  filter(year < 2024)

## VISUALIZE ###################################################################

# Time series ------------------------------------------------------------------

ggplot(panel, aes(x = year, y = h)) +
  stat_summary(geom = "line", fun = sum) +
  stat_summary(geom = "point", fun = sum, shape = 21, size = 4, fill = "cadetblue") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Total fishing effort (days)")

ggplot(panel, aes(x = year, y = h)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1, fill = "cadetblue") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Average fishing effort (days)")

panel %>% 
  group_by(year, disp) %>% 
  summarize(h = sum(h, na.rm = T)) %>% 
  group_by(disp) %>%
  mutate(h = (h - mean(h)) / sd(h)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = h, fill = disp)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = c(0, 1),
        legend.justification = c(0,1),
        legend.background = element_blank()) +
  labs(x = "Year",
       y = "Normalized fishing effort ([h - mu] / sigma)",
       fill = "Status") +
  scale_fill_brewer(palette = "Set2")

ggplot(panel, aes(x = year, y = h, fill = disp)) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1) +
  geom_smooth(method = "lm", aes(group = paste(year <= 2017, disp)), alpha = 0.1, color = "black") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1,0),
        legend.background = element_blank()) +
  labs(x = "Year",
       y = "Average fishing effort (days)",
       fill = "Status") +
  scale_fill_brewer(palette = "Set2")


m1 <- feols(h ~ year + after * displaced,
            data = panel,
            cluster ~ vessel_rnpa)

m2 <- feols(h ~ after * displaced,
            data = panel %>% filter(between(year - 2017, -5, 5)),
            cluster ~ vessel_rnpa)

modelsummary(list(m1, m2),
             stars = T,
             coef_omit = "year")

# Spatial redistribution -------------------------------------------------------
rast <- tracks %>%
  filter(fishing == 1,
         lon < -80) %>%
  mutate(lon = (floor(lon / 0.5) * 0.5) + 0.25,
         lat = (floor(lat / 0.5) * 0.5) + 0.25) %>%
  group_by(aft, year, lon, lat) %>%
  summarize(h = sum(hours, na.rm = T) / 24) %>%
  ungroup() %>%
  group_by(aft, lon, lat) %>%
  summarize(h = mean(h, na.rm = T)) %>%
  ungroup()

ggplot(data = rast, aes(x = lon, y = lat, fill = log(h))) +
  geom_tile() + 
  facet_wrap(~aft)




rast %>% 
  select(lon, lat, aft, h) %>%
  spread(aft, h) %>%
  mutate(dif = (After - Before)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = dif))


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------