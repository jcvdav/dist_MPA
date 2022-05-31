######################################################
#title#
######################################################
# 
# Purpose
#
######################################################



library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Tabular
scored <- readRDS(here("data", "processed_data", "scored_tracks.rds"))

usage_by_year <- scored %>% 
  filter(kmeans_fishing) %>% 
  count(year, inside) %>% 
  mutate(inside = ifelse(inside, "Inside", "Outside")) %>% 
  group_by(year) %>% 
  mutate(pct = n / sum(n))

pct_outside <- 1 - (usage_by_year %>%
                      filter(year <= 2017, inside == "Inside") %>%
                      pull(pct) %>%
                      mean())

annual_pct_inside_outside <- ggplot(data = usage_by_year,
       mapping = aes(x = year, y = pct, fill = inside)) +
  geom_col(col = "black") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  geom_hline(yintercept = pct_outside, linetype = "dashed") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, pct_outside, 1), labels = scales::percent) +
  labs(x = "Year",
       y = "Percent of annual fishing effort",
       fill = "Location")

ggsave(plot = annual_pct_inside_outside,
       filename = here("docs", "img", "annual_pct_inside_outside.png"),
       width = 6,
       height = 3)


usage_by_year_vessel <- scored %>% 
  filter(kmeans_fishing,
         before) %>% 
  count(year, vessel_rnpa, inside) %>% 
  mutate(inside = ifelse(inside, "Inside", "Outside")) %>% 
  group_by(year, vessel_rnpa) %>% 
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  filter(!inside == "Inside") %>% 
  mutate(pct = 1 - pct) %>% 
  select(-inside)

annual_pct_inside_by_vessel <- ggplot(data = usage_by_year_vessel) +
  geom_violin(mapping = aes(x = year, y = pct, group = year)) +
  geom_jitter(mapping = aes(x = year, y = pct, group = vessel_rnpa),
              height = 0, width = 0.3,
              shape = 21,
              color = "black",
              fill = "steelblue") +
  geom_hline(yintercept = 1 - pct_outside, linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.15)) +
  labs(x = "Year",
       y = "Percent of annual fishing effort")


ggsave(plot = annual_pct_inside_by_vessel,
       filename = here("docs", "img", "annual_pct_inside_by_vessel.png"),
       width = 6,
       height = 3)
