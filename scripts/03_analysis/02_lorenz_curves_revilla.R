library(here)
library(tidyverse)
library(scales)

tracks <-
  readRDS(file = here("data", "processed", "scored_tracks.rds"))
vessel_info <- 
  readRDS(file = here("data", "processed", "clean_vessel_info.rds"))

panel <- tracks %>% 
  filter(!after) %>% 
  group_by(year, vessel_rnpa, inside) %>% 
  summarize(h = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  filter(h > 0) %>% 
  group_by(year, vessel_rnpa) %>% 
  mutate(h = h / sum(h, na.rm = T)) %>% 
  ungroup() %>% 
  filter(inside)

vessel_info %>% 
  select(vessel_rnpa, first_year) %>% 
  left_join(panel, by = "vessel_rnpa") %>% 
  replace_na(replace = list(h = 0)) %>% 
  mutate(vessel_rnpa = fct_reorder(vessel_rnpa, h, mean)) %>% 
  ggplot(aes(x = vessel_rnpa, y = h, group = vessel_rnpa)) + 
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, fill = "cadetblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "% Fishing effort within Revilla") +
  theme_bw()


panel2 <- tracks %>% 
  filter(fishing,
         !after) %>% 
  group_by(year, vessel_rnpa, inside) %>% 
  summarize(h = sum(hours, na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(h = h / sum(h, na.rm = T)) %>% 
  ungroup() %>% 
  complete(vessel_rnpa, nesting(year, inside), fill = list(h = 0)) %>% 
  filter(inside) %>%
  group_by(year) %>% 
  arrange(h) %>% 
  mutate(pct_n = 1:n() / n(),
         pct_h = h / sum(h, na.rm = T),
         c_pct_h = cumsum(pct_h))
  

lorenz_revilla <- ggplot(panel2, aes(x = pct_n, y = c_pct_h, group = year)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "% Vessels",
       y = "% Fishing effort within Revilla") +
  theme_bw() +
  coord_equal()

lorenz_revilla

## EXPORT ######################################################################

ggsave(here("results", "figures", "lorenz_revilla.png"), lorenz_revilla,
       width = 5, height = 5, dpi = 300)
