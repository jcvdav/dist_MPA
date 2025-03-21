######################################################
#title#
######################################################
# 
# Lorenz curves
#
######################################################


tracks <- readRDS(here("data", "processed", "clean_tracks.rds")) %>% 
  filter(year < 2022)
landings <- readRDS(here("data", "processed", "tuna_landings.rds")) %>% 
  select(-ba)

processed <- tracks %>% 
  group_by(year, vessel_rnpa) %>% 
  summarize(fh = sum(hours * fishing, na.rm = T),
            h = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(landings, by = c("year", "vessel_rnpa")) %>% 
  complete(vessel_rnpa, nesting(year), fill = list(fh = 0, h = 0, landed_weight = 0)) %>% 
  mutate(ba = ifelse(year <= 2017, "Before", "After"),
         ba = fct_reorder(ba, year))

activity <- processed %>% 
  group_by(year) %>% 
  arrange(h) %>% 
  mutate(vessels = (1:n()) / n(),
         pct = h / sum(h, na.rm = T),
         cpct = cumsum(pct)) %>% 
  ungroup() %>% 
  ggplot(aes(x = vessels, y = cpct, group = year, color = ba)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "% Vessels",
       y = "% Activity",
       color = "Period") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "None") +
  coord_equal()

fishing_hours <- processed %>% 
  group_by(year) %>% 
  arrange(fh) %>% 
  mutate(vessels = (1:n()) / n(),
         pct = fh / sum(fh, na.rm = T),
         cpct = cumsum(pct)) %>% 
  ungroup() %>% 
  ggplot(aes(x = vessels, y = cpct, group = year, color = ba)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "% Vessels",
       y = "% Fishing effort",
       color = "Period") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "None") +
  coord_equal()

landed_plot <- processed %>% 
  group_by(year) %>% 
  arrange(landed_weight) %>% 
  mutate(vessels = (1:n()) / n(),
         pct = landed_weight / sum(landed_weight, na.rm = T),
         cpct = cumsum(pct)) %>% 
  ungroup() %>% 
  mutate(ba = ifelse(year <= 2017, "Before", "After")) %>% 
  ggplot(aes(x = vessels, y = cpct, group = year, color = ba)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "% Vessels",
       y = "% Landings",
       color = "Period") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "None") +
  coord_equal()




# GINI CALCULATION
gini_plot <- processed %>% 
  group_by(year, ba) %>% 
  summarize_at(.vars = vars(fh, h, landed_weight), .funs = gini) %>% 
  pivot_longer(cols = c(fh, h, landed_weight),
               names_to = "measure",
               values_to = "gini") %>% 
  mutate(measure = case_when(measure == "h" ~ "Activity",
                             measure == "fh" ~ "Fishing effort",
                             measure == "landed_weight" ~ "Landings")) %>% 
  ggplot(aes(x = measure, y = gini, fill = ba)) +
  geom_boxplot(color = "black", size = 0.2) +
  geom_point(aes(fill = ba), shape = 21, position = position_dodge(width = 2/3)) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Measure",
       y = "Gini index",
       fill = NULL) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank()) 

cowplot::plot_grid(plot_grid(activity, fishing_hours, landed_plot, ncol = 3),
                   gini_plot, ncol = 1)
