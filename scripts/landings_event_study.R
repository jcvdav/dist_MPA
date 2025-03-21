################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Analyze changes in landings
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------

# Load data --------------------------------------------------------------------
landings <- readRDS(here("data", "processed", "tuna_landings.rds")) %>% 
  ungroup() %>% 
  filter(year >= 2007) %>% 
  complete(vessel_rnpa, nesting(year, ba), fill = list(landed_weight = 0))
vessel_info <- readRDS(here("data", "processed", "clean_vessel_info.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

landings_panel <- inner_join(landings, vessel_info, by = "vessel_rnpa")


ggplot(landings_panel, aes(x = year, y = landed_weight, fill = disp)) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, size = 1)

feols(landed_weight ~ i(year, displaced, 2017),
      data = landings_panel,
      panel.id = ~vessel_rnpa + year,
      cluster = ~vessel_rnpa) %>% 
  tidy() %>% 
  filter(!term == "(Intercept)") %>% 
  mutate(term = as.numeric(str_extract(term, pattern = "-?[:digit:]+"))) %>% 
  ggplot(aes(x = term, y = estimate, ymin = estimate-std.error, ymax = estimate + std.error)) +
  geom_pointrange(shape = 21, size = 1, fill = "cadetblue") +
  geom_vline(xintercept = 2017.5, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x = "Year",
       y = "Estimate",
       fill = "Status")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------