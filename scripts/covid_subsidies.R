
# REMEMBER THAT I HAD THIS:
# Database: BigQueryConnection
# spp              n
# <chr>        <int>
#   1 others       64117
# 2 shrimp        1563
# 3 sardine        112
# 4 tuna           108
# 5 sardine tuna     2 -> assigned as tuna because saridne might just be bait
# 6 tuna shrimp      2 -> assigned as shrimp because it might just be tuna on the side

vessel_info <- tbl(mex_fisheries, "vessel_info") %>% 
  mutate(spp = case_when(tuna == 0 & sardine == 1 & shrimp == 0 ~ "sardine",
                         tuna == 0 & sardine == 0 & shrimp == 1 ~ "shrimp",
                         tuna == 1 & sardine == 0 & shrimp == 0 ~ "tuna",
                         tuna == 1 & sardine == 1 & shrimp == 0 ~ "tuna",
                         tuna == 1 & sardine == 0 & shrimp == 1 ~ "shrimp",
                         tuna == 0 & sardine == 0 & shrimp == 0 ~ "others",
                         T ~ "others"
  )) %>% 
  select(vessel_rnpa, spp, engine_power_hp)

tracks <- tbl(mex_fisheries, "mex_vms_processed_v_20220912") %>% 
  inner_join(vessel_info, by = "vessel_rnpa") %>% 
  filter(speed > 0) %>% 
  group_by(year, month, spp) %>%
  summarize(h = sum(hours, na.rm = T),
            n = n_distinct(vessel_rnpa))

l_tracks <- collect(tracks) %>% 
  mutate(date = lubridate::ymd(paste(year, month, "15", sep = "-")),
         hn = h / n)
  

ggplot(data = l_tracks,
       mapping = aes(x = date, y = hn, color = spp)) + 
  geom_line() +
  labs(x = "Date",
       y = "Activity (hours / vessel)", 
       color = "Target species") +
  theme_bw()


