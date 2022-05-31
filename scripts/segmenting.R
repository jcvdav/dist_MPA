
dist_from_last <- function(x, y){
  dist_x <- c(NA, diff(x))
  dist_y <- c(NA, diff(y))
  
  dist <- sqrt((dist_x) ^ 2 + (dist_y) ^ 2)
  return(dist)
}


add_distance <- function(data){
  data %>% 
    arrange(datetime) %>% 
    mutate(dist_from_last = dist_from_last(x = lon, y = lat),
           time_from_last = c(NA, diff(datetime)))
}

scored <- readRDS(here("data", "processed_data", "scored_tracks.rds"))

a <- scored %>% 
  group_by(vessel_rnpa) %>% 
  nest() %>% 
  mutate(data = map(data, add_distance)) %>% 
  unnest(data)


x <- runif(10, 0, 10)
y <- runif(10, 0, 10)

plot(x, y, type = "l")


tibble(id = 1:10,
       x = x, y = y) %>% 
  mutate(dist = dist_from_last(x, y)) %>% 
  ggplot(aes(x = x, y = y, color = dist)) +
  geom_path(arrow = arrow(length = unit(x = 5, units = "mm")))

colnames(scored)

mex <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")

filter(scored, vessel_rnpa == "00041632") %>% 
  arrange(datetime) %>% 
  mutate(dist = dist_from_last(lon, lat),
         time = datetime - c(NA, lag(datetime)),) %>% 
  select(lon, lat, datetime, dist, speed, time) %>% 
  ggplot() +
  geom_sf(data = mex) +
  geom_path(aes(x = lon, y = lat, color = speed),
            arrow = arrow(length = unit(x = 1, units = "mm")))

  
## TRYING SEGMENTS
test <- filter(scored, vessel_rnpa %in% c("00041632")) %>% 
  group_by(vessel_rnpa) %>% 
  arrange(datetime) %>% 
  # drop_na(datetime) %>%
  mutate(dist = dist_from_last(lon, lat),
         lag_datetime = lag(datetime),
         time = difftime(time1 = datetime, time2 = lag_datetime, units = "hours")) %>% 
  mutate(leq24h = time >= 24 | is.na(datetime),
         leq24h = ifelse(is.na(leq24h), T, leq24h),
         seg_id = paste(vessel_rnpa, cumsum(leq24h), sep = "_")) %>% 
  ungroup() %>% 
  select(vessel_rnpa, lon, lat, datetime, dist, speed, time, leq24h, seg_id)

seg_info <- test %>% 
  group_by(vessel_rnpa, seg_id) %>% 
  summarize(n_pos = n(),
            n_pos_mov = sum(speed > 0),
            good_segment = n_pos_mov > 24 * 7,
            seg_start = min(datetime),
            seg_end = max(datetime))


ggplot(data = test %>% 
         filter(seg_id %in% c("00041632_4", "00043422_8"))) +
  geom_sf(data = mex) +
  geom_path(aes(x = lon, y = lat, color = datetime),
            arrow = arrow(length = unit(x = 1, units = "mm"))) +
  facet_wrap(~seg_id)
