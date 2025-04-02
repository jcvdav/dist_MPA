library(here)
library(sf)
library(tidyverse)

test <- readRDS(here("data", "processed_data", "scored_tracks.rds")) |> 
  filter(sardine == 0,
         kmeans_fishing,
         year(datetime) == 2019,
         implied_speed_knots > 1) |> 
  arrange(datetime) |> 
  mutate(decade = round(year(datetime) / 10) * 10,
         year = year(datetime),
         month = month(datetime),
         week = week(datetime),
         day = yday(datetime)) |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) |> 
  st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0")

proc <- function(x, grp){
  x |> 
    group_by_at(c("name", grp)) |> 
    count() |> 
    ungroup() |> 
    filter(n >= 3) |> 
    st_concave_hull(0.1) |> 
    st_make_valid() %>% 
    mutate(area = st_area(.),
           x = grp[length(grp)]) |> 
    st_drop_geometry() |> 
    group_by(x, name) |> 
    summarize(m = mean(area),
              sd = sd(area),
              .groups = "drop")
}

# Lets' try by day, week, month, year, decade
day <- proc(test, c("decade", "year", "month", "week", "day"))

week <- proc(test, c("decade", "year", "month", "week"))

month <- proc(test, c("decade", "year", "month"))

year <- proc(test, c("decade", "year"))

decade <- proc(test, c("decade"))

all <- bind_rows(day, week, month, year, decade) |> 
  mutate(x = fct_relevel(x, "day", "week", "month", "year", "decade"),
         xx = case_when(x == "day" ~ 1,
                        x == "week" ~ 7,
                        x == "month" ~ 30,
                        x == "year" ~ 365,
                        x == "decade" ~ 3650),
         m = as.numeric(m) / 1e6) 

ggplot(all, aes(x = xx, y = m)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(all, aes(x = xx, y = m / xx)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")


# I need to make sure that wehn I group by day.. month... year... it's actually using data within the group. For example, if I groiup by year but the vessel is only active in one month, then the year is not really capturing anything else.
# # I also need to add distance_to_last again
# 
# Make versions where y axis is hours, distance, and area