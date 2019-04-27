## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE, message = FALSE)


## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)


## ------------------------------------------------------------------------
source("http://www-math.bgsu.edu/~albert/ACTION/zone.R")


## ------------------------------------------------------------------------
hr <- read_csv("http://bit.ly/homerun2017")


## ------------------------------------------------------------------------
hr %>% filter(player_name == "Mike Trout") %>% 
  arrange(game_date) %>% 
  head(1) %>% 
  select(launch_speed, launch_angle, hit_distance_sc,
         hc_x, hc_y, balls, strikes, inning, 
         home_team, away_team)


## ------------------------------------------------------------------------
ggplot() +
  add_zone(Color = "red") +
  geom_point(data = filter(hr, pitch_type == "CU"),
             aes(plate_x, plate_z)) +
  facet_wrap(~ stand)


## ------------------------------------------------------------------------
ggplot(data = hr,
             aes(launch_angle, launch_speed)) +
  geom_point(alpha = 0.2) +
  geom_smooth(color = "red")


## ------------------------------------------------------------------------
hr %>% 
  group_by(player_name) %>% 
  summarize(HR = n()) %>% 
  arrange(desc(HR)) %>% 
  select(player_name, HR) %>% 
  head(10)


## ------------------------------------------------------------------------
hr %>% 
  arrange(desc(launch_speed)) %>% 
  head(1) %>% 
  select(player_name, game_date, launch_speed)


## ------------------------------------------------------------------------
hr %>% 
  arrange(desc(hit_distance_sc)) %>% 
  head(1) %>% 
  select(player_name, game_date, hit_distance_sc)


## ------------------------------------------------------------------------
hr %>% 
  arrange(desc(launch_speed)) %>% 
  tail(1) %>% 
  select(player_name, game_date, launch_speed)


## ------------------------------------------------------------------------
hr %>% 
 mutate(Count = paste(balls, strikes, sep="-")) -> hr


## ------------------------------------------------------------------------
hr %>% 
 group_by(Count) %>% 
 summarize(N = n()) %>% 
 ggplot(aes(Count, N)) +
 geom_col()


## ------------------------------------------------------------------------
ggplot(hr, aes(hit_distance_sc)) +
  geom_histogram(color = "white", fill = "orange") +
  xlim(300, 500)


## ------------------------------------------------------------------------
hr %>% 
 group_by(Count) %>% 
 summarize(LS = mean(launch_speed, na.rm = TRUE)) %>% 
 ggplot(aes(Count, LS)) +
 geom_point()


## ------------------------------------------------------------------------
hr %>% 
 group_by(Count) %>% 
 summarize(LS = mean(hit_distance_sc, na.rm = TRUE)) %>% 
 ggplot(aes(Count, LS)) +
 geom_point()


## ------------------------------------------------------------------------
hr %>% mutate(
  loc_x = hc_x - 125.42,
  loc_y = 198.27 - hc_y,
  spray_angle = atan(loc_x / loc_y) * 180 / pi) %>% 
  ggplot(aes(spray_angle)) +
  geom_histogram(color = "white", fill = "blue") +
  facet_wrap(~ stand)

