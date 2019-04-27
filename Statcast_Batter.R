# Exploring Statcast Home Run Data

## install some packages
library(dplyr)
library(ggplot2)
library(readr)


## read in a R function to add the zone to a graph
source("http://www-math.bgsu.edu/~albert/ACTION/zone.R")


## reads in all home run data from the 2017 season
hr <- read_csv("http://bit.ly/homerun2017")


## explore Mike Trout's first home run that season
hr %>% filter(player_name == "Mike Trout") %>% 
  arrange(game_date) %>% 
  head(1) %>% 
  select(launch_speed, launch_angle, hit_distance_sc,
         hc_x, hc_y, balls, strikes, inning, 
         home_team, away_team)


## plot locations of all home runs hit against curve balls
ggplot() +
  add_zone(Color = "red") +
  geom_point(data = filter(hr, pitch_type == "CU"),
             aes(plate_x, plate_z)) +
  facet_wrap(~ stand)


## plots the launch angle and launch speed for all hr
ggplot(data = hr,
             aes(launch_angle, launch_speed)) +
  geom_point(alpha = 0.2) +
  geom_smooth(color = "red")


## home run leaders in 2017?
hr %>% 
  group_by(player_name) %>% 
  summarize(HR = n()) %>% 
  arrange(desc(HR)) %>% 
  select(player_name, HR) %>% 
  head(10)


## who had the hardest hit home run in 2017?
hr %>% 
  arrange(desc(launch_speed)) %>% 
  head(1) %>% 
  select(player_name, game_date, launch_speed)


## who had the longest hit home run in 2017?
hr %>% 
  arrange(desc(hit_distance_sc)) %>% 
  head(1) %>% 
  select(player_name, game_date, hit_distance_sc)


## who had the softest hit home run in 2017?
hr %>% 
  arrange(desc(launch_speed)) %>% 
  tail(1) %>% 
  select(player_name, game_date, launch_speed)


## introduce a count variable
hr %>% 
 mutate(Count = paste(balls, strikes, sep="-")) -> hr

## home runs hit against different counts
hr %>% 
 group_by(Count) %>% 
 summarize(N = n()) %>% 
 ggplot(aes(Count, N)) +
 geom_col()


## distances of home runs?
ggplot(hr, aes(hit_distance_sc)) +
  geom_histogram(color = "white", fill = "orange") +
  xlim(300, 500)


## launch speeds of home runs?
hr %>% 
 group_by(Count) %>% 
 summarize(LS = mean(launch_speed, na.rm = TRUE)) %>% 
 ggplot(aes(Count, LS)) +
 geom_point()


## mean launch speed of home runs on each count?
hr %>% 
 group_by(Count) %>% 
 summarize(LS = mean(hit_distance_sc, na.rm = TRUE)) %>% 
 ggplot(aes(Count, LS)) +
 geom_point()


## spray angle of home runs, how does it vary by batter side?
hr %>% mutate(
  loc_x = hc_x - 125.42,
  loc_y = 198.27 - hc_y,
  spray_angle = atan(loc_x / loc_y) * 180 / pi) %>% 
  ggplot(aes(spray_angle)) +
  geom_histogram(color = "white", fill = "blue") +
  facet_wrap(~ stand)

