
## read in three packages
library(dplyr)
library(ggplot2)
library(readr)


## read in a R function to add the zone to a graph
source("http://www-math.bgsu.edu/~albert/ACTION/zone.R")


## read in Tribe starter data for 2018 season
tribe <- read_csv("http://bit.ly/tribepitching")


## remove some rare events and define type variable
tribe %>% 
  filter((description %in% 
          c("hit_by_pitch", "pitchout")) == FALSE) %>% 
  mutate(Type = ifelse(description %in%
        c("ball", "blocked_ball", 
          "called_strike"), "Called", "Swing")) -> tribe


## only look at data for Corey Kluber
ck <- filter(tribe, player_name == "Corey Kluber")

# what pitches does Kluber throw?
ck %>% 
  group_by(pitch_type) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(pitch_type, N)) +
  geom_col()


## How do these pitches compare with respect to speed?
ck %>% 
  ggplot(aes(pitch_type, release_speed)) +
  geom_boxplot()


## How do these pitches compare with respect to movement?
ck %>% 
  ggplot(aes(pfx_x, pfx_z, color = pitch_type)) +
  geom_point()


## Let's focus on Kluber's curveballs and cutters
twopitches <- filter(ck,
                     pitch_type %in% c("CU", "FC"))


## Where does he throw these two pitches?
twopitches %>% 
  ggplot(aes(plate_x, plate_z)) +
  geom_point() +
  facet_wrap(~ pitch_type) +
  add_zone()


## Locations of called pitches?
ck %>% filter(Type == "Called") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## Location and outcome of swung pitches?
ck %>% filter(Type == "Swing") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## Location and outcome of swung curve balls?
ck %>% filter(Type == "Swing", pitch_type == "CU") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


### Location and outcome of swung cutters?
ck %>% filter(Type == "Swing", pitch_type == "FC") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## Data for Kluber and Baumer
kluber_bauer <- filter(tribe, player_name %in%
   c("Corey Kluber", "Trevor Bauer"))


## Compare outcomes of swung changeups
kluber_bauer %>% 
  filter(Type == "Swing", pitch_type == "CH") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone() +
  facet_grid(~ player_name)


# Define miss variable
kluber_bauer %>% 
  mutate(Miss = ifelse(description %in%
  c("swinging_strike", "swinging_strike_blocked"),
  TRUE, FALSE)) -> kluber_bauer


# which pitcher is better in getting misses?
kluber_bauer %>% 
  filter(Type == "Swing") %>% 
  group_by(player_name, Miss) %>% 
  count()

