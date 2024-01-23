# Setup ----
library(tidyverse)
library(readxl)
library(sf)

# Data ----
es_blkgrp <- readRDS("flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("tract_names.xlsx", sheet = "blkgrp2020")


# Prep ----
## add block group names ----
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

es_blkgrp <- es_blkgrp %>% 
  left_join(blkgrp_names) 


# Maps ----
## Block groups ----
# with names
ggplot(es_blkgrp) +
  geom_sf() +
  geom_sf_text(aes(label = str_wrap(names, 1)), # 1 word per line
               size = 2,
               fun.geometry = sf::st_centroid
  ) +
  theme_void()

# split only at commas
es_blkgrp <- es_blkgrp %>% 
  mutate(names_multilines = str_replace_all(names, ", ", ",\n"))

ggplot(es_blkgrp) +
  geom_sf(color = "white", fill = "grey85", linewidth = 1) +
  geom_sf_text(aes(label = names_multilines), 
               size = 2,
               fun.geometry = sf::st_centroid
  ) +
  theme_void()

# ggsave("blkgrp_names.png")

# this might help?
# https://yutannihilation.github.io/ggsflabel/index.html

## Composite ----
ggplot(es_blkgrp) +
  geom_sf(aes(fill = mean_HazardNumber), color = "white") +
  scale_fill_gradient(low = "grey", high = "darkblue", 
                      name = "Average\nFlood\nHazard\nCompsite") +
  theme_void() +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = c(.8, .4))

# ggsave("blockgroup_composite_map.png")
