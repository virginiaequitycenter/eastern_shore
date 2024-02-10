# Setup ----
library(tidyverse)
library(readxl)
library(sf)
library(ggrepel)

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

## Jacob's revisions ----
set.seed(999)

# Pull out centroids to repel away from
centroids <- sf::st_centroid(es_blkgrp)

xs <- lapply(centroids$geometry, function(x) x[1]) |> unlist()
ys <- lapply(centroids$geometry, function(x) x[2]) |> unlist()

ggplot(es_blkgrp) +
  geom_sf(color = "white", fill = "grey85", linewidth = 1) +
  ggrepel::geom_text_repel(aes(x = xs, y = ys, label = names_multilines), 
                           size = 2,
                           min.segment.length = 0,
                           max.overlaps = Inf,
                           segment.alpha = .30
  ) +
  theme_void()

# ggsave("blkgrp_names.png")

## alternatives
## only some lines
# ggplot(es_blkgrp) +
#   geom_sf(color = "white", fill = "grey85", linewidth = 1) +
#   ggrepel::geom_text_repel(aes(x = xs, y = ys, label = names_multilines), 
#                            size = 2,
#                            min.segment.length = .20,
#                            max.overlaps = Inf,
#                            segment.alpha = .30
#   ) +
#   theme_void()
# 
# ## more repulsion, longer lines
# ggplot(es_blkgrp) +
#   geom_sf(color = "white", fill = "grey85", linewidth = 1) +
#   ggrepel::geom_text_repel(aes(x = xs, y = ys, label = names_multilines), 
#                            size = 2,
#                            min.segment.length = 0,
#                            max.overlaps = Inf,
#                            segment.alpha = .30,
#                            box.padding = 0.40,
#                            max.iter = 50000, max.time = 5
#   ) +
#   theme_void()


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

