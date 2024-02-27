# libraries
library(tidyverse)
library(readxl)
library(sf)
library(ggiraph)
library(patchwork)
library(ggspatial)

# https://albert-rapp.de/posts/ggplot2-tips/17_ggiraph/17_ggiraph.html

pal <- c("#3aadfc", "#e74b77")

# pop data
pop_blkgrp <- read_csv("population_blkgrp.csv")
pop_blkgrp <- pop_blkgrp %>% 
  separate_wider_delim(NAME, delim = "; ", 
                       names = c("block_grp", "tract", "county", "state")) %>% 
  mutate(GEOID = as.character(GEOID))

# names
blkgrp_names <- read_excel("tract_names.xlsx", sheet = "blkgrp2020")
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp),
         names = str_remove(names, "'"))

# geo
blkgrp_geo <- read_sf("esva_blkgrp_shapefile.geojson")

# join
bg_df <- blkgrp_geo %>% 
  left_join(pop_blkgrp, by = "GEOID") %>% 
  left_join(blkgrp_names, by = "GEOID") %>% 
  mutate(toolinfo = paste0(names, "\n", 
                           "Percent: ", round(percent_lowage_w, 1),
                           "\n", "Number: ", jobs_lowage_w))


## map
gg_map <- ggplot(bg_df) +
  #annotation_map_tile(type = "cartolight") +
  geom_sf_interactive(aes(fill = percent_lowage_w,
                          tooltip = toolinfo, data_id = GEOID),
                      color = "white") +
  scale_fill_gradient(low = "grey90", high = "darkgreen", 
                      name = "Percent\nof Workers\nin Low-wage\nJobs") +
  theme_void() +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = c(.8, .4))

girafe(ggobj = gg_map)

## points
gg_point <- ggplot(bg_df, aes(x = county, y = percent_lowage_w,
                  color = percent_lowage_w, tooltip = toolinfo, 
                  data_id = GEOID)) +
  geom_point_interactive(size = 7, alpha = 0.2) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::percent_format(scale = 1),
                     name = "Percent of Population") +
  scale_x_discrete(name = "",
                   labels = function(groups) str_wrap(groups, width = 10)) +
  scale_color_gradient(low = "grey", high = "darkgreen", 
                      guide = "none") +
  #scale_color_manual(values = pal, guide = "none") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        strip.background = element_rect(color = "black", fill = "#f1f1f1"),
        strip.text.x = element_text(size = 12),
        strip.placement = "outside")

girafe(ggobj = gg_point)

gg_point_flip <- ggplot(bg_df, aes(x = percent_lowage_w, y = fct_rev(county),
                              color = percent_lowage_w, tooltip = toolinfo, 
                              data_id = GEOID)) +
  geom_point_interactive(size = 7, alpha = 0.2, color = "darkgreen") +
  scale_x_continuous(limits = c(0, 100),
                     labels = scales::percent_format(scale = 1),
                     name = "Percent of Population") +
  scale_y_discrete(name = "",
                   labels = function(groups) str_wrap(groups, width = 10)) +
  # scale_color_gradient(low = "grey", high = "darkgreen", 
  #                      guide = "none") +
  # scale_color_manual(values = pal, guide = "none") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        strip.background = element_rect(color = "black", fill = "#f1f1f1"),
        strip.text.x = element_text(size = 12),
        strip.placement = "outside")

girafe(ggobj = gg_point_flip)

gg_map_tile <- ggplot(bg_df) +
  annotation_map_tile(type = "cartolight") +
  geom_sf_interactive(aes(fill = percent_lowage_w,
                          tooltip = toolinfo, data_id = toolinfo), 
                      color = "white", alpha = 1/2) +
  scale_fill_gradient(low = "grey90", high = "darkgreen", 
                      name = "Percent\nof Workers\nin Low-wage\nJobs") +
  theme_void() +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = c(.8, .4))

# girafe(ggobj = gg_map_tile)


## link them
girafe(ggobj = gg_point + gg_map + plot_layout(widths = c(0.45, 0.65)), 
       width_svg = 10, height_svg = 5) %>%
  girafe_options(opts_hover(css = "fill:darkorange;"))

girafe(ggobj = gg_point_flip + gg_map + plot_layout(widths = c(0.45, 0.65)), 
       width_svg = 10, height_svg = 5) %>%
  girafe_options(opts_hover(css = "fill:darkorange;"))

girafe(ggobj = gg_point_flip + gg_map_tile + plot_layout(widths = c(0.45, 0.65)), 
       width_svg = 10, height_svg = 5) %>%
  girafe_options(opts_hover(css = "fill:darkorange;"))


