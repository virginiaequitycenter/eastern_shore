# libraries
library(tidyverse)
library(readxl)
library(sf)
library(ggiraph) # https://www.ardata.fr/ggiraph-book/
library(patchwork)
library(ggspatial) # appears to require installation of "rosm" and "prettymapr" as well


# https://albert-rapp.de/posts/ggplot2-tips/17_ggiraph/17_ggiraph.html

# data ----
# flood hazard data
flood_blkgrp <- readRDS("flood_composite_blkgrp.RDS")

# make 10 percent columns
flood_blkgrp <- flood_blkgrp %>% 
  mutate(across(starts_with("sum"), ~.x/cells, .names = "per_{.col}"))

# pop data
pop_blkgrp <- read_csv("population_blkgrp.csv")
pop_blkgrp <- pop_blkgrp %>% 
  separate_wider_delim(NAME, delim = "; ", 
                       names = c("block_grp", "tract", "county", "state")) %>% 
  mutate(GEOID = as.character(GEOID))

# block group names
blkgrp_names <- read_excel("tract_names.xlsx", sheet = "blkgrp2020")
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp),
         names = str_remove(names, "'"))

# join
bg_df <- flood_blkgrp %>% 
  left_join(pop_blkgrp, by = "GEOID") %>% 
  left_join(blkgrp_names, by = "GEOID") 


# flood --- 
# change hover info
bg_df <- bg_df %>% 
  mutate(toolinfo = paste0(names, "\n", 
                           "Percent: ",
                           round(per_sum_HighTideFlooding, 1)))

# make map
gg_map <- ggplot(bg_df) +
  # annotation_map_tile(type = "cartolight") +
  geom_sf_interactive(aes(fill = per_sum_HighTideFlooding,
                          tooltip = toolinfo, data_id = GEOID), 
                      color = "white") +
  scale_fill_gradient(low = "grey90", high = "darkblue", 
                      name = "Percent\nof Area Subject\nto High Tide\nFlooding") +
  theme_void() +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = c(.8, .4))

# make points
gg_point <- ggplot(bg_df, aes(x = per_sum_HighTideFlooding, 
                              y = fct_rev(county),
                              tooltip = toolinfo, 
                              data_id = GEOID)) +
  geom_point_interactive(size = 7, alpha = 0.2, color = "#98817b") +
  scale_x_continuous(limits = c(0, 1),
                     labels = scales::percent_format(scale = 100),
                     name = "Percent of Area") +
  scale_y_discrete(name = "",
                   labels = function(groups) str_wrap(groups, width = 10)) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        strip.background = element_rect(color = "black", fill = "#f1f1f1"),
        strip.text.x = element_text(size = 12),
        strip.placement = "outside")

girafe(ggobj = gg_point + gg_map + plot_layout(widths = c(0.45, 0.65)), 
       width_svg = 12, height_svg = 6) %>%
  girafe_options(opts_hover(css = "fill:darkorange;stroke:darkblue;stroke-width:2px;"))         

# pop ----
# change hover info
bg_df <- bg_df %>% 
  mutate(toolinfo = paste0(names, "\n", 
                           "Percent: ",
                           round(percent_lowage_w, 1),
                           "\n", "Number: ", jobs_lowage_w))

# make map
gg_map <- ggplot(bg_df) +
  # annotation_map_tile(type = "cartolight") +
  geom_sf_interactive(aes(fill = percent_lowage_w,
                          tooltip = toolinfo, data_id = GEOID), 
                      color = "white") +
  scale_fill_gradient(low = "grey90", high = "darkgreen", 
                      name = "Percent\nof Workers\nin Low-wage\nJobs") +
  theme_void() +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = c(.8, .4))

# make points
gg_point <- ggplot(bg_df, aes(x = percent_lowage_w, 
                              y = fct_rev(county),
                              tooltip = toolinfo, 
                              data_id = GEOID)) +
  geom_point_interactive(size = 7, alpha = 0.2, color = "#674c47") +
  scale_x_continuous(limits = c(0, 100),
                     labels = scales::percent_format(scale = 1),
                     name = "Percent of Population") +
  scale_y_discrete(name = "",
                   labels = function(groups) str_wrap(groups, width = 10)) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        strip.background = element_rect(color = "black", fill = "#f1f1f1"),
        strip.text.x = element_text(size = 12),
        strip.placement = "outside")

girafe(ggobj = gg_point + gg_map + plot_layout(widths = c(0.45, 0.65)), 
       width_svg = 12, height_svg = 6) %>%
  girafe_options(opts_hover(css = "fill:darkorange;stroke:darkgreen;stroke-width:2px;"))
