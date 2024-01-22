
# Setup ----
library(tidyverse)
library(readxl)
library(sf)
library(patchwork)

# Data ----
es_blkgrp <- readRDS("flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("tract_names.xlsx", sheet = "blkgrp2020")

# Prep ----
## make 10 percent columns ----
es_blkgrp <- es_blkgrp %>% 
  mutate(across(starts_with("sum"), ~.x/cells, .names = "per_{.col}"))

## add block group names ----
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

es_blkgrp <- es_blkgrp %>% 
  left_join(blkgrp_names)

## reshape for heatmap ----
es_blkgrp_long <- es_blkgrp %>% 
  select(GEOID, COUNTYFP, names, starts_with("per")) %>% 
  pivot_longer(starts_with("per"), names_to = "variable", values_to = "risk") %>% 
  mutate(variable = str_remove(variable, "per_sum_"),
         variable = factor(variable, levels = c("HighTideFlooding", "vzone.1percent", "azone.1percent", "azone.2percent",
                                                "ss.cat1", "ss.cat2", "ss.cat3", "slr1.ft", "slr2.ft", "slr3.ft"),
                           labels = c("High Tide", "FEMA V (1%)", "FEMA A (1%)", "FEMA A (2%)", 
                                      "Surge Category 1", "Surge Category 2", "Surge Category 3", "Sea Level 1 Foot",
                                      "Sea Level 2 Foot", "Sea Level 3 Foot")))
                                                

# Heat map ----
# https://rpubs.com/mgontar/215319
## components ----
components_legend <- ggplot(es_blkgrp_long, aes(x = variable, 
                                               y = fct_reorder(names, as.numeric(GEOID)), 
                                               fill = risk)) +
  geom_tile(color = "white") +
  coord_equal() + # shrink the length of x relative to y
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "grey", high = "darkblue", name = "") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 5),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0, size = 5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 6), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),  
        plot.margin = margin(0,0,0,0,"pt"),
  )

## composite ----
# how to show composite value? 
# for the moment, the mean across cells in a block group
composite_clean <- ggplot(es_blkgrp, aes(y = fct_reorder(names, as.numeric(GEOID)), 
                                                         x = mean_HazardNumber, 
                                                         fill = mean_HazardNumber)) +
  geom_col() +
  scale_fill_gradient(low = "grey", high = "darkblue") +
  coord_fixed(ratio = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.margin = margin(0,0,0,0,"pt") # may not be doing anything
  ) 
  
# but now the gradient/legends are different --
# may need to shift everything to percentiles, or normalize everything to 0-100?

## combine ----
# patchwork (played with grid.arrange, but wasn't lining up)
tile <- components_legend + composite_clean + 
  plot_layout(ncol = 2) 

# ggsave(plot = tile, filename = "heatmap_tile.png")


# Table version ----
# gt? https://gt.albert-rapp.de/
library(gt)
library(gtExtras)

# reduce to needed columns
es_blkgrp_table <- es_blkgrp %>% 
  st_drop_geometry() %>% 
  select(GEOID, names, starts_with("per"), mean_HazardNumber) %>% 
  rename_with(~str_remove(.x, "per_sum_")) %>% 
  mutate(names = fct_reorder(names, GEOID)) %>% 
  select(-GEOID)

# create vector of new column labels
current_names <- colnames(es_blkgrp_table)
new_names <- c("Area", "1 Foot", "2 Foot", "3 Foot", "V Zone (1%)", "A Zone (1%)", "A Zone (2%)", 
               "Cat 1", "Cat 2", "Cat 3", "High Tide", "Composite")
names(new_names) <- current_names

# create color palette
color_palette <- c("grey", "darkblue")

# table
tbl <- es_blkgrp_table %>% 
  arrange(names) %>% 
  gt() %>% 
  cols_label(.list = new_names) %>% 
  tab_spanner(label = md("**Sea Level Rise**"), columns = 2:4) %>% 
  tab_spanner(label = md("**FEMA Zones**"), columns = 5:7) %>% 
  tab_spanner(label = md("**Storm Surge**"), columns = 8:10) %>% 
  tab_spanner(label = md("**Tide**"), columns = 11) %>% 
  fmt_number(columns = where(is.numeric), decimals = 2) %>% 
  data_color(
    columns = c(2:11),
    palette = color_palette
  ) %>% 
  gt_plt_bar_pct(column = mean_HazardNumber, scaled = FALSE, height = 20, width = 100,
                 fill = "darkblue", background = "grey") %>% 
  tab_options(table.font.size = 10) %>% 
  cols_width(c(2:11) ~ px(50))

# gtsave(tbl, "heatmap_table.png", expand = 10)

# reactable? https://glin.github.io/reactable/
