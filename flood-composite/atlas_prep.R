## Prep data for atlas prototype

library(tidyverse)
library(readxl)
library(sf)

# Data ----
es_blkgrp <- readRDS("../flood-composite/flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("../flood-composite/tract_names.xlsx", sheet = "blkgrp2020")
pop <- read_csv("../flood-composite/population_blkgrp.csv")

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

## add demographic/population/housing data ----
pop <- pop %>% 
  select(GEOID, totpop_est, tothh_est, jobs_total_w, jobs_total_h,
         whiteper_est, blackper_est, ltnxper_est, remainper_est, 
         age17per_est, age65per_est, onlineper_est, 
         ownhhper_est, renthhper_est, bldgage70per_est, 
         medhhinc_est, medrent_est, medhome_est, 
         percent_lowage_w, percent_lowage_h
  ) %>% 
  mutate(GEOID = as.character(GEOID))

es_blkgrp_pop <- es_blkgrp %>% 
  left_join(pop)

## Save ----
write_csv(es_blkgrp_pop, "esva-atlas-prototype/blkgrp_pop_data.csv")
