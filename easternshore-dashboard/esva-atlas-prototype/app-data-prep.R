# App data prep file
library(here)
library(shiny)
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(RColorBrewer)
library(tigris)
options(tigris_use_cache = TRUE)
library(highcharter)
library(bslib)

# Set WD
setwd(here("esva-atlas-prototype"))

# Read in/wrangle data ----
## Block Group names
blkgrp_names <- read_excel("app_data/tract_names.xlsx", sheet = "blkgrp2020")
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

## Population data
pop <- read_csv("app_data/population_blkgrp.csv")
pop <- pop %>% 
  mutate(tract_id = as.character(GEOID),
         GEOID = as.character(GEOID)) 

## Climate data
storm_surge <- read_delim("app_data/IsabelStormOutput_upd.txt") %>% 
  mutate(GEOID = as.character(GEOID))

storm_isabel <- storm_surge %>% 
  select(GEOID, PeakSurge_m, MeanSurge_m, InundationAreaFraction) 

storm_isabel_2050 <- storm_surge %>% 
  select(GEOID, PeakSurge2050_m, MeanSurge2050_m, InundationAreaFraction2050) 

new_names <- c("GEOID", "PeakSurge", "MeanSurge", "Frac")
names(storm_isabel) <- new_names
names(storm_isabel_2050) <- new_names

king_tide <- read_delim("app_data/KingTide_BLGP.txt")
king_tide <- king_tide %>% 
  mutate(GEOID = as.character(GEOID)) 

king_tide_2050 <- read_delim("app_data/KingTide_2050_BLGP.txt")
king_tide_2050 <- king_tide_2050 %>% 
  mutate(GEOID = as.character(GEOID)) 

## Read in/get geometries 
blkgrp_geo <- st_read("app_data/cbg-selected/esva_2020blkgrp_clipped.geojson")
blkgrp_geo <- st_transform(blkgrp_geo, 4326)

counties_geo <- counties(state = 'VA', year = 2022, cb = TRUE) # from tigris / used 2021 bc 2022 caused error
counties_geo <- counties_geo %>% subset(COUNTYFP %in% c("001"))
counties_geo <- st_transform(counties_geo, 4326)
counties_geo <- st_transform(counties_geo, 4326)
## leaflet map boundary settings
bbox <- st_bbox(counties_geo) %>% as.vector()

## Schools
schools_sf <- st_read("../data/schools_sf.geojson")

# add demographic/population/housing data
pop_est <- pop %>% 
  select(GEOID, tract_id, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est, medhhinc_est
  ) %>% 
  mutate(GEOID = as.character(GEOID),
         whiteper_est = round(whiteper_est,0),
         blackper_est = round(blackper_est,0),
         ltnxper_est = round(ltnxper_est,0),
         remainper_est = round(remainper_est,0),
         age17per_est = round(age17per_est,0),
         age18to64per_est = round(age18to64per_est,0),
         age65per_est = round(age65per_est,0))

# join data frames
data_wrangle <- function(df){
  df <- df %>% 
    left_join(blkgrp_names) %>% 
    left_join(blkgrp_geo) %>% 
    left_join(pop_est)
  df <- sf::st_as_sf(df)
  
  df <- df %>% 
    mutate(PercentInundated = round(Frac * 100, 0)) %>% 
    select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
           PeakSurge, MeanSurge, Frac, PercentInundated,
           totpop_est, whiteper_est, blackper_est, ltnxper_est, remainper_est,
           age17per_est, age18to64per_est, age65per_est, medhhinc_est,
           geometry)
  
  names(df) = c("GEOID", "tract_id", "locality", "localityfips", "tract", "blkgrp", "names", 
    "Peak Surge", "Mean Surge", "Inundation Area Fraction", "Percent of Area Inundated", 
    "Total Population", "Percent White Population", "Percent Black Population", "Percent Hispanic Population", 
    "All Others", "Population under 18 yrs", "Population 18-64 yrs", "Population over 65 yrs", 
    "Median Household Income",
    "geometry"
  )
  
  return(df)
}

storm_isabel <- data_wrangle(storm_isabel)
storm_isabel_2050 <- data_wrangle(storm_isabel_2050)
king_tide <- data_wrangle(king_tide)
king_tide_2050 <- data_wrangle(king_tide_2050)

# Heatmap data ----
heatmap_dat <- function(df){
  df <- df %>% 
    rename_with(.fn = ~ paste0("var_", .x), .cols = c("Peak Surge", "Mean Surge", "Percent of Area Inundated")) %>%
    select(GEOID, locality, names, starts_with("var_")) %>%
    pivot_longer(starts_with("var_"), names_to = "variable", values_to = "risk") %>%
    mutate(variable = factor(variable, 
                             levels = c("var_Peak Surge", "var_Mean Surge", "var_Percent of Area Inundated"),
                             labels = c("Peak Surge", "Mean Surge", "Percent of Area Inundated"))
    ) %>%
    st_drop_geometry()
  
  df <- df[with(df, order(locality,names)),]
  
  return(df)
}

storm_isabel_hm <- heatmap_dat(storm_isabel)
storm_isabel_2050_hm <- heatmap_dat(storm_isabel_2050)
king_tide_hm <- heatmap_dat(king_tide)
king_tide_2050_hm <- heatmap_dat(king_tide_2050)

# storm_blkgrp_long <- storm_blkgrp %>%
#   rename_with(.fn = ~ paste0("var_", .x), .cols = c("Peak Surge", "Mean Surge", "Percent of Area Inundated")) %>%
#   select(GEOID, COUNTYFP, names, starts_with("var_")) %>%
#   pivot_longer(starts_with("var_"), names_to = "variable", values_to = "risk") %>%
#   mutate(group = case_when(str_detect(variable, "2050") ~ "Scenario2050",
#                            .default = "Scenario1"),
#          # variable = str_remove(variable, "var_"),
#          variable = factor(variable, levels = c("var_PeakSurge_m", "var_MeanSurge_m", "var_InundationAreaFraction", "var_PeakSurge2050_m", "var_MeanSurge2050_m", "var_InundationAreaFraction2050"),
#                            labels = c("Peak Surge", "Mean Surge", "Inundation Area Fraction", "Peak Surge", "Mean Surge", "Inundation Area Fraction")
#          ),
#          locality = case_when(COUNTYFP == "001" ~ "Accomack",
#                               COUNTYFP == "131" ~ "Northampton")
#   ) %>%
#   st_drop_geometry()
# 
# storm_blkgrp_long <- storm_blkgrp_long[with(storm_blkgrp_long, order(locality,names)),]

# Willis Wharf ----

willis_wharf <- read_csv("app_data/WhillisWarf_Isabel_detailed_output.csv")
willis_wharf_2050 <- read_csv("app_data/WhillisWarf_Isabel_future_detailed_output.csv")

patch_geo <- st_read("app_data/WhillisWarf_PatchID_DEM/WhillisWarf_PatchID_DEM.shp")
patch_geo <- st_transform(patch_geo, 4326)

willis_wharf <- willis_wharf %>% left_join(patch_geo) %>% 
  st_as_sf()

willis_wharf_2050 <- willis_wharf_2050 %>% left_join(patch_geo) %>% 
  st_as_sf()

# willis_wharf_2050 %>%
#   ggplot() +
#   geom_sf(aes(fill = Max_m), color = "black")

new_names <- c("PatchID", "X_m", "Y_m", "NAVD88ELEV_m", "Max Water Levels (m)", "Inundation Time", "DEM_m","geometry")
names(willis_wharf) <- new_names
names(willis_wharf_2050) <- new_names

# Metadata ----
scenario_meta <- data.frame(scenario = c("Hurricane Isabel 2003", "Hurricane Isabel 2050 Projection", 
                                         "King Tide", "King Tide 2050 Projection"),
                            def = c("Historical Hurricane Isabel 2003 ADCIRC model run", 
                                    "Future Hurricane Isabel 2050 ADCIRC model run: model simulation by considering future sea-level rise of +0.2 meters and increased storm wind speed (*1.1)", 
                                    "Historical King Tide 2009 ADCIRC model run", 
                                    "Future King Tide 2050 ADCIRC model run: model simulation by considering future sea-level rise +0.2 meters"))

variable_meta <- data.frame(variable = c("Peak Surge", "Mean Surge", "Percent of Area Inundated"),
                            def = c("Peak flooding water depth in meters for inundated land areas (elevation >0 m mean sea level) within each census block group",
                                    "The average peak flooding water depth in meters for inundated land areas (elevation >0 m mean sea level) within each census block group", 
                                    "Percent of land area inundated (between 0-100%; 100% for completely inundated for all land areas) within each census block group"))



# Save for app ----
# create new app_data.Rdata file
save(counties_geo, blkgrp_geo, schools_sf,
     storm_isabel, storm_isabel_2050, 
     king_tide, king_tide_2050,
     storm_isabel_hm, storm_isabel_2050_hm, 
     king_tide_hm, king_tide_2050_hm,
     willis_wharf, willis_wharf_2050, patch_geo,
     scenario_meta, variable_meta,
     file = "esva-atlas-prototype/www/app_data.Rdata")
