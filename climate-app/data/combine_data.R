## ......................................
## Combine cville collection data for app
## Starting with tract data
##   (add block group, blocks, county later)
## 
## Authors: Michele Claibourn
## Created: 2022-01-31
## Updated: 2022-02-24
## ......................................

# Setup ----

library(tidyverse)
library(sf)
library(janitor)


## ......................................
# Read in data ----
# wanted to read in all tract data,
#   but same-named, different-formatted vars causing problems;
#   in later data acquisition updates, enforce common naming
#   for geoid, locality/county, tract, etc.

# tract_files <- dir(path = "data-csv", pattern = "tract.csv") 
# data <- tract_files %>%
#     map(~ read_csv(file.path("data-csv", .)))
## ......................................


acs <- read_csv("eastern_shore_collection/data/acs_eastern_tract.csv") %>% # fix this name (cville_tract)
  mutate(geoid = as.character(GEOID)) %>% 
  select(geoid, locality, tract, ends_with("E"))

life <- read_csv("eastern_shore_collection/data/lifeexp_eastern_tract.csv") %>%
  mutate(geoid = as.character(GEOID)) %>%
  select(geoid, life_expectancy)

air <- read_csv("eastern_shore_collection/data/airquality_eastern_tract.csv") %>% 
  mutate(geoid = as.character(gid)) %>% 
  select(geoid, locality = countyfp,
         tract, pm2_5_1981, pm2_5_2016, percentile_1981, percentile_2016,
         pm_change_1981_2016, pctile_change_1981_2016)

cdc <- read_csv("eastern_shore_collection/data/cdcplaces_eastern_tract.csv") %>% 
  mutate(geoid = as.character(locationname)) %>% 
  select(geoid, countyname, Coronary_Heart_Disease2018:Annual_Checkup2018)

daym <- read_csv("eastern_shore_collection/data/daymet_eastern_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  filter(year == 2020) %>% 
  select(geoid, locality = COUNTYFP,
         June_AvgMaxTF:TotpercInch)

fccbb <- read_csv("eastern_shore_collection/data/fcc_broadband_eastern_tract.csv") %>% 
  mutate(geoid = as.character(tract)) %>% 
  select(geoid, !c(tract, bbmin_dl, bbmin_up))

nri <- read_csv("eastern_shore_collection/data/fema_nri_eastern_tract.csv") %>% 
  mutate(geoid = as.character(TRACTFIPS)) %>% 
  select(geoid, locality = COUNTYFIPS, 
         BUILDVALUE:AREA, ends_with("AFREQ")) %>% 
  select(!starts_with("CFLD")) 

ls8 <- read_csv("eastern_shore_collection/data/landsat8_easternShore_tracts.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  filter(start_date == as.Date("2020-07-04")) %>% 
  select(geoid, min_temp = min, max_temp = max, mean_temp = mean,
         med_temp = median)

lead <- read_csv("eastern_shore_collection/data/lead_eastern_tract.csv") %>% 
  mutate(geoid = as.character(FIP)) %>% 
  select(geoid, locality = county_fip, 
         totalelep:avg_hh_exp, percentburdened,
         percent_burdened_owners, percent_burdened_renters)

lexp <- read_csv("eastern_shore_collection/data/leadexposure_eastern_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  select(geoid, locality = countyfips, leadriskscore_raw, lead_risk_rank)

nlcd <- read_csv("eastern_shore_collection/data/nlcd_eastern_tracts.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  select(geoid, locality = COUNTYFP, 
         tree_can:imp_surf, percent_dev:percent_for)

ejs <- read_csv("eastern_shore_collection/data/ejscreen_eastern_tract.csv") %>%
  mutate(locality = as.character(str_sub(tract, 3, 5))) %>%
  mutate(geoid = as.character(tract)) %>%
  select(DSLPM, CANCER, RESP, PTRAF, PWDIS, PNPL,
         PRMP, PTSDF, OZONE, PM25, geoid, locality)

walk <- read_csv("eastern_shore_collection/data/walk_eastern_tract.csv") %>%
  mutate(locality = as.character(str_sub(FIPS_TRACT, 3, 5))) %>%
  mutate(geoid = as.character(FIPS_TRACT)) %>%
  select(geoid, locality, avg_intersection_density:avg_walkability_index)

flood <- read_csv("eastern_shore_collection/data/nfhl_eastern_tracts.csv") %>%
  mutate(geoid = as.character(GEOID)) %>%
  select(-'...1', -GEOID)

# need to reshape the flood hazard data so that it matches the googlesheet information 
flood <- flood %>%
  pivot_wider(names_from = zone, id_cols = geoid,
              values_from = c(area, perc))

# ADD LATER 
# ejscreen (only blockgroup until aggregated, needs processing)
# walkability (only blockgroup until aggregated, needs processing), 
# nfhl (in branch, needs more processing; some tracts missing?), 
# hmda (need to choose what to aggregate to tract)
# fema_nfip/policies (need to check; need to choose what to aggregate to tract)
# location affordability (need to check)
# lodes: need to determine key vars
  # lodes_employment, lodes_rescoummutepatterns, 
  # lodes_residentcommute, lodes_workercommute, 
# food_retail (maybe as point?) or aggregate/count in tract?
# noaa for county


## ......................................
# Merge tract data ----
df <- acs %>% 
  left_join(cdc) %>% 
  left_join(life) %>%
  left_join(air) %>% 
  left_join(lexp) %>% 
  left_join(daym) %>% 
  left_join(fccbb) %>% 
  left_join(lead) %>% 
  left_join(nlcd) %>% 
  left_join(ls8) %>% 
  left_join(nri) %>% 
  left_join(walk) %>%
  left_join(ejs) %>%
  left_join(flood) %>%
  select(-state)

df <- df %>% 
  mutate(pop = totalpopE)


## ......................................
# Add tract metadata ----

# Read in googlesheets info
#   su_, group (only for selectable), name, source, description
#   consider adding url for exploration/documentation files?

googlesheets4::gs4_deauth()
url_sheet <- "https://docs.google.com/spreadsheets/d/1nqm3DuVXD1ObbVe_deacvT7uSLdBXfQJo3mkbqDwrVo/edit?usp=sharing"

pretty_acs <- googlesheets4::read_sheet(url_sheet, sheet = "acs")
pretty_lif <- googlesheets4::read_sheet(url_sheet, sheet = "life")
pretty_cdc <- googlesheets4::read_sheet(url_sheet, sheet = "cdc_places")
pretty_air <- googlesheets4::read_sheet(url_sheet, sheet = "airquality")
pretty_lex <- googlesheets4::read_sheet(url_sheet, sheet = "lead_exposure")
pretty_dym <- googlesheets4::read_sheet(url_sheet, sheet = "DAYMET")
pretty_fcc <- googlesheets4::read_sheet(url_sheet, sheet = "fcc_broadband")
pretty_leb <- googlesheets4::read_sheet(url_sheet, sheet = "lead")
pretty_nlc <- googlesheets4::read_sheet(url_sheet, sheet = "nlcd")
pretty_ls8 <- googlesheets4::read_sheet(url_sheet, sheet = "landsat8")
pretty_nri <- googlesheets4::read_sheet(url_sheet, sheet = "fema_nri")
pretty_wlk <- googlesheets4::read_sheet(url_sheet, sheet = "walkability")
pretty_ejs <- googlesheets4::read_sheet(url_sheet, sheet = "ejscreen")
pretty_nfh <- googlesheets4::read_sheet(url_sheet, sheet = "nfhl")

pretty <- bind_rows(pretty_acs, pretty_cdc, pretty_air,
                    pretty_lex, pretty_dym, pretty_fcc,
                    pretty_leb, pretty_nlc, pretty_ls8,
                    pretty_nri, pretty_wlk, pretty_ejs,
                    pretty_lif, pretty_nfh)

# Adapt Clay's code to assign attributes
# add pretty metadata to as var attribute: name, source, description
j <- match(pretty$varname, names(df))
# remove vars from pretty that don't yet exist in df
pretty_j <- pretty[(pretty$varname %in% names(df)),]
j <- match(pretty_j$varname, names(df))

for(i in seq_along(j)){
  attr(df[[j[i]]], which = "goodname") <- pretty_j$name[i]
  attr(df[[j[i]]], which = "source") <- pretty_j$source[i]
  attr(df[[j[i]]], which = "description") <- pretty_j$description[i]
  attr(df[[j[i]]], which = "about") <- pretty_j$notes[i]
}
 
# create data frame of group and varnames
group_df <- pretty_j %>%
  select(varname, group, name) %>%
  filter(!is.na(group))

ind_ct <- df %>%
  #filter(GEO_LEVEL == "Census Tract") %>%
  select(group_df$varname) %>%
  map_lgl(~ !all(is.na(.x)))

# add indicator logicals to group_df and sort
# column bg - TRUE if variable available for Block Group
# column ct - TRUE if variable available for Census Tract
# all vars available for County
group_df_dem <- cbind(group_df, #bg = ind_bg[-length(ind_bg)],
                  ct = ind_ct) %>% 
  mutate(group = factor(group, levels = c("Demographic & Social", 
                                          "Health", 
                                          "Youth & Educaton", 
                                          "Jobs & Income", 
                                          "Housing & Transportation",
                                          "Risk Factors",
                                          "Community Assets & Infrastructure", 
                                          "Climate Measures")
                        )) %>% 
                  #ct = ind_ct[-length(ind_ct)]) %>%
  arrange(group, name)

ind_demfirst_ct <- split(group_df_dem, group_df_dem$group) %>%
  map(function(x)filter(x, ct)) %>%
  map(function(x)pull(x, varname, name))

group_df_clim <- cbind(group_df, #bg = ind_bg[-length(ind_bg)],
                      ct = ind_ct) %>% 
  mutate(group = factor(group, levels = c("Climate Measures", 
                                          "Community Assets & Infrastructure", 
                                          "Risk Factors", 
                                          "Housing & Transportation",
                                          "Jobs & Income",
                                          "Youth & Educaton", 
                                          "Health", 
                                          "Demographic & Social")
                        )) %>% 
  #ct = ind_ct[-length(ind_ct)]) %>%
  arrange(group, name)

ind_climfirst_ct <- split(group_df_clim, group_df_clim$group) %>%
  map(function(x)filter(x, ct)) %>%
  map(function(x)pull(x, varname, name))


## ......................................
# Join to geography ----
tract_shape <- readRDS("eastern_shore_collection/data/eastshore_tracts.RDS") %>% 
  clean_names() %>% 
  select(geoid, locality = countyfp, name,
         geometry)

geo <- tract_shape %>% 
  left_join(df) 

geo <- st_transform(geo, crs = 4326)


## ......................................
# Save Rdata ----
saveRDS(df, file = "climate-app/data/eastern_data.RDS")
saveRDS(geo, file = "climate-app/data/eastern_geo.RDS")
save(df, geo, group_df_dem, group_df_clim, 
     ind_demfirst_ct, ind_climfirst_ct, 
     file = "climate-app/data/eastern_dat.RData")


# Add indices at tract (or county) level?
# AHDI, dissimilarty, interaction, isolation?