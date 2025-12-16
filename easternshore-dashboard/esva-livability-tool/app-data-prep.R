# Libraries
library(here)
library(shiny)
library(tidyverse)
library(boxr)
library(jsonlite) 
library(sf)
library(leaflet)
library(RColorBrewer)
library(rcartocolor)
library(bslib)
library(qs)
library(rmapshaper)
library(readxl)

# Set WD
setwd(here("esva-livability-tool"))

# Box authentication
readRenviron("~/.Renviron")
box_auth(client_id = Sys.getenv('BOX_CLIENT_ID'), client_secret = Sys.getenv('BOX_CLIENT_SECRET'))

# Get Box Data
dir_id <- "305971996036" # EA_Export
box_setwd(dir_id)

# box_ls()

box_fetch(
  dir_id = box_getwd(),
  local_dir = "app_data/EA_Export",
  recursive = TRUE,
  overwrite = TRUE,
  delete = TRUE
)

# Data prep ----

# Get Box ID function
get_boxid <- function(path) {
  boxid <- str_split(path, "https://virginia.app.box.com/file/", simplify = TRUE)[2]
  boxid
}


block_geo <- st_read("app_data/blocks/esva_2020block_clipped.geojson")
block_geo <- st_transform(block_geo, 4326) %>% 
  select(GEOID20)

block_geo <- ms_simplify(block_geo) %>% 
  sf::st_collection_extract()

# Population data ---
pop_dat <- box_read_csv("2026502973438") %>% 
  separate_wider_delim(NAME, ", ", names = c(NA, NA, NA, "locality", NA)) %>% 
  mutate(GEOID20 = as.character(GEOID)) %>% 
  select(GEOID20, locality, total, hisp, black, white, pop_under18, total_housing, occupied_housing, jobs_wac, jobs_wac_low, jobs_rac, jobs_rac_low)

# block_housing_data<- box_read_csv("2026502973438") %>%
#   mutate(GEOID20 = as.character(GEOID),
#          housing_units = total_housing) %>%
#   select(GEOID20, housing_units)
# write_csv(block_housing_data, "json_edits/block_housing_data.csv")

## Total population baselines ----
pop_total <- pop_dat %>% 
  summarize(across(c(total:jobs_rac_low), sum)) %>% 
  mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
         per_occupied_housing = (occupied_housing/total_housing)*100,
         across(c(jobs_rac_low), ~ (.x/jobs_rac)*100, .names = "per_{.col}"),
         across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}"),
         bin = "ESVA Total", event = "none")

pop_total_county <- pop_dat %>% 
  group_by(locality) %>% 
  summarize(across(c(total:jobs_rac_low), sum)) %>% 
  mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
         per_occupied_housing = (occupied_housing/total_housing)*100,
         across(c(jobs_rac_low), ~ (.x/jobs_rac)*100, .names = "per_{.col}"),
         across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}"),
         bin = "ESVA Total", event = "none")

pop_total_acc <- pop_total_county %>% filter(locality == "Accomack County")
pop_total_north <- pop_total_county %>% filter(locality == "Northampton County")

# Data prep function ----
ea_prep_func <- function(eafile) {
  
  # Read in ea_export
  ea_export <- fromJSON(paste0("app_data/EA_Export/", eafile))
  
  # Get CSV-JSON
  json_path <- ea_export$data$path
  json_id <- get_boxid(json_path)
  json <- box_read_json(file_id = json_id)
  
  # Get CSV
  csv_id <- get_boxid(json$path)
  csv <- box_read_csv(csv_id)
  # csv <- box_read_csv("2045429539192")
  
  # Join geometry
  csv_geo <- csv %>% 
    mutate(GEOID20 = as.character(GEOID20),
           locality = case_when(substr(GEOID20, 1,5) == "51001" ~ "Accomack County",
                                substr(GEOID20, 1,5) == "51131" ~ "Northampton County")) %>%
    left_join(block_geo, by = join_by(GEOID20 == GEOID20)) %>%
    st_as_sf()
  
  # Gather data for app
  region_bbox <- ea_export$regionBoundingBox
  bbox <- c(region_bbox$lonMin, region_bbox$latMin, region_bbox$lonMax, region_bbox$latMax)
  
  year <- ea_export$data$key
  descriptionTitle <- ea_export$descriptionTitle
  event <- ea_export$event
  description <- ea_export$descriptionText
  dataCategories <- ea_export$dataCategories
  
 measures <- list() 
  
  for (i in seq_along(ea_export$dataColumns$name)) {
    name <- ea_export$dataColumns$name[i]
    name_index <- match(name, json$schema$fields$name)
    title <- json$schema$fields$title[name_index]
    field_description <- json$schema$fields$description[name_index]
    # field_description <- ea_export$dataColumns$`$description`[[i]]

    # print(names(json$schema$fields))
    unit <- if("unit" %in% names(json$schema$fields)){
      n <- json$schema$fields$unit[name_index]
      if(n == "none" | n == "-"){NULL} else {n}
    } else {NULL}
    # print(unit)
    
    var <- quo(name)
    map_data <- csv_geo %>% select(GEOID20, locality, UQ(var), geometry)
    
    # print(var)
    legend_breaks <- as.vector(ea_export$dataColumns$bins[[i]])
    legend_labels <- as.list(ea_export$dataColumns$labels[[i]])
    
    sel_range <- c(min(legend_breaks), max(legend_breaks))
    
    col_pal <- if(str_detect(event, "groundwater")){
      rev(brewer.pal(length(legend_labels), "YlGnBu"))
    } else if (str_detect(event, "Extreme")){
      carto_pal(length(legend_labels), "Earth")
    ## Uncomment for Roadway Flooding ----
    # } else if (str_detect(dataCategories, "inlandflooding")) {
    #   c('#fff5f0', '#ffd6c6', '#ffb59c', '#ff9172', '#fb6a4a', '#e34e37', '#c83528', '#a91e1d', '#890b14', '#67000d') #Reds
    ## Uncomment for Housing ----
    # } else if (str_detect(dataCategories, "housing")){
    #   brewer.pal(length(legend_labels), "Purples")
    } else {brewer.pal(length(legend_labels), "GnBu")}
    
    
    csv_pop <- map_data %>% 
      left_join(pop_dat, by = join_by(GEOID20 == GEOID20, locality == locality)) %>% 
      st_drop_geometry()
    
    # View(csv_pop)
    
    # binning function
    bin_function <- function(name) {
      var_name_bin = cut(name,
                         breaks = legend_breaks,
                         labels = legend_labels,
                         include.lowest = TRUE)
    }
    
    # create bin_ variable for each variable identified in ea json
    csv_pop <- csv_pop %>% 
      mutate(
        across(name, bin_function,
                    .names = "bin_{.col}"),
             event = event)
    
    group_vars <- syms(csv_pop %>% select(starts_with("bin")) %>% names())
    
    pop_data <- csv_pop %>% 
      select(-locality) %>% 
      group_by(!!group_vars[[1]], event) %>%
      summarize(across(c(total:jobs_rac_low), sum)) %>% 
      mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
             per_occupied_housing = (occupied_housing/total_housing)*100,
             across(c(jobs_rac_low), ~ (.x/jobs_rac)*100, .names = "per_{.col}"), 
             across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}")) %>% 
      ungroup() %>% 
      mutate(per_total = (total/sum(total))*100,
             per_housing = (total_housing/sum(total_housing))*100) %>% 
      rename(bin = group_vars[[1]]) %>%
      bind_rows(pop_total) %>% 
      mutate(bin = factor(bin, levels = c(legend_labels, "ESVA Total"))) 
      # select(bin, event, starts_with("per_"))
    
    pop_data_acc <- csv_pop %>% 
      filter(locality == "Accomack County") %>%
      group_by(!!group_vars[[1]], event) %>%
      summarize(across(c(total:jobs_rac_low), sum)) %>% 
      mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
             per_occupied_housing = (occupied_housing/total_housing)*100,
             across(c(jobs_rac_low), ~ (.x/jobs_rac)*100, .names = "per_{.col}"), 
             across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}")) %>% 
      ungroup() %>% 
      mutate(per_total = (total/sum(total))*100,
             per_housing = (total_housing/sum(total_housing))*100) %>% 
      rename(bin = group_vars[[1]]) %>%
      bind_rows(pop_total_acc) %>% 
      mutate(bin = factor(bin, levels = c(legend_labels, "ESVA Total")))
    
    pop_data_north <- csv_pop %>% 
      filter(locality == "Northampton County") %>%
      group_by(!!group_vars[[1]], event) %>%
      summarize(across(c(total:jobs_rac_low), sum)) %>% 
      mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
             per_occupied_housing = (occupied_housing/total_housing)*100,
             across(c(jobs_rac_low), ~ (.x/jobs_rac)*100, .names = "per_{.col}"), 
             across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}")) %>% 
      ungroup() %>% 
      mutate(per_total = (total/sum(total))*100,
             per_housing = (total_housing/sum(total_housing))*100) %>% 
      rename(bin = group_vars[[1]]) %>%
      bind_rows(pop_total_north) %>% 
      mutate(bin = factor(bin, levels = c(legend_labels, "ESVA Total")))

    # add total housing to map_data
    map_data <- map_data %>% 
      left_join(pop_dat, by = join_by(GEOID20 == GEOID20, locality == locality)) %>% 
      select(GEOID20, locality, UQ(var), total_housing, geometry)
    
    ls_name <- as.character(title)
    ls_name
    ls <- list(name=name, map_data=map_data, title=title, field_description=field_description, unit=unit,
               legend_breaks=legend_breaks, legend_labels=legend_labels, sel_range=sel_range, 
               col_pal=col_pal, pop_data=pop_data, pop_data_acc=pop_data_acc, pop_data_north=pop_data_north)
    # print(ls)
    # measures <- list(title=ls)
    measures <- append(measures, setNames(list(ls), as.character(title)))
    # measures <- append(measures, list(title=ls))
# print(measures)
  }
  
# print(measures)
  
  file_prep <- list(year=year, event=event, description=description, 
                    descriptionTitle=descriptionTitle, bbox=bbox,
                    measures=measures)
  
  file_prep
  
}

# Groundwater w/o sewers ----
groundwater_rm_sewer_2024 <- ea_prep_func(list.files("app_data/EA_Export")[1])
groundwater_rm_sewer_2030 <- ea_prep_func(list.files("app_data/EA_Export")[3])
groundwater_rm_sewer_2040 <- ea_prep_func(list.files("app_data/EA_Export")[5])
groundwater_rm_sewer_2050 <- ea_prep_func(list.files("app_data/EA_Export")[7])
groundwater_rm_sewer_2060 <- ea_prep_func(list.files("app_data/EA_Export")[9])
groundwater_rm_sewer_2080 <- ea_prep_func(list.files("app_data/EA_Export")[11])

groundwater_rm_sewer <- list("2024"=groundwater_rm_sewer_2024, "2030"=groundwater_rm_sewer_2030, 
                             "2040"=groundwater_rm_sewer_2040, "2050"=groundwater_rm_sewer_2050, 
                             "2060"=groundwater_rm_sewer_2060, "2080"=groundwater_rm_sewer_2080)

save(groundwater_rm_sewer, file = "saved_rdata/groundwater_rm_sewer.Rda")

# Groundwater ----
groundwater_2024 <- ea_prep_func(list.files("app_data/EA_Export")[2])
groundwater_2030 <- ea_prep_func(list.files("app_data/EA_Export")[4])
groundwater_2040 <- ea_prep_func(list.files("app_data/EA_Export")[6])
groundwater_2050 <- ea_prep_func(list.files("app_data/EA_Export")[8])
groundwater_2060 <- ea_prep_func(list.files("app_data/EA_Export")[10])
groundwater_2080 <- ea_prep_func(list.files("app_data/EA_Export")[12])

groundwater <- list("2024"=groundwater_2024, "2030"=groundwater_2030, 
                    "2040"=groundwater_2040, "2050"=groundwater_2050, 
                    "2060"=groundwater_2060, "2080"=groundwater_2080)

save(groundwater, file = "saved_rdata/groundwater.Rda")

# Storm Surge: Hurricane Dorian ----
dr_2019 <- ea_prep_func(list.files("app_data/EA_Export")[13])
dorian <- list("2019"=dr_2019)

save(dorian, file = "saved_rdata/dorian.Rda")

# Storm Surge Isabel ----
ib_2003 <- ea_prep_func(list.files("app_data/EA_Export")[14])
ib_2025 <- ea_prep_func(list.files("app_data/EA_Export")[15])
ib_2030 <- ea_prep_func(list.files("app_data/EA_Export")[16])
ib_2040 <- ea_prep_func(list.files("app_data/EA_Export")[17])
ib_2050 <- ea_prep_func(list.files("app_data/EA_Export")[18])
ib_2060 <- ea_prep_func(list.files("app_data/EA_Export")[19])
ib_2080 <- ea_prep_func(list.files("app_data/EA_Export")[20])

isabel <- list("2003"=ib_2003, "2025" = ib_2025, "2030"=ib_2030, 
               "2040"=ib_2040, "2050"=ib_2050, 
               "2060"=ib_2060, "2080"=ib_2080)

save(isabel, file = "saved_rdata/isabel.Rda")

# Storm Surge Hurricane Joaquin ----
jq_2015 <- ea_prep_func(list.files("app_data/EA_Export")[21])
joaquin <- list("2015"=jq_2015)

save(joaquin, file = "saved_rdata/joaquin.Rda")

# Storm Surge King Tide ----
kt_2009 <- ea_prep_func(list.files("app_data/EA_Export")[22])
kingtide <- list("2009"=kt_2009)

save(kingtide, file = "saved_rdata/kingtide.Rda")

# Storm Surge Nor'Ida Storm ----
ni_2009 <- ea_prep_func(list.files("app_data/EA_Export")[23])
norida <- list("2009"=ni_2009)

save(norida, file = "saved_rdata/norida.Rda")

# Storm Surge Sandy ----
sd_2012 <- ea_prep_func(list.files("app_data/EA_Export")[24])
sandy <- list("2012"=sd_2012)

save(sandy, file = "saved_rdata/sandy.Rda")

# Composite Storm Surge Risk ----
composite_ss <- ea_prep_func(list.files("app_data/EA_Export")[25])
composite_risk <- list("Composite"=composite_ss)

save(composite_risk, file = "saved_rdata/composite_risk.Rda")

# Extreme Wetness/Dryness ----
ewd_2025 <- ea_prep_func(list.files("app_data/EA_Export")[26])
ewd_2030 <- ea_prep_func(list.files("app_data/EA_Export")[27])
ewd_2040 <- ea_prep_func(list.files("app_data/EA_Export")[28])
ewd_2050 <- ea_prep_func(list.files("app_data/EA_Export")[29])
ewd_2060 <- ea_prep_func(list.files("app_data/EA_Export")[30])
ewd_2080 <- ea_prep_func(list.files("app_data/EA_Export")[31])

extremes <- list("2025" = ewd_2025, "2030"=ewd_2030, 
                 "2040"=ewd_2040, "2050"=ewd_2050, 
                 "2060"=ewd_2060, "2080"=ewd_2080)

save(extremes, file = "saved_rdata/extremes.Rda")

# Roadway flooding ----
rdflood_2020 <- ea_prep_func(list.files("app_data/EA_Export")[32])
rdflood_2040 <- ea_prep_func(list.files("app_data/EA_Export")[33])
rdflood_2060 <- ea_prep_func(list.files("app_data/EA_Export")[34])
rdflood_2080 <- ea_prep_func(list.files("app_data/EA_Export")[35])
landuse_2025 <- ea_prep_func(list.files("app_data/EA_Export")[36])

roadflood <- list("2020"=rdflood_2020, "2040"=rdflood_2040, "2060"=rdflood_2060, "2080"=rdflood_2080, 
                  "Current Land Cover"=landuse_2025)

save(roadflood, file = "saved_rdata/roadflood.Rda")

# # Land Use/Land Cover - added above to inland flooding
# landuse_2025 <- ea_prep_func(list.files("app_data/EA_Export")[29])
# landuse <- list("2025"=landuse_2025)
# 
# save(landuse, file = "saved_rdata/landuse.Rda")

# Water Level Depth ----
wld_2020 <- ea_prep_func(list.files("app_data/EA_Export")[37])
avg_wld <- list("2020-2023"=wld_2020)
save(avg_wld, file = "saved_rdata/avg_wld.Rda")

# Septic System Risk Assessment ----
ssra_2020 <- ea_prep_func(list.files("app_data/EA_Export")[38])
septic <- list("2020-2023"=ssra_2020)
save(septic, file = "saved_rdata/septic.Rda")

# Seawater intrusion w/out sewers ----
swi_rm_sewer_2024 <- ea_prep_func(list.files("app_data/EA_Export")[39])
swi_rm_sewer_2030 <- ea_prep_func(list.files("app_data/EA_Export")[41])
swi_rm_sewer_2040 <- ea_prep_func(list.files("app_data/EA_Export")[43])
swi_rm_sewer_2050 <- ea_prep_func(list.files("app_data/EA_Export")[45])
swi_rm_sewer_2060 <- ea_prep_func(list.files("app_data/EA_Export")[47])
swi_rm_sewer_2080 <- ea_prep_func(list.files("app_data/EA_Export")[49])

swi_rm_sewer <- list("2024" = swi_rm_sewer_2024, "2030"=swi_rm_sewer_2030, 
            "2040"=swi_rm_sewer_2040, "2050"=swi_rm_sewer_2050, 
            "2060"=swi_rm_sewer_2060, "2080"=swi_rm_sewer_2080)

save(swi_rm_sewer, file = "saved_rdata/swi_rm_sewer.Rda")

# Seawater intrusion ----
swi_2024 <- ea_prep_func(list.files("app_data/EA_Export")[40])
swi_2030 <- ea_prep_func(list.files("app_data/EA_Export")[42])
swi_2040 <- ea_prep_func(list.files("app_data/EA_Export")[44])
swi_2050 <- ea_prep_func(list.files("app_data/EA_Export")[46])
swi_2060 <- ea_prep_func(list.files("app_data/EA_Export")[48])
swi_2080 <- ea_prep_func(list.files("app_data/EA_Export")[50])

swi <- list("2024" = swi_2024, "2030"=swi_2030, 
                    "2040"=swi_2040, "2050"=swi_2050, 
                    "2060"=swi_2060, "2080"=swi_2080)

save(swi, file = "saved_rdata/swi.Rda")

# Housing ----
housing_2020 <- ea_prep_func(list.files("app_data/EA_Export")[51])

housing <- list("U.S. Census, 2020" = housing_2020)

save(housing, file = "saved_rdata/housing.Rda")


# Compile App data ----
# Load previous data
# load("saved_rdata/groundwater_rm_sewer.Rda")
# load("saved_rdata/groundwater.Rda")
# load("saved_rdata/avg_wld.Rda")
# load("saved_rdata/composite_risk.Rda")
# load("saved_rdata/dorian.Rda")
# load("saved_rdata/extremes.Rda")
# load("saved_rdata/isabel.Rda")
# load("saved_rdata/joaquin.Rda")
# load("saved_rdata/kingtide.Rda")
# load("saved_rdata/norida.Rda")
# load("saved_rdata/sandy.Rda")
# load("saved_rdata/roadflood.Rda")
# load("saved_rdata/septic.Rda")
# load("saved_rdata/swi_rm_sewer.Rda")
# load("saved_rdata/swi.Rda")
# load("saved_rdata/housing.RDA")


app_dat <- list(`Depth to Groundwater (Areas Without Sewer Access)`= groundwater_rm_sewer, 
                `Depth to Groundwater`=groundwater, 
                `Extreme Wetness/Dryness`=extremes, 
                `Inland Flooding`=roadflood,
                `Seawater Intrusion (Areas Without Public Water Utilities)`=swi_rm_sewer,
                `Seawater Intrusion`=swi,
                `Storm Surge` = list(
                  `Composite Storm Surge Risk`=composite_risk,
                  `Hurricane Dorian`=dorian, `Hurricane Isabel`=isabel,
                  `Hurricane Joaquin`=joaquin, `Hurricane Sandy`=sandy, 
                  `King Tide`=kingtide, `Nor'Ida Storm`=norida),
                `Case Study Areas: Average Water Level Depth`=avg_wld,
                `Case Study Areas: Septic System Risk Assessment`=septic,
                `Housing`=housing
                )

  
  
qs::qsave(app_dat, "esva_app_data_12_2025.qs")


# Population data ----

## Block Group names
blkgrp_names <- read_excel("app_data/tract_names.xlsx", sheet = "blkgrp2020")
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

# Block group population
pop <- read_csv("app_data/population_blkgrp.csv")
pop <- pop %>% 
  mutate(tract_id = as.character(GEOID),
         GEOID = as.character(GEOID)) 

pop_est <- pop %>% 
  select(GEOID, tract_id, totpop_est,
         whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est, medhhinc_est
  ) %>% 
  mutate(GEOID = as.character(GEOID),
         totpop_est = round(totpop_est,0),
         whiteper_est = round(whiteper_est,0),
         blackper_est = round(blackper_est,0),
         ltnxper_est = round(ltnxper_est,0),
         remainper_est = round(remainper_est,0),
         age17per_est = round(age17per_est,0),
         age18to64per_est = round(age18to64per_est,0),
         age65per_est = round(age65per_est,0))

## Read in/get geometries 
blkgrp_geo <- st_read("app_data/esva_2020blkgrp_clipped.geojson")
blkgrp_geo <- st_transform(blkgrp_geo, 4326)
blkgrp_geo <- ms_simplify(blkgrp_geo) %>% 
  sf::st_collection_extract()

blkgrop_pop <- pop_est %>% 
  left_join(blkgrp_names) %>% 
  left_join(blkgrp_geo)

blkgrop_pop <- blkgrop_pop %>% 
  select(GEOID, tract_id, locality, localityfips, tract, blkgrp, names, 
         totpop_est, whiteper_est, blackper_est, ltnxper_est, remainper_est,
         age17per_est, age18to64per_est, age65per_est, medhhinc_est,
         geometry)

names(blkgrop_pop) = c("GEOID", "tract_id", "locality", "localityfips", "tract", "blkgrp", "names", 
              "Estimated Population", "Percent White Population", "Percent Black Population", "Percent Hispanic Population", 
              "All Others", "Population under 18 yrs", "Population 18-64 yrs", "Population over 65 yrs", 
              "Median Household Income",
              "geometry"
)

qs::qsave(blkgrop_pop, "blkgrop_pop.qs")
