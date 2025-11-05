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
  local_dir = "app_data/app_data_updated",
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


block_geo <- st_read("data/blocks/esva_2020block_clipped.geojson")
block_geo <- st_transform(block_geo, 4326) %>% 
  select(GEOID20)

block_geo <- ms_simplify(block_geo) %>% 
  sf::st_collection_extract()

# Population data ---
pop_dat <- box_read_csv("2026502973438") %>% 
  mutate(GEOID20 = as.character(GEOID)) %>% 
  select(GEOID20, total, hisp, black, white, pop_under18, total_housing, occupied_housing, jobs_wac, jobs_wac_low, jobs_rac, jobs_rac_low)

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


# Data prep function ----
ea_prep_func <- function(eafile) {
  
  # Read in ea_export
  ea_export <- fromJSON(paste0("app_data/app_data_updated/", eafile))
  
  # Get CSV-JSON
  json_path <- ea_export$data$path
  json_id <- get_boxid(json_path)
  json <- box_read_json(file_id = json_id)
  
  # Get CSV
  csv_id <- get_boxid(json$path)
  csv <- box_read_csv(csv_id)
  
  # Join geometry
  csv_geo <- csv %>% 
    mutate(GEOID20 = as.character(GEOID20)) %>%
    left_join(block_geo, by = join_by(GEOID20 == GEOID20)) %>%
    st_as_sf()
  
  # Join with population data
  # csv_pop <- csv %>% 
  #   mutate(GEOID20 = as.character(GEOID20)) %>% 
  #   left_join(pop_dat, by = join_by(GEOID20 == GEOID20))
  
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
    map_data <- csv_geo %>% select(GEOID20, UQ(var), geometry)
    # print(var)
    legend_breaks <- as.vector(ea_export$dataColumns$bins[[i]])
    legend_labels <- as.list(ea_export$dataColumns$labels[[i]])
    
    sel_range <- c(min(legend_breaks), max(legend_breaks))
    
    col_pal <- if(str_detect(event, "groundwater")){
      rev(brewer.pal(length(legend_labels), "YlGnBu"))
    } else if (str_detect(event, "Extreme")){
      carto_pal(length(legend_labels), "Earth")
    } else if (str_detect(dataCategories, "inlandflooding")) {
      # c('#fff7ec', '#ffdbb7', '#ffbd8c', '#fe9c68', '#f57d4d', '#e4613a', '#cf4729', '#b72e1a', '#9c170d', '#7f0000') #OrRd
      c('#fff5f0', '#ffd6c6', '#ffb59c', '#ff9172', '#fb6a4a', '#e34e37', '#c83528', '#a91e1d', '#890b14', '#67000d') #Reds
      # carto_pal(length(legend_labels), "SunsetDark")
      # c('#efedf5', '#d8d4e8', '#c3bada', '#afa1cd', '#9c88c0', '#896fb2', '#7757a5', '#653e98', '#53248a', '#3f007d') #Purples
    # } else if (str_detect(dataCategories, "housing")){
    #   brewer.pal(length(legend_labels), "Purples")
    } else {brewer.pal(length(legend_labels), "GnBu")}
    
    
    csv_pop <- map_data %>% 
      left_join(pop_dat, by = join_by(GEOID20 == GEOID20)) %>% 
      st_drop_geometry()
    
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

    # add total housing to map_data
    map_data <- map_data %>% 
      left_join(pop_dat, by = join_by(GEOID20 == GEOID20)) %>% 
      select(GEOID20, UQ(var), total_housing, geometry)
    
    ls_name <- as.character(title)
    ls_name
    ls <- list(name=name, map_data=map_data, title=title, field_description=field_description, unit=unit,
               legend_breaks=legend_breaks, legend_labels=legend_labels, sel_range=sel_range, 
               col_pal=col_pal, pop_data=pop_data)
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

# Groundwater
groundwater_2024 <- ea_prep_func(list.files("app_data/app_data_updated")[1])
groundwater_2030 <- ea_prep_func(list.files("app_data/app_data_updated")[2])
groundwater_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[3])
groundwater_2050 <- ea_prep_func(list.files("app_data/app_data_updated")[4])
groundwater_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[5])
groundwater_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[6])

groundwater <- list("2024"=groundwater_2024, "2030"=groundwater_2030, 
                    "2040"=groundwater_2040, "2050"=groundwater_2050, 
                    "2060"=groundwater_2060, "2080"=groundwater_2080)

save(groundwater, file = "saved_rdata/groundwater.Rda")

# Storm Surge: Hurricane Dorian
dr_2019 <- ea_prep_func(list.files("app_data/app_data_updated")[7])
dorian <- list("2019"=dr_2019)

save(dorian, file = "saved_rdata/dorian.Rda")

# Storm Surge Isabel
ib_2003 <- ea_prep_func(list.files("app_data/app_data_updated")[8])
ib_2025 <- ea_prep_func(list.files("app_data/app_data_updated")[9])
ib_2030 <- ea_prep_func(list.files("app_data/app_data_updated")[10])
ib_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[11])
ib_2050 <- ea_prep_func(list.files("app_data/app_data_updated")[12])
ib_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[13])
ib_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[14])

isabel <- list("2003"=ib_2003, "2025" = ib_2025, "2030"=ib_2030, 
               "2040"=ib_2040, "2050"=ib_2050, 
               "2060"=ib_2060, "2080"=ib_2080)

save(isabel, file = "saved_rdata/isabel.Rda")

# Storm Surge Hurricane Joaquin
jq_2015 <- ea_prep_func(list.files("app_data/app_data_updated")[15])
joaquin <- list("2015"=jq_2015)

save(joaquin, file = "saved_rdata/joaquin.Rda")

# Storm Surge King Tide
kt_2009 <- ea_prep_func(list.files("app_data/app_data_updated")[16])
kingtide <- list("2009"=kt_2009)

save(kingtide, file = "saved_rdata/kingtide.Rda")

# Storm Surge Nor'Ida Storm
ni_2009 <- ea_prep_func(list.files("app_data/app_data_updated")[17])
norida <- list("2009"=ni_2009)

save(norida, file = "saved_rdata/norida.Rda")

# Composite Storm Surge Risk
composite_ss <- ea_prep_func(list.files("app_data/app_data_updated")[18])
composite_risk <- list("Composite"=composite_ss)

save(composite_risk, file = "saved_rdata/composite_risk.Rda")

# Extreme Wetness/Dryness
ewd_2025 <- ea_prep_func(list.files("app_data/app_data_updated")[19])
ewd_2030 <- ea_prep_func(list.files("app_data/app_data_updated")[20])
ewd_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[21])
ewd_2050 <- ea_prep_func(list.files("app_data/app_data_updated")[22])
ewd_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[23])
ewd_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[24])

extremes <- list("2025" = ewd_2025, "2030"=ewd_2030, 
                 "2040"=ewd_2040, "2050"=ewd_2050, 
                 "2060"=ewd_2060, "2080"=ewd_2080)

save(extremes, file = "saved_rdata/extremes.Rda")

# Inland flooding
rdflood_2020 <- ea_prep_func(list.files("app_data/app_data_updated")[25])
rdflood_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[26])
rdflood_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[27])
rdflood_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[28])
landuse_2025 <- ea_prep_func(list.files("app_data/app_data_updated")[29])

roadflood <- list("2020"=rdflood_2020, "2040"=rdflood_2040, "2060"=rdflood_2060, "2080"=rdflood_2080, 
                  "Current Land Cover"=landuse_2025)

save(roadflood, file = "saved_rdata/roadflood.Rda")

# # Land Use/Land Cover - added above to inland flooding
# landuse_2025 <- ea_prep_func(list.files("app_data/app_data_updated")[29])
# landuse <- list("2025"=landuse_2025)
# 
# save(landuse, file = "saved_rdata/landuse.Rda")

# Water Level Depth
wld_2020 <- ea_prep_func(list.files("app_data/app_data_updated")[30])
avg_wld <- list("2020-2023"=wld_2020)
save(avg_wld, file = "saved_rdata/avg_wld.Rda")

# Septic System Risk Assessment
ssra_2020 <- ea_prep_func(list.files("app_data/app_data_updated")[31])
septic <- list("2020-2023"=ssra_2020)
save(septic, file = "saved_rdata/septic.Rda")


# Seawater intrusion
swi_2024 <- ea_prep_func(list.files("app_data/app_data_updated")[32])
swi_2030 <- ea_prep_func(list.files("app_data/app_data_updated")[33])
swi_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[34])
swi_2050 <- ea_prep_func(list.files("app_data/app_data_updated")[35])
swi_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[36])
swi_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[37])

swi <- list("2024" = swi_2024, "2030"=swi_2030, 
                    "2040"=swi_2040, "2050"=swi_2050, 
                    "2060"=swi_2060, "2080"=swi_2080)

save(swi, file = "saved_rdata/swi.Rda")

# Housing
housing_2020 <- ea_prep_func(list.files("app_data/app_data_updated")[38])

housing <- list("U.S. Census, 2020" = housing_2020)

save(housing, file = "saved_rdata/housing.Rda")


# Compile App data ----
# Load previous data
# load("saved_rdata/groundwater.Rda")
# load("saved_rdata/avg_wld.Rda")
# load("saved_rdata/composite_risk.Rda")
# load("saved_rdata/dorian.Rda")
# load("saved_rdata/extremes.Rda")
# load("saved_rdata/isabel.Rda")
# load("saved_rdata/joaquin.Rda")
# load("saved_rdata/kingtide.Rda")
# # load("saved_rdata/landuse.Rda")
# load("saved_rdata/norida.Rda")
# load("saved_rdata/roadflood.Rda")
# load("saved_rdata/septic.Rda")
# load("saved_rdata/swi.Rda")
# load("saved_rdata/housing.RDA")


app_dat <- list(`Depth to Groundwater`=groundwater, `Extreme Wetness/Dryness`=extremes, 
                `Inland Flooding`=roadflood,
                `Seawater Intrusion`=swi,
                `Composite Storm Surge Risk`=composite_risk,
                `Storm Surge: Hurricane Dorian`=dorian, `Storm Surge: Hurricane Isabel`=isabel,
                `Storm Surge: Hurricane Joaquin`=joaquin, `Storm Surge: King Tide`=kingtide,
                `Storm Surge: Nor'Ida Storm`=norida, 
                `Case Study Areas: Average Water Level Depth`=avg_wld,
                `Case Study Areas: Septic System Risk Assessment`=septic,
                `Housing`=housing
                )

  
  
qs::qsave(app_dat, "app_data_test.qs")


## Population data ----

## Block Group names
blkgrp_names <- read_excel("tract_names.xlsx", sheet = "blkgrp2020")
blkgrp_names <- blkgrp_names %>% 
  mutate(localityfips = str_pad(localityfips, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0"),
         GEOID = paste0("51",localityfips,tract,blkgrp))

# Block group population
pop <- read_csv("population_blkgrp.csv")
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
blkgrp_geo <- st_read("esva_2020blkgrp_clipped.geojson")
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
