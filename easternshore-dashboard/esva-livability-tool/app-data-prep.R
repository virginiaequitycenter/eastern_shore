# Libraries
library(here)
library(shiny)
library(tidyverse)
library(boxr)
library(jsonlite) 
library(sf)
library(leaflet)
library(RColorBrewer)
library(bslib)
library(qs)

# Set WD
setwd(here("esva-livability-tool"))

# Box authentication
readRenviron("~/.Renviron")
box_auth(client_id = Sys.getenv('BOX_CLIENT_ID'), client_secret = Sys.getenv('BOX_CLIENT_SECRET'))

# # Get Box Data
# dir_id <- "305971996036" # EA_Export
# box_setwd(dir_id)
# 
# # box_ls()
# 
# box_fetch(
#   dir_id = box_getwd(),
#   local_dir = "app_data/app_data_updated",
#   recursive = TRUE,
#   overwrite = TRUE,
#   delete = TRUE
# )

# Data prep ----

# Get Box ID function
get_boxid <- function(path) {
  boxid <- str_split(path, "https://virginia.app.box.com/file/", simplify = TRUE)[2]
  boxid
}


block_geo <- st_read("data/blocks/esva_2020block_clipped.geojson")
block_geo <- st_transform(block_geo, 4326) %>% 
  select(GEOID20)

# Population data ---
pop_dat <- box_read_csv("1883093293417") %>% 
  mutate(GEOID20 = as.character(GEOID)) %>% 
  select(GEOID20, total, hisp, black, white, pop_under18, total_housing, occupied_housing, jobs_wac, jobs_wac_low)

## Total population baselines ----
pop_total <- pop_dat %>% 
  summarize(across(c(total:jobs_wac_low), sum)) %>% 
  mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
         per_occupied_housing = (occupied_housing/total_housing)*100,
         across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}"),
         bin = "ESVA Total", event = "none")


# Data prep function
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
  csv_pop <- csv %>% 
    mutate(GEOID20 = as.character(GEOID20)) %>% 
    left_join(pop_dat, by = join_by(GEOID20 == GEOID20))
  
  # Gather data for app
  region_bbox <- ea_export$regionBoundingBox
  bbox <- c(region_bbox$lonMin, region_bbox$latMin, region_bbox$lonMax, region_bbox$latMax)
  
  year <- ea_export$data$key
  descriptionTitle <- ea_export$descriptionTitle
  event <- ea_export$event
  description <- ea_export$descriptionText
  
 measures <- list() 
  
  for (i in seq_along(ea_export$dataColumns$name)) {
    name <- ea_export$dataColumns$name[i]
    name_index <- match(name, json$schema$fields$name)
    title <- json$schema$fields$title[name_index]
    field_description <- json$schema$fields$description[name_index]
    # print(title)
    
    var <- quo(name)
    map_data <- csv_geo %>% select(GEOID20, UQ(var), geometry)
    
    # print(dat)

    legend_breaks <- as.vector(ea_export$dataColumns$bins[[i]])
    legend_labels <- as.list(ea_export$dataColumns$labels[[i]])
    
    # binning function
    bin_function <- function(name) {
      var_name_bin = cut(name,
                         breaks = legend_breaks,
                         labels = legend_labels,
                         include.lowest = TRUE)
    }
    
    # create bin_ variable for each variable identified in ea json
    csv_pop <- csv_pop %>% 
      mutate(across(all_of(name), bin_function,
                    .names = "bin_{.col}"),
             event = event)
    
    group_vars <- syms(csv_pop %>% select(starts_with("bin")) %>% names())
    
    pop_data <- csv_pop %>% 
      group_by(!!group_vars[[1]], event) %>% 
      summarize(across(c(total:jobs_wac_low), sum)) %>% 
      mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
             per_occupied_housing = (occupied_housing/total_housing)*100,
             across(c(jobs_wac_low), ~ (.x/jobs_wac)*100, .names = "per_{.col}")) %>% 
      ungroup() %>% 
      mutate(per_total = (total/sum(total))*100,
             per_housing = (total_housing/sum(total_housing))*100) %>% 
      rename(bin = group_vars[[1]]) %>% 
      bind_rows(pop_total) %>% 
      mutate(bin = factor(bin, levels = c(legend_labels, "ESVA Total"))) %>% 
      select(bin, event, starts_with("per_"))

    ls_name <- as.character(title)
    ls_name
    ls <- list(name=name, map_data=map_data, title=title, field_description=field_description, 
               legend_breaks=legend_breaks, legend_labels=legend_labels, pop_data=pop_data)
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

# Storm Surge: Hurricane Dorian
dr_2019 <- ea_prep_func(list.files("app_data/app_data_updated")[7])
dorian <- list("2019"=dr_2019)

# Storm Surge Isabel
ib_2003 <- ea_prep_func(list.files("app_data/app_data_updated")[8])
ib_2025 <- ea_prep_func(list.files("app_data/app_data_updated")[10])
ib_2030 <- ea_prep_func(list.files("app_data/app_data_updated")[11])
ib_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[12])
ib_2050 <- ea_prep_func(list.files("app_data/app_data_updated")[13])
ib_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[14])
ib_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[15])

isabel <- list("2003"=ib_2003, "2025" = ib_2025, "2030"=ib_2030, 
               "2040"=ib_2040, "2050"=ib_2050, 
               "2060"=ib_2060, "2080"=ib_2080)

# Storm Surge Hurricane Joaquin
jq_2015 <- ea_prep_func(list.files("app_data/app_data_updated")[16])
joaquin <- list("2015"=jq_2015)

# Storm Surge King Tide
kt_2009 <- ea_prep_func(list.files("app_data/app_data_updated")[17])
kingtide <- list("2009"=kt_2009)

# Storm Surge Nor'Ida Storm
ni_2009 <- ea_prep_func(list.files("app_data/app_data_updated")[18])
norida <- list("2009"=ni_2009)

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

# Roadway flooding
rdflood_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[32])
rdflood_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[33])
rdflood_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[34])

roadflood <- list("2040"=rdflood_2040, "2060"=rdflood_2060, "2080"=rdflood_2080)

# Water Level Depth
wld_2020 <- ea_prep_func(list.files("app_data/app_data_updated")[35])
avg_wld <- list("2020-2023"=wld_2020)
# Septic System Risk Assessment
ssra_2020 <- ea_prep_func(list.files("app_data/app_data_updated")[36])
septic <- list("2020-2023"=ssra_2020)

# Seawater intrusion
swi_2024 <- ea_prep_func(list.files("app_data/app_data_updated")[37])
swi_2030 <- ea_prep_func(list.files("app_data/app_data_updated")[38])
swi_2040 <- ea_prep_func(list.files("app_data/app_data_updated")[39])
swi_2050 <- ea_prep_func(list.files("app_data/app_data_updated")[40])
swi_2060 <- ea_prep_func(list.files("app_data/app_data_updated")[41])
swi_2080 <- ea_prep_func(list.files("app_data/app_data_updated")[42])

swi <- list("2024" = swi_2024, "2030"=swi_2030, 
                    "2040"=swi_2040, "2050"=swi_2050, 
                    "2060"=swi_2060, "2080"=swi_2080)


# Compile App data ----
# app_dat <- list(groundwater_2030=groundwater_2030, groundwater_2040=groundwater_2040)
app_dat <- list(`Depth to Groundwater`=groundwater, `Extreme Wetness/Dryness`=extremes, 
                `Roadway Flooding`=roadflood,
                `Seawater Intrusion`=swi, 
                `Storm Surge: Hurricane Dorian`=dorian, `Storm Surge: Hurricane Isabel`=isabel,
                `Storm Surge: Hurricane Joaquin`=joaquin, `Storm Surge: King Tide`=kingtide,
                `Storm Surge: Nor'Ida Storm`=norida, 
                `Case Study Areas: Average Water Level Depth`=avg_wld,
                `Case Study Areas: Septic System Risk Assessment`=septic
                )

  
  
qs::qsave(app_dat, "app_data_test.qs")
