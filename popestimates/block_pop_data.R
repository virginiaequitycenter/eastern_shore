# Generate block level population data
# 2024-12-04 mpc


# Libraries ---- 
library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)

# Blocks ---- 
cb <- st_read("cb/esva_2020block_clipped.geojson") %>% 
  st_transform(st_crs(4269))

# ggplot(cb) +
#   geom_sf() +
#   theme_void()


# Decennial Redistricting Files ----
# pl <- load_variables(year = 2020, dataset = "pl")
# dhc <- load_variables(year = 2020, dataset = "dhc")

## Total housing units and occupied housing units ----
housing_vars <- c("total_housing" = "H1_001N",
                  "occupied_housing" = "H1_002N")

housing_units <- get_decennial(geography = "block", 
                        state = "51", 
                        county = c("001", "131"),
                        variables = housing_vars,
                        year = 2020)
# review for availability of tenure by block
# https://www.census.gov/data/tables/2024/dec/2020-census-detailed-dhc-b.html


## Population by race ----
# pop_race_vars <- c("total" = "P1_001N", 
#                    "white" = "P1_003N", 
#                    "black" =  "P1_004N", 
#                    "amind_aknat" = "P1_005N", 
#                    "asian" = "P1_006N", 
#                    "nathw_pacis" = "P1_007N", 
#                    "other_race" = "P1_008N", 
#                    "multi_race" = "P1_009N")
# pop_race <- get_decennial(geography = "block",
#                            state = "51", 
#                            county = c("001", "131"),
#                            variables = pop_race_vars,
#                            year = 2020)

## Population by race/ethnicity ----
pop_race_ethn_vars <- c("total" = "P2_001N", 
                        "hisp" = "P2_002N",
                   "white" = "P2_005N", 
                   "black" =  "P2_006N", 
                   "amind_aknat" = "P2_007N", 
                   "asian" = "P2_008N", 
                   "nathw_pacis" = "P2_009N", 
                   "other_race" = "P2_010N", 
                   "multi_race" = "P2_011N")

pop_race_ethn <- get_decennial(geography = "block",
                               state = "51", 
                               county = c("001", "131"),
                               variables = pop_race_ethn_vars,
                               year = 2020)
# Note: consider reviewing detailed multiracial for inclusion


## Population under 18 ----
## over and under 18 are all that is available 
## (I was wrong about the full age bins)
pop_over18 <- get_decennial(geography = "block",
                            state = "51", 
                            county = c("001", "131"),
                            variables = c("pop_over18" = "P3_001N"),
                            year = 2020)

pop_under18 <- pop_race_ethn %>% 
  filter(variable == "total") %>% 
  bind_rows(pop_over18) %>% 
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = value) %>% 
  mutate(pop_under18 = total - pop_over18) %>% 
  select(-total)


# Low-income/LODES ----
# https://lehd.ces.census.gov/data/
# https://lehd.ces.census.gov/data/lodes/LODES8/va/
# rac: va_rac_S000_JT01_2022.csv.gz
# wac: va_wac_S000_JT01_2022.csv.gz

## rac ----
url1 <- "https://lehd.ces.census.gov/data/lodes/LODES8/va/rac/va_rac_S000_JT01_2022.csv.gz"
download.file(url1, destfile = "lodes/rac_s000.csv.gz")

rac_s000 <- read_csv(gzfile("lodes/rac_s000.csv.gz"))   

rac_s000 <- rac_s000 %>% 
  mutate(h_geocode = as.character(h_geocode),
         county = str_sub(h_geocode,1,5)) %>% 
  filter(county %in% c("51001", "51131")) %>% 
  select(GEOID = h_geocode, jobs_rac = C000, jobs_rac_low = CE01, jobs_rac_mid = CE02, jobs_rac_hi = CE03)

## wac ----
url2 <- "https://lehd.ces.census.gov/data/lodes/LODES8/va/wac/va_wac_S000_JT01_2022.csv.gz"
download.file(url2, destfile = "lodes/wac_s000.csv.gz")

wac_s000 <- read_csv(gzfile("lodes/wac_s000.csv.gz")) 

wac_s000 <- wac_s000 %>% 
  mutate(w_geocode = as.character(w_geocode),
         county = str_sub(w_geocode,1,5)) %>% 
  filter(county %in% c("51001", "51131")) %>% 
  select(GEOID = w_geocode, jobs_wac = C000, jobs_wac_low = CE01, jobs_wac_mid = CE02, jobs_wac_hi = CE03)

# could also obtain
# jobs by
## less than HS, HS or equiv, Some college, BA
## 20 collapsed industry codes, including
### Agriculture, Forestry, Fishing and Hunting; 
### Mining, Quarrying, and Oil and Gas Extraction;
### Utilities; Construction; Manufacturing; Wholesale Trade;
### Retail Trade; Transportation and Warehousing; Information;
### Finance and Insurance; Real Estate and Rental and Leasing; 
### Professional, Scientific, and Technical Services; 
### Management of Companies and Enterprises;
### Administrative and Support and Waste Management and Remediation Services;
### Educational Services; Health Care and Social Assistance; 
### Arts, Entertainment, and Recreation; 
### Accommodation and Food Services; Other Services; Public Administration


# Join all pop data ---- 
# pivot housing and pop data
pop_race_ethn_wide <- pop_race_ethn %>% 
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = value)

housing_wide <- housing_units %>% 
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = value)

block_pop <- pop_race_ethn_wide %>% 
  left_join(pop_under18) %>% 
  left_join(housing_wide) %>% 
  left_join(rac_s000) %>% 
  left_join(wac_s000)

block_pop <- block_pop %>% replace(is.na(.), 0)


# Amend discrepancies ----
# between housing units == 0 and non-zero pop or jobs_rac/jobs_rac_low
block_pop_correction <- block_pop %>% 
  mutate(across(total:pop_under18, ~if_else(total_housing == 0, 0, .)),
         across(jobs_rac:jobs_rac_hi, ~if_else(total_housing == 0, 0, .)))

# Save ----
write_csv(block_pop, "block_pop_data.csv")
write_csv(block_pop_correction, "block_pop_data_updated.csv")

# Join to cb shapefile and map ----
sum(block_pop$GEOID %in% cb$GEOID20) # 2688
sum(cb$GEOID20 %in% block_pop$GEOID) # 2688
# 464 blocks in block_pop but not cb
block_pop %>% 
  filter(! GEOID %in% cb$GEOID20) %>% 
  view()
# all but two are zero pop, zero housing blocks
# (each has occupied housing count of 1)
cb_pop <- cb %>% 
  left_join(block_pop, by = c("GEOID20" = "GEOID"))


# Check ---- 
ggplot(cb_pop) +
  geom_sf(aes(fill = total)) +
  scale_fill_viridis(direction = -1)
  theme_void()
  
ggplot(cb_pop) +
  geom_sf(aes(fill = jobs_wac_low)) +
  scale_fill_viridis(direction = -1)
  theme_void()  
  
# would it be better to make 0s NA?
# yes, i think so
cb_pop %>% 
  mutate(total_na = ifelse(total == 0, NA, total)) %>%  
  ggplot() +
  geom_sf(aes(fill = total_na)) +
  scale_fill_viridis(direction = -1) +
  theme_void()

cb_pop %>% 
  mutate(jobs_wac_na = ifelse(jobs_wac == 0, NA, jobs_wac)) %>%  
  ggplot() +
  geom_sf(aes(fill = jobs_wac_na)) +
  scale_fill_viridis(direction = -1) +
  theme_void()

cb_pop %>% 
  mutate(jobs_rac_low_na = ifelse(jobs_rac_low == 0, NA, jobs_rac_low)) %>%  
  ggplot() +
  geom_sf(aes(fill = jobs_rac_low_na)) +
  scale_fill_viridis(direction = -1) +
  theme_void()

cb_pop %>% 
  mutate(jobs_rac_hi_na = ifelse(jobs_rac_hi == 0, NA, jobs_rac_hi)) %>%  
  ggplot() +
  geom_sf(aes(fill = jobs_rac_hi_na)) +
  scale_fill_viridis(direction = -1) +
  theme_void()
