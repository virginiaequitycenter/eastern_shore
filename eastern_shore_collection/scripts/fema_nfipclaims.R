# Khalila Karefa-Kargbo, Chris Barber
# 2021-07-06, original
# 2023-15-02, updated
# FEMA NFIP Claims (Insurance) Data 

##  Modifications to Khalila's scirpt:
#   updated broken source link, 
#   loaded Census housing data for % of households insured by NFIP calculations,
#   adjusting for inflation,
#   removed code pertaining to Cville, 
#   added object for all ES claims vs only claims >=2010,
#   kept all original claims columns in fema_es_2010, 

# Source link: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v1

library(tidyverse)
library(mosaic)  
library(tidycensus)
library(tigris)
library(DataExplorer)
library(sf)
library(leaflet)
library(viridis)

##  Define localities of interest

es <- c('51001','51131')

##  Get NFIP claims & Census data 

# claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")

claims <- readRDS('fema_df.rds') #keeping this in for my purposes

vars <- load_variables(year = 2021,  dataset = "acs5")

housing_vars <- c('B25001_001','B25003_002','B25003_003',
                  'B25081_001', 'B25081_009', 'B25002_003')


es_housing <- get_acs(geography = 'tract', variables = housing_vars,
                      year = 2021, state = 'VA',
                      county = c('Accomack', 'Northampton'),
                      geometry = T,
                      output = 'wide')

es_housing <- es_housing %>%
  rename('Total_Housing_Units' = B25001_001E,
         'Total_Owner_Occupied' = B25003_002E,
         'Total_Renter_Occupied' = B25003_003E,
         'Total_Units_wMortgage' = B25081_001E,
         'Total_Units_noMortgage' = B25081_009E,
         'Total_Vacant_Units' = B25002_003E)


##  Filter to region & year

fema_va <- claims %>%
  filter(state == 'VA') %>%
  rename(GEOID = censusTract)

# write_rds(fema_va, "fema_df.rds")

fema_es_all <- claims %>%
  filter(countyCode %in% es)

fema_es_2010 <- fema_es_all %>%
  filter(yearOfLoss >= 2010)

##  Adjusting for inflation 
#   Using BLS CPI "Chained CPI for All Urban Consumers, U.S. city average (C-CPI-U)" annual averages 
#   Available at https://www.bls.gov/cpi/data.html

inflation_index <- tribble(~Year, ~CPI, 2010, 125.615, 2011, 129.453,
                           2012, 131.976, 2013, 133.592, 2014, 135.524,
                           2015, 135.362, 2016, 136.625, 2017, 139.036,
                           2018, 141.842, 2019, 143.904, 2020, 145.449,
                           2021, 151.950, 2022, 163.668)

inflation_index <- inflation_index %>% rename(yearOfLoss = Year)

inflation_index <- inflation_index %>% 
  mutate(Inflation_Pct = (CPI - lag(CPI))/lag(CPI)*100) 

inflation_index$Inflation_Pct[[1]] = 1.425

inflation_index$Inflation_Pct <- round(inflation_index$Inflation_Pct, 2)
#thought I'd plot this, but not necessary

fema_es_2010 <- inner_join(fema_es_2010, inflation_index, by = "yearOfLoss")

fema_es_2010 <- fema_es_2010 %>%
  mutate(Adj_BuildingClaim = amountPaidOnBuildingClaim/CPI * 100,
         Adj_ContentsClaim = amountPaidOnContentsClaim/CPI * 100,
         Adj_ComplianceClaim = amountPaidOnIncreasedCostOfComplianceClaim/CPI * 100)

fema_es_2010 <- fema_es_2010 %>% rowwise() %>% mutate(
  Adj_TotalPaid = sum(c(Adj_BuildingClaim, Adj_ContentsClaim), na.rm = T)) %>%
  mutate(Adj_TotalPaid = round(Adj_TotalPaid, 2))

# Calculating unadjusted claims amounts

fema_es_2010 <- fema_es_2010 %>% rowwise() %>% mutate(
  Orig_TotalPaid = sum(c(amountPaidOnBuildingClaim, amountPaidOnContentsClaim), na.rm = T)) %>%
    mutate(Orig_TotalPaid = round(Orig_TotalPaid, 2))

p1 <- fema_es_2010 %>%
  select(yearOfLoss, censusTract, Orig_TotalPaid, Adj_TotalPaid) %>%
  group_by(yearOfLoss) %>%
  summarise(TotOrig = sum(Orig_TotalPaid, na.rm = T),
            TotAdj = sum(Adj_TotalPaid, na.rm = T))

p1 %>% ggplot(aes(TotOrig, as.factor(yearOfLoss))) + geom_point(color = 'blue') +
  geom_point(data = p1, aes(TotAdj), color = 'red') +
  coord_flip() #not sure how useful this is, but still interesting 
  
##  Mapping claim coordinate to assess accuracy of tract location 

fema_es_2010 <- st_as_sf(fema_es_2010,
                         coords = c('longitude', 'latitude'),
                         crs = 4326)

es_housing <- st_transform(es_housing, 4326)

pal <- colorNumeric('viridis', domain = fema_es_2010$Adj_TotalPaid)

leaflet(fema_es_2010) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = es_housing, fillColor = 'white', fillOpacity = 0.25,
              weight = 0.5, label = es_housing$GEOID) %>%
  addCircleMarkers(data = fema_es_2010,
                   color = ~pal(Adj_TotalPaid),
                   stroke = F, opacity = 0.75, 
                   popup = fema_es_2010$censusTract)
# Hover over tract and click on point to gauge how accurate it is
# doesn't seem to be very useful

### Lee's addition: #########################################################################
# trying to figure out a way to see if the 
# fema reported census tract matches the census tract from the lat 
# and long they report as well. 

# going to create a map where the census tracts are colored, and the 
# points are colored by the census tracts reported by fema to see 
# if they fall in a matching color 

# need to create factors in the data that will map the polygons and the 
# data for the circles so that each census tract gets assigned the same color 

# creating data frame for colors and census tracts so that they will match 

cl <- data.frame(color = c("#f94144","#f9844a","#4d908e","#c8b6ff",
                                    "#90be6d", "#43aa8b", "#fff3b0", "#da627d",
                                    "#577590", "#277da1", "#218380", "#b5838d",
                                    "#00509d", "#6930c3", "#d81159","#a68a64",
                                    "#8f2d56", "#f8961e", "#ffca3a", "#6b9080"),
                  censusTract = c("51001990100", "51001090300","51001090401", "51001090102", "51001090700",
                                  "51131930100", "51131930200", "51001090202", "51001090500", "51131990100",
                                  "51001980100", "51001090101", "51001980200", "51001090800", "51001090600",
                                  "51001090402", "51001090201", "51131930301", "51131930302", "51001990200"))

es_housing <- es_housing %>%
  left_join(cl, by = "censusTract")

fema_es_2010 <- fema_es_2010 %>%
  rename(censusTract = GEOID) %>%
  left_join(cl, by = "censusTract")

# fema_es_2010 %>% 
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
#   st_jitter(factor = 0.001)
                     
# pal <- colorFactor(colors, domain = es_housing$censusTract)
                     
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = es_housing, 
              fillColor = es_housing$color, 
              fillOpacity = .5,
              weight = 0.5, label = es_housing$censusTract) %>%
  addCircleMarkers(data = fema_es_2010,
                   fillColor = fema_es_2010$color,
                   stroke = F, opacity = 1, radius = 3,
                   popup = fema_es_2010$censusTract)


# What are the stats per Census tract? 
# I haven't applied any inflation adjustments to this 

claims_bytract <- fema_es_2010 %>%
  group_by(censusTract) %>%
  filter(!is.na(censusTract)) %>%
  summarize(n = count(censusTract),
            TotalAmountPaidOnBuilding = sum(amountPaidOnBuildingClaim, na.rm = T),
            TotalBuildingCoverage = sum(totalBuildingInsuranceCoverage, na.rum = T),
            TotalAmountPaidOnContents = sum(amountPaidOnContentsClaim, na.rm = T),
            TotalContentsCoverage = sum(totalContentsInsuranceCoverage, na.rm = T),
            TotalAmountPaidOnCompliance = sum(amountPaidOnIncreasedCostOfComplianceClaim, na.rm = T))

# Creating summary variables

claims_bytract <- claims_bytract %>%
  mutate(TotalAmountPaid = TotalAmountPaidOnBuilding + TotalAmountPaidOnContents,
         TotalCoverage = TotalBuildingCoverage + TotalContentsCoverage,
         BuildingPayoutDifference = TotalBuildingCoverage - TotalAmountPaidOnBuilding,
         ContentsPayoutDifference = TotalContentsCoverage - TotalAmountPaidOnContents)
            
# Save to csv
write_csv(claims_bytract, path = "es_claimsbytract.csv")