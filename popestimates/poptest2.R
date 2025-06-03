# Test population estimates workflow
# with final data files
# 2025-06-03 mpc


# Setup ----
library(boxr)
library(sf)
library(jsonlite)
library(tidyverse)
library(patchwork)

pal <- c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000")


# Read data ----
## Population data ----
# does this need to be formalized in box?
# currently read locally, added to repo
pop <- read_csv("block_pop_data.csv") %>% 
  mutate(GEOID = as.character(GEOID))

## Block shapefile ----
# does this need to be formalized in box?
# currently read locally, 
# but available in box as geojson
# https://virginia.app.box.com/file/1669511851078 
# (though not readable by box_read function)
blocks <- st_read("cb/esva_2020block_clipped.geojson")

## Groundwater data ----
# Choose one of Farshad's files
# https://virginia.app.box.com/file/1869334139989
gw_json_export <- box_read_json("1869334139989") # read in single ea_export json file
gw_json_boxid <- str_extract(gw_json_export$data$path, "(\\d+)") # get id of csv json file
gw_json_csv <- box_read_json(gw_json_boxid) # read in csv json file
gw_csv_boxid <- str_extract(gw_json_csv$path, "(\\d+)") # get id of csv data file
gw_csv <- box_read_csv(gw_csv_id) %>% # read data file
  mutate(GEOID20 = as.character(GEOID20))

## Combine data ----
# sum(gw_csv$GEOID20 %in% blocks$GEOID20) # 2647
gw_df <- left_join(blocks, pop, by=c("GEOID20"="GEOID")) %>%
  left_join(gw_csv, by = c("GEOID20"="GEOID20")) %>% 
  st_drop_geometry()


# Generate impacted pop estimates ----
## Total population baselines ----
pop_total <- pop %>% 
  summarize(across(c(total:jobs_wac_hi), sum)) %>% 
  mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
         per_occupied_housing = (occupied_housing/total_housing)*100,
         across(c(jobs_rac_low:jobs_rac_hi), ~ (.x/jobs_rac)*100, .names = "per_{.col}"),
         across(c(jobs_wac_low:jobs_wac_hi), ~ (.x/jobs_wac)*100, .names = "per_{.col}"),
         bin = "ESVA Total", event = "none")

## Data set variables ---- 
### identify variables to use ----
var_name <- as.vector(gw_json_export$dataColumns$name)

# identify bins (here bins are same for all identified variables)
legend_breaks <- as.vector(gw_json_export$dataColumns$bins[[1]])
legend_labels <- as.list(gw_json_export$dataColumns$labels[[1]])

### create bins for identified variables ----
# binning function
bin_function <- function(var_name) {
  var_name_bin = cut(var_name,
            breaks = legend_breaks,
            labels = legend_labels,
            include.lowest = TRUE)
}

# create bin_ variable for each variable identified in ea json
gw_df_test <- gw_df %>% 
  mutate(across(all_of(var_name), bin_function,
                .names = "bin_{.col}"),
         event = gw_json_export$event)


### create summary df by variable bins ----
# TODO, make this mappable across group_vars
# e.g., use syms() so objects are understandable as data frame column names
group_vars <- syms(gw_df_test %>% select(starts_with("bin")) %>% names())
# to create n_x summary data frames... but for now try one
gw_df_var1 <- gw_df_test %>% 
  group_by(!!group_vars[[1]], event) %>% 
  summarize(across(c(total:jobs_wac_hi), sum)) %>% 
  mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
         per_occupied_housing = (occupied_housing/total_housing)*100,
         across(c(jobs_rac_low:jobs_rac_hi), ~ (.x/jobs_rac)*100, .names = "per_{.col}"),
         across(c(jobs_wac_low:jobs_wac_hi), ~ (.x/jobs_wac)*100, .names = "per_{.col}")) %>% 
  ungroup() %>% 
  mutate(per_total = (total/sum(total))*100,
         per_housing = (total_housing/sum(total_housing))*100) %>% 
  rename(bin = !!group_vars[[1]]) %>% 
  bind_rows(pop_total) %>% 
  mutate(bin = factor(bin, levels = c(legend_labels, "ESVA Total")))


# Example visual ----
## Single Bar ----
gw_df_var1 %>% 
  filter(event != "none") %>% 
  ggplot(aes(x = bin, y = per_hisp)) +
  geom_col(aes(fill = bin)) +
  geom_text(aes(label = round(per_hisp, 1)), 
            vjust = 1) +
  scale_fill_manual(values = pal, na.value="grey", guide = "none") +
  expand_limits(x= c(1, 8)) +
  geom_hline(aes(yintercept = gw_df_var1_total %>% 
                   filter(event == "none") %>% 
                   pull(per_hisp)), 
             linetype = 2, color = "brown4") +
  annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
           y = gw_df_var1_total %>% 
             filter(event == "none") %>% 
             pull(per_hisp)) +   
  labs(title = "Percent of Residents: Hispanic",
       subtitle = "By Impact: Groundwater Depth",
       x = "",
       y = "") +
  theme_classic()

## Bars over all popvars ----
# probably need to further limit the pop variables
# proposed for removal: 
#   amind_aknat, asian, nathw_pacis, other_race,
#   pop_over18, jobs_rac_low, jobs_rac_mid, jobs_rac_hi
#   possibly: jobs_wac_mid, jobs_wac_hi 

## all except population and housing totals
popvars <- select(gw_df_var1_total, starts_with("per")) %>% names()
poplabels <- c("Hispanic", "White", "Black", "American Indian/Alaskan Native",
               "Asian", "Native Hawaiian/Pacific Islander", "Remaining Racial Categories",
               "Multiracial", "Over 18", "Under 18", "Occupied Housing",
               "Low-wage Jobs by Residence", "Mid-wage Jobs by Residence",
               "High-wage Jobs by Residence", "Low-wage Jobs by Workplace",
               "Mid-wage Jobs by Workplace", "High-wage Jobs by Workplace",
               "Total Population", "Total Housing Stock")

bar_plots <- map2(popvars[1:17], poplabels[1:17], 
                        ~ gw_df_var1 %>% 
                          filter(event != "none") %>% 
                          ggplot(aes(x = bin, y = .data[[.x]])) +
                          geom_col(aes(fill = bin)) +
                          geom_text(aes(y = .data[[.x]], 
                                        label = round(.data[[.x]], 1)),
                                    vjust = 1, color = "black") +
                          geom_hline(aes(yintercept = gw_df_var1 %>% 
                                           filter(bin == "ESVA Total") %>% 
                                           pull(.x)), color = "brown4", linetype = 2) +
                          expand_limits(x= c(1, 8)) +
                          annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
                                   y = gw_df_var1 %>% 
                                     filter(event == "none") %>% 
                                     pull(.x)) +   
                          scale_fill_manual(values = pal, na.value="grey", guide = "none") +
                          labs(title = paste0("Percent of Residents: ", .y),
                               subtitle = "By Impact",
                               x = "", y = "") +
                          theme_classic())

bar_plots[17]
# wrap_plots(bar_plots)

# prop_total and prop_housing shouldn't have total reference line
bar_plots_tot <- map2(popvars[18:19], poplabels[18:19], 
                            ~ gw_df_var1 %>% 
                              filter(event != "none") %>% 
                              ggplot(aes(x = bin, y = .data[[.x]])) +
                              geom_col(aes(fill = bin)) +
                              geom_text(aes(y = .data[[.x]], 
                                            label = round(.data[[.x]], 1)),
                                        vjust = 1, color = "black") +
                              scale_fill_manual(values = pal, na.value="grey", guide = "none") +
                              labs(title = paste0("Percent of: ", .y),
                                   subtitle = "By Impact",
                                   x = "", y = "") +
                              theme_classic())
bar_plots_tot[1]
# wrap_plots(bar_plots_tot)
