# Test population estimates workflow
# all attributes over one layer
# with final data files
# 2025-09-23 mpc; 2025-10-31-risk ratio
# pat's composite risk measure


# Setup ----
library(boxr)
library(sf)
library(jsonlite)
library(tidyverse)
library(patchwork)
library(plotly)

pal <- RColorBrewer::brewer.pal(5, "Purples")

# Read data ----
box_auth()

## Population data ----
pop <- box_read_csv("2026502973438") %>% 
  mutate(GEOID20 = as.character(GEOID))

## Storm Surge Risk data ----
# esva_Block20_StormSurgeRisk_EA.json
# https://virginia.app.box.com/file/1984496287140
ssr_json_export <- box_read_json("1984496287140") # read in single ea_export json file
ssr_json_boxid <- str_extract(ssr_json_export$data$path, "(\\d+)") # get id of csv json file
ssr_json_csv <- box_read_json(ssr_json_boxid) # read in csv json file
ssr_csv_id <- str_extract(ssr_json_csv$path, "(\\d+)") # get id of csv data file
ssr_csv <- box_read_csv(ssr_csv_id) %>% # read data file
  mutate(GEOID20 = as.character(GEOID20))

## Combine data ----
# sum(ssr_csv$GEOID20 %in% pop$GEOID20) # 2688
ssr_df <- left_join(ssr_csv, pop, by=c("GEOID20"="GEOID20")) 


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
var_name <- as.vector(ssr_json_export$dataColumns$name)

# identify bins (here bins are same for all identified variables)
legend_breaks <- as.vector(ssr_json_export$dataColumns$bins[[1]])
legend_labels <- as.list(ssr_json_export$dataColumns$labels[[1]])

### create bins for identified variables ----
# binning function
bin_function <- function(var_name) {
  var_name_bin = cut(var_name,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     include.lowest = TRUE)
}

# create bin_ variable for each variable identified in ea json
ssr_df <- ssr_df %>% 
  mutate(across(all_of(var_name), bin_function,
                .names = "bin_{.col}"),
         event = ssr_json_export$event)


### create summary df by variable bins ----
# TODO, make this mappable across group_vars
# e.g., use syms() so objects are understandable as data frame column names
group_vars <- syms(ssr_df %>% select(starts_with("bin")) %>% names())
# group_vars2 <- as.vector(gw_df %>% select(starts_with("bin")) %>% names())
# group_vars_description <- as.vector(gw_json_export$dataColumns$`$description`)

# to create n_x summary data frames... but for now try one
ssr_df_estimates <- ssr_df %>% 
  group_by(!!group_vars[[1]], event) %>% 
  summarize(num_blocks = n(),
            across(c(total:jobs_wac_hi), sum)) %>% 
  mutate(across(c(hisp:pop_under18), ~ (.x/total)*100, .names = "per_{.col}"),
         per_occupied_housing = (occupied_housing/total_housing)*100,
         across(c(jobs_rac_low:jobs_rac_hi), ~ (.x/jobs_rac)*100, .names = "per_{.col}"),
         across(c(jobs_wac_low:jobs_wac_hi), ~ (.x/jobs_wac)*100, .names = "per_{.col}")) %>% 
  ungroup() %>% 
  mutate(per_blocks = (num_blocks/sum(num_blocks))*100,
         per_total = (total/sum(total))*100,
         per_housing = (total_housing/sum(total_housing))*100) %>% 
  rename(bin = group_vars[[1]]) %>% 
  bind_rows(pop_total) %>% 
  mutate(bin = factor(bin, levels = c(legend_labels, "ESVA Total")))

## Add risk ratios ----
# test specific: hisp/sum(hisp) / (total-hisp)/sum(total)-sum(hisp))
ssr_df_test_rr <- ssr_df_estimates %>% 
  filter(event != "none") %>% 
  mutate(ratio_black = (black/sum(black) / ( (total-black) / (sum(total)-sum(black)) )))

# generalize: 
ssr_df_rr_estimates <- ssr_df_estimates %>% 
  filter(event != "none") %>% 
  mutate(across(c(hisp:pop_under18), 
                ~ (.x/sum(.x) / ((total-.x) / (sum(total) -sum(.x)) )), .names = "rr_{.col}"),
         across(c(jobs_rac_low:jobs_rac_hi), ~ (.x/sum(.x) / ((jobs_rac-.x) / (sum(jobs_rac) -sum(.x)) )), .names = "rr_{.col}"),
         across(c(jobs_wac_low:jobs_wac_hi), ~ (.x/sum(.x) / ((jobs_wac-.x) / (sum(jobs_wac) -sum(.x)) )), .names = "rr_{.col}"))


# Example visual: composition ----
## Single Bar ----
ssr_df_estimates %>% 
  filter(event != "none") %>% 
  ggplot(aes(x = bin, y = per_white, label = white)) +
  geom_col(aes(fill = bin)) +
  geom_text(aes(label = round(per_white, 1)), 
            vjust = 1) +
  scale_fill_manual(values = pal, na.value="grey", guide = "none") +
  expand_limits(x= c(1, 8)) +
  geom_hline(aes(yintercept = ssr_df_estimates %>% 
                   filter(event == "none") %>% 
                   pull(per_white)), 
             linetype = 2, color = "brown4") +
  annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
           y = ssr_df_estimates %>% 
             filter(event == "none") %>% 
             pull(per_white)) +   
  labs(title = "Percent of Residents: White",
       subtitle = "By Impact: ",
       x = "",
       y = "") +
  theme_classic()  


# ## Single dot ----
# # (to weight by hh units)
# ssr_df_test %>% 
#   mutate(housing_bins = case_when(
#     total_housing == 0 ~ 1,
#     total_housing > 0 & total_housing <= 10 ~ 2,
#     total_housing > 10 & total_housing <= 100 ~ 3,
#     total_housing > 100 & total_housing <= 1000 ~ 4,
#     total_housing > 1000 & total_housing <= 10000 ~ 5,
#     total_housing > 10000 ~ 6
#   )) %>% 
#   filter(event != "none") %>% 
#   ggplot(aes(x = bin, y = per_white)) +
#   geom_segment(aes(x=bin, yend = per_white, y = 0), color = "grey75") +
#   geom_point(aes(color = bin, size = housing_bins)) +
#   geom_text(aes(label = round(per_white, 1)), 
#             nudge_y = 7) +
#   scale_color_manual(values = pal, na.value="grey", guide = "none") +
#   scale_size_continuous(range = c(1,10),
#                         labels = c("Up to 10", "11 to 100", "101 to 1,000", "1,001 to 10,000", "Over 10,000")) +
#   expand_limits(x= c(1, 8), y = c(0,100)) +
#   geom_hline(aes(yintercept = ssr_df_test %>% 
#                    filter(event == "none") %>% 
#                    pull(per_white)), 
#              linetype = 2, color = "brown4") +
#   annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
#            y = ssr_df_test %>% 
#              filter(event == "none") %>% 
#              pull(per_white)) +   
#   labs(title = "Percent of Residents: White",
#        subtitle = "By Impact: ",
#        x = "",
#        y = "") +
#   theme_classic()

## Variable bar width ----
# (to scale width by housing units) 
# ssr_df_test %>% 
#   mutate(housing_share = total_housing/max(total_housing)) %>% 
#   filter(event != "none") %>% 
#   ggplot(aes(x = bin, y = per_white)) +
#   geom_col(aes(fill = bin, width = housing_share),
#            position = "identity") +
#   geom_text(aes(label = round(per_white, 1)), 
#             vjust = 1) +
#   scale_fill_manual(values = pal, na.value="grey", guide = "none") +
#   expand_limits(x= c(1, 8)) +
#   geom_hline(aes(yintercept = ssr_df_test %>% 
#                    filter(event == "none") %>% 
#                    pull(per_white)), 
#              linetype = 2, color = "brown4") +
#   annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
#            y = ssr_df_test %>% 
#              filter(event == "none") %>% 
#              pull(per_white)) +   
#   labs(title = "Percent of Residents: White",
#        subtitle = "By Impact: ",
#        x = "",
#        y = "") +
#   theme_classic()

# Example Visual: Risk ----
ssr_df_rr_estimates %>% 
  filter(event != "none") %>% 
  ggplot(aes(x = bin, y = rr_white, label = white)) +
  geom_col(aes(fill = bin)) +
  geom_text(aes(label = paste0(round(rr_white, 1),"x")), 
            vjust = 1) +
  scale_fill_manual(values = pal, na.value="grey", guide = "none") +
  scale_y_continuous(trans = "log",
                     breaks = c(0.1, 0.25, 0.5, 0.75, 1, 1.25, 2, 3, 4),
                     labels = c("0.10", "0.25", "0.5", "0.75", "1", "1.25", "2", "3", "4")) +
  geom_hline(yintercept = 1, 
             linetype = 2, color = "black") +
  labs(title = "Risk Ratio: Black",
       subtitle = "By Impact: ",
       x = "",
       y = "") +
  theme_classic()  


# Bars over all popvars ----
# probably need to further limit the pop variables
# proposed for removal: 
#   amind_aknat, asian?, nathw_pacis, other_race,
#   pop_over18, jobs_rac_low, jobs_rac_mid, jobs_rac_hi
#   possibly: jobs_wac_hi 

## all except population and housing totals
popvars <- select(ssr_df_estimates, starts_with("per")) %>% names()
poplabels <- c("Hispanic", "White", "Black", "American Indian/Alaskan Native",
               "Asian", "Native Hawaiian/Pacific Islander", "Remaining Racial Categories",
               "Multiracial", "Over 18", "Under 18", "Occupied Housing",
               "Low-wage Jobs by Residence", "Mid-wage Jobs by Residence",
               "High-wage Jobs by Residence", "Low-wage Jobs by Workplace",
               "Mid-wage Jobs by Workplace", "High-wage Jobs by Workplace",
               "Total Blocks", "Total Population", "Total Housing Units")

## Composition ----
bar_plots <- map2(popvars[1:17], poplabels[1:17], 
                  ~ ssr_df_estimates %>% 
                    filter(event != "none") %>% 
                    ggplot(aes(x = bin, y = .data[[.x]])) +
                    geom_col(aes(fill = bin)) +
                    geom_text(aes(y = .data[[.x]], 
                                  label = round(.data[[.x]], 1)),
                              vjust = 1, color = "black") +
                    geom_hline(aes(yintercept = ssr_df_estimates %>% 
                                     filter(bin == "ESVA Total") %>% 
                                     pull(.x)), color = "brown4", linetype = 2) +
                    expand_limits(x= c(1, 8)) +
                    annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
                             y = ssr_df_estimates %>% 
                               filter(event == "none") %>% 
                               pull(.x)) +   
                    scale_fill_manual(values = pal, na.value="grey", guide = "none") +
                    labs(title = paste0("Percent of Residents: ", .y),
                         subtitle = "By Impact",
                         x = "", y = "") +
                    theme_classic())

bar_plots[17]
# wrap_plots(bar_plots)

# dot_plots <- map2(popvars[1:17], poplabels[1:17], 
#                ~ ssr_df_test %>% 
#                  mutate(housing_bins = case_when(
#                    total_housing <= 10 ~ 2,
#                    total_housing > 10 & total_housing <= 100 ~ 3,
#                    total_housing > 100 & total_housing <= 1000 ~ 4,
#                    total_housing > 1000 & total_housing <= 10000 ~ 5,
#                    total_housing > 10000 ~ 6
#                  )) %>%
#                  filter(event != "none") %>% 
#                  ggplot(aes(x = bin, y = .data[[.x]])) +
#                  geom_segment(aes(x=bin, yend = .data[[.x]], y = 0), color = "grey75") +
#                  geom_point(aes(color = bin, size = housing_bins)) +
#                  geom_text(aes(y = .data[[.x]], 
#                                label = round(.data[[.x]], 1)),
#                            nudge_y = 7, color = "black") +
#                  geom_hline(aes(yintercept = ssr_df_test %>% 
#                                   filter(bin == "ESVA Total") %>% 
#                                   pull(.x)), color = "brown4", linetype = 2) +
#                  expand_limits(x= c(1, 8), y = c(0,100)) +
#                  annotate("text", x = 7, label = "ESVA\nTotal", color = "brown4",
#                           y = ssr_df_test %>% 
#                             filter(event == "none") %>% 
#                             pull(.x)) +   
#                  scale_size_continuous(range = c(1,10),
#                                        labels = c("Up to 10", "11 to 100", "101 to 1,000", "1,001 to 10,000", "Over 10,000")) +
#                  scale_color_manual(values = pal, na.value="grey", guide = "none") +
#                  labs(title = paste0("Percent of Residents: ", .y),
#                       subtitle = "By Impact",
#                       x = "", y = "") +
#                  theme_classic())
# 
# dot_plots[3]
# wrap_plots(dot_plots)

# prop_total and prop_housing shouldn't have total reference line
bar_plots_tot <- map2(popvars[18:20], poplabels[18:20], 
                      ~ ssr_df_estimates %>% 
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

## Risk ----
popvars_rr <- select(ssr_df_rr_estimates, starts_with("rr")) %>% names()
poplabels_rr <- c("Hispanic", "White", "Black", "American Indian/Alaskan Native",
               "Asian", "Native Hawaiian/Pacific Islander", "Remaining Racial Categories",
               "Multiracial", "Over 18", "Under 18", 
               "Low-wage Jobs by Residence", "Mid-wage Jobs by Residence",
               "High-wage Jobs by Residence", "Low-wage Jobs by Workplace",
               "Mid-wage Jobs by Workplace", "High-wage Jobs by Workplace")

# removed: "Occupied Housing", "Total Blocks", "Total Population", "Total Housing Units"
bar_plots_rr <- map2(popvars_rr[1:16], poplabels_rr[1:16], 
                  ~ ssr_df_rr_estimates %>% 
                    filter(event != "none") %>% 
                    ggplot(aes(x = bin, y = .data[[.x]])) +
                    geom_col(aes(fill = bin)) +
                    geom_text(aes(y = .data[[.x]], 
                                  label = paste0(round(.data[[.x]], 1),"x")),
                              vjust = 1, color = "black") +
                    geom_hline(yintercept = 1, color = "black", linetype = 2) +
                    scale_y_continuous(trans = "log",
                                       breaks = c(0.1, 0.25, 0.5, 0.75, 1, 1.25, 2, 3, 4),
                                       labels = c("0.10", "0.25", "0.5", "0.75", "1", "1.25", "2", "3", "4")) +
                    scale_fill_manual(values = pal, na.value="grey", guide = "none") +
                    labs(title = paste0("Risk Ratio: ", .y),
                         subtitle = "By Impact",
                         x = "", y = "") +
                    theme_classic())

bar_plots_rr[1]
# wrap_plots(bar_plots_rr)


# Additional elements for RMD file ----
region_bbox <- ssr_json_export$regionBoundingBox
bbox <- c(region_bbox$lonMin, region_bbox$latMin, region_bbox$lonMax, region_bbox$latMax)

descriptionTitle <- ssr_json_export$descriptionTitle

var1_name <- ssr_json_export$dataColumns$name[1]
var1_name_index <- match(var1_name, ssr_json_csv$schema$fields$name)
var1_title <- ssr_json_csv$schema$fields$title[var1_name_index]

# remove objects not needed for RMD file ----
rm(group_vars, ssr_csv, pop, pop_total,
   ssr_csv_id, ssr_json_boxid, pal, var_name, bin_function,
   ssr_json_csv, ssr_json_export)

# Save for use in rmd file ----
save.image("ssr_pop_example.Rdata")


