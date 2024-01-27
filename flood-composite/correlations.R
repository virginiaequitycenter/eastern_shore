# Setup ----
library(tidyverse)
library(readxl)
library(sf)
library(patchwork)

# Data ----
es_blkgrp <- readRDS("flood_composite_blkgrp.RDS")
blkgrp_names <- read_excel("../eastern_shore/tracts/tract_names.xlsx", sheet = "blkgrp2020")
pop <- read_csv("../eastern_shore/flood-composite/population_blkgrp.csv")


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

# Plots ----
## For workshop ----
### Percent Low Wage Workers by Workplace ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(percent_lowage_w)) %>% 
  mutate(tercile = ntile(percent_lowage_w, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# weighted mean
pop_med %>% 
  ggplot(aes(x = tercile, y = weighted_mean, fill = weighted_mean)) +
  geom_col() +
  scale_fill_gradient(low = "grey", high = "darkblue",
                      limits = range(es_blkgrp_pop$mean_HazardNumber)) +
  scale_x_discrete(labels = c("0-25%", "26%-34%", "35%-68%")) +
  scale_y_continuous(limits = c(0,8)) +
  labs(x = "Percent of Workers in Low-Wage Jobs", 
       y = "Average Flood Hazard Value across Block Groups",
       title = "Average Flood Hazard in Block Groups Where Workers Work",
       subtitle = "By percent of workers in jobs paying less than $15,000/year") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 8))

ggsave("corr_jobs_average.png")

# grouped scatterplot
# set.seed(1017)
es_blkgrp_pop %>%
  filter(!is.na(percent_lowage_w)) %>% 
  mutate(tercile = ntile(percent_lowage_w, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = jobs_total_w)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  scale_x_discrete(labels = c("0-25%", "26%-34%", "35%-68%")) +
  #coord_flip() +
  labs(x = "Percent of Workers in Low-Wage Jobs", 
       y = "Average Flood Hazard Value in Block Groups",
       title = "Flood Hazard in Block Groups Where Workers Work",
       subtitle = "By percent of workers in jobs paying less than $15,000/year") +
  annotate("text", x = .6, y = 6, label = "Block\ngroup", size = 3) +
  annotate("curve", x = .6, y = 6.5, xend = 0.9, yend = 8, 
           color = "gray30", curvature = -0.4, arrow = arrow(length = unit(0.25, 'cm'))) +
  annotate("curve", x = .6, y = 5.25, xend = 0.9, yend = 4.5, 
           color = "gray30", curvature = 0.4, arrow = arrow(length = unit(0.25, 'cm'))) +
  annotate("text", x = 2.5, y = 4, label = "Average\nacross\nblock\ngroups", size = 3) +
  annotate("curve", x = 2.65, y = 3.5, xend = 2.9, yend = 4, 
           color = "gray30", curvature = 0.3, arrow = arrow(length = unit(0.25, 'cm'))) +
  annotate("curve", x = 2.35, y = 4, xend = 2.1, yend = 3.5, 
           color = "gray30", curvature = 0.3, arrow = arrow(length = unit(0.25, 'cm'))) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title = element_text(size = 8))

ggsave("corr_jobs_annotated.png")

### Jobs paying less than 15K ----
ggplot(es_blkgrp_pop) +
  geom_sf(aes(fill = percent_lowage_w), color = "white") +
  scale_fill_gradient(low = "grey", high = "darkblue", 
                      name = "Percent\nof Workers\nin Low-wage\nJobs") +
  theme_void() +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = c(.8, .4))

ggsave("blockgroup_map_jobs.png")

## Everything ----

### white ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(whiteper_est)) %>% 
  mutate(tercile = ntile(whiteper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(whiteper_est)) %>% 
  mutate(tercile = ntile(whiteper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Residents who are White", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = whiteper_est, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Residents who are White", y = "Average Flood Hazard Composite") +
  theme_minimal()

### black ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(blackper_est)) %>% 
  mutate(tercile = ntile(blackper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(blackper_est)) %>% 
  mutate(tercile = ntile(blackper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Residents who are Black", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = blackper_est, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Residents who are Black", y = "Average Flood Hazard Composite") +
  theme_minimal()

### latinx ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(ltnxper_est)) %>% 
  mutate(tercile = ntile(ltnxper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(ltnxper_est)) %>% 
  mutate(tercile = ntile(ltnxper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Residents who are LatinX", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = ltnxper_est, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Residents who are LatinX", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Age 17 and under ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(age17per_est)) %>% 
  mutate(tercile = ntile(age17per_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(age17per_est)) %>% 
  mutate(tercile = ntile(age17per_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Residents who are 17 and Under", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = age17per_est, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Residents who are 17 and Under", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Age 65 and over ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(age65per_est)) %>% 
  mutate(tercile = ntile(age65per_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(age65per_est)) %>% 
  mutate(tercile = ntile(age65per_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Residents who are 65 and Over", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = age65per_est, y = mean_HazardNumber, color = mean_HazardNumber, size = totpop_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Residents who are 65 and Over", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Online ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(onlineper_est)) %>% 
  mutate(tercile = ntile(onlineper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(onlineper_est)) %>% 
  mutate(tercile = ntile(onlineper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Households with Broadband Access", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = onlineper_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Households with Broadband Access", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Homeowners ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(ownhhper_est)) %>% 
  mutate(tercile = ntile(ownhhper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(ownhhper_est)) %>% 
  mutate(tercile = ntile(ownhhper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Home-Owning Households", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = ownhhper_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Home-Owning Households", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Renters ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(renthhper_est)) %>% 
  mutate(tercile = ntile(renthhper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(renthhper_est)) %>% 
  mutate(tercile = ntile(renthhper_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Renting Households", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = renthhper_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Renting Households", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Building age ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(bldgage70per_est)) %>% 
  mutate(tercile = ntile(bldgage70per_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(bldgage70per_est)) %>% 
  mutate(tercile = ntile(bldgage70per_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Homes Built before 1970", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = bldgage70per_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Homes Built before 1970", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Median HH Income ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(medhhinc_est)) %>% 
  mutate(tercile = ntile(medhhinc_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(medhhinc_est)) %>% 
  mutate(tercile = ntile(medhhinc_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Median Household Income", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = medhhinc_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Median Household Income", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Median Gross Rents ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(medrent_est)) %>% 
  mutate(tercile = ntile(medrent_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(medrent_est)) %>% 
  mutate(tercile = ntile(medrent_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Median Gross Rent", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = medrent_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Median Gross Rent", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Median Home Value ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(medhome_est)) %>% 
  mutate(tercile = ntile(medhome_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(medhome_est)) %>% 
  mutate(tercile = ntile(medhome_est, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Median Home Value", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = medhome_est, y = mean_HazardNumber, color = mean_HazardNumber, size = tothh_est)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Median Home Value", y = "Average Flood Hazard Composite") +
  theme_minimal()

### Percent Low Wage Workers by Workplace ----
es_blkgrp_pop %>%
  filter(!is.na(percent_lowage_w)) %>% 
  mutate(tercile = ntile(percent_lowage_w, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(percent_lowage_w)) %>% 
  mutate(tercile = ntile(percent_lowage_w, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = jobs_total_w)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Low-Wage Jobs in Area", 
       y = "Average Flood Hazard Composite",
       title = "Flood Hazard by Percent of Low-Wage Jobs",
       subtitle = "By Location of Workplace") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = percent_lowage_w, y = mean_HazardNumber, color = mean_HazardNumber, size = jobs_total_w)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE, color = "grey50", linetype = 1) +
  geom_vline(xintercept = pop_med$max - 1, color = "black", linetype = 3) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Low-Wage Jobs by Workplace of Worker", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


### Percent Low Wage Workers by Residence ----
pop_med <- es_blkgrp_pop %>%
  filter(!is.na(percent_lowage_h)) %>% 
  mutate(tercile = ntile(percent_lowage_h, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  group_by(tercile) %>% 
  summarize(m = median(mean_HazardNumber),
            mean = mean(mean_HazardNumber),
            weighted_mean = weighted.mean(mean_HazardNumber, cells), 
            min = min(percent_lowage_w),
            max = max(percent_lowage_w))

# grouped scatterplot
es_blkgrp_pop %>%
  filter(!is.na(percent_lowage_h)) %>% 
  mutate(tercile = ntile(percent_lowage_h, 3),
         tercile = factor(tercile, labels = c("Lowest third", "Middle third", "Highest third"))) %>% 
  ggplot(aes(x = tercile, y = mean_HazardNumber, color = mean_HazardNumber, size = jobs_total_h)) + 
  geom_jitter(height = 0, width = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_point(data = pop_med, aes(y = weighted_mean), size = 4, shape = 8, color = "red") +
  #coord_flip() +
  labs(x = "Percent of Low-Wage Jobs by Residence of Worker", y = "Average Flood Hazard Composite") +
  theme_minimal() +
  theme(legend.position = "none")

# scatterplot with lowess
ggplot(es_blkgrp_pop, aes(x = percent_lowage_h, y = mean_HazardNumber, color = mean_HazardNumber, size = jobs_total_h)) +
  geom_point() +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_smooth(se = FALSE) +
  guides(color = "none", size = "none") +
  labs(x = "Percent of Low-Wage Jobs by Residence of Worker", y = "Average Flood Hazard Composite") +
  theme_minimal()

