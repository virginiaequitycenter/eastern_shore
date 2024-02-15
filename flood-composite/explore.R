## Libraries ----
library(tidyverse)
library(tidycensus)
library(lehdr)


popdat_blkgrp <- read_csv("population_blkgrp.csv")
popdat_county <- read_csv("population_county.csv")


county_race <- popdat_county %>% 
  filter(demographic == "race") %>% 
  mutate(percent = estimate/totalpop)

ggplot(county_race, aes(x = reorder(variable, desc(percent)), y = percent, fill = NAME)) +
  geom_bar(stat='identity',
           # position = 'dodge'
           )+
  facet_wrap(~NAME)

county_age <- popdat_county %>% 
  filter(demographic == "age") %>% 
  mutate(percent = estimate/totalpop)

ggplot(county_age, aes(x = variable, y = percent, fill = NAME)) +
  geom_bar(stat='identity',
           # position = 'dodge'
  )+
  facet_wrap(~NAME)

county_jobs <- popdat_county %>% 
  filter(demographic == "jobs") %>% 
  mutate(percent = estimate/total_jobs)

county_jobs_w <- popdat_county %>% 
  filter(demographic == "jobs") %>% 
  mutate(percent = estimate/total_jobs) %>% 
  filter(str_detect(variable, '_w'))

ggplot(county_jobs_w, aes(x = variable, y = percent, fill = NAME)) +
  geom_bar(stat='identity',
           # position = 'dodge'
  )+
  facet_wrap(~NAME)
