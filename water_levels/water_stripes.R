# Extreme water level stripes
# data from: https://tidesandcurrents.noaa.gov/est/est_station.shtml?stnid=8631044
# wachapreague, extreme water levels, monthly maximum

# Libraries ---- 
library(tidyverse)
library(janitor)
library(RColorBrewer)

# Data ---- 
wl <- read_csv("8631044_EWL_MHHW.csv", skip = 4) %>% 
  clean_names()

# Prep data ---- 
wl <- wl %>% 
  mutate(date_chr = paste0(year, "-", month, "-", 1),
         date = ymd(date_chr))

wl_year <- wl %>% 
  group_by(year) %>% 
  summarize(max = max(monthly_maxima_mhhw_meters))

# Stripe theme/palette ---- 
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major=element_blank(),
        legend.title = element_blank(),
        axis.text.x=element_text(vjust=3),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=14,face="bold")
  )

col_strip <- brewer.pal(11,"RdBu")

# Stripes ---- 
# monthly 
ggplot(wl,
       aes(x=date,y=1,fill=monthly_maxima_mhhw_meters)) +
  geom_tile() +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y",
               expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors=rev(col_strip))+
  guides(fill=guide_colorbar(barwidth = 1))+
  labs(title="Wachapreague Station 8631044: Monthly Extreme Water Levels (m)",
       caption="Data: NOAA Tides and Currents (https://tidesandcurrents.noaa.gov/est/est_station.shtml?stnid=8631044)")+
  theme_strip

ggsave("water_level_stripes_monthly.png")

# annual max
ggplot(wl_year,
       aes(x=year,y=1,fill=max)) +
  geom_tile() +
  scale_x_continuous(breaks = seq(1978,2023, 2),
               expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors=rev(col_strip))+
  guides(fill=guide_colorbar(barwidth = 1))+
  labs(title="Wachapreague Station 8631044: Annual Extreme Water Levels (m)",
       caption="Data: NOAA Tides and Currents (https://tidesandcurrents.noaa.gov/est/est_station.shtml?stnid=8631044)")+
  theme_strip

ggsave("water_level_stripes_yearly.png")
