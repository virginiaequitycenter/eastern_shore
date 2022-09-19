library(tidyverse)
library(fs)

data_dir <- "nhgis0001_csv"
csv_files <- dir_ls(data_dir, regexp = "\\.csv$")

for (i in 1:length(csv_files)) {
  assign(str_extract(csv_files[i], "\\d{4}_county"), 
         read_csv(csv_files[i])%>% 
           filter(STATE == "Virginia" & COUNTY %in% c('Accomack', 'Northampton')) %>% 
           select(YEAR, STATE, COUNTY, total_pop = last_col()))
}

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
pop_all_yrs <- data.frame()
for (i in 1:length(dfs)) {
  x <- eval(parse(text = dfs[i]))
  pop_all_yrs <- bind_rows(pop_all_yrs, x)
}

add_2010_2020 <- data.frame(YEAR=c(2010,2010,2020,2020),
                            STATE=c('Virginia', 'Virginia','Virginia', 'Virginia'),
                            COUNTY=c('Accomack', 'Northampton','Accomack', 'Northampton'),
                            total_pop=c(33164,12389,33413,12282),
                            stringsAsFactors=FALSE)

pop_all_yrs <- pop_all_yrs %>% bind_rows(add_2010_2020)
write_csv(pop_all_yrs, file = 'eastern_shore_populations.csv')

read_csv("eastern_shore_populations.csv")

