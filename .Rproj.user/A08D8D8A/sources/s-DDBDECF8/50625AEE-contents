library(tidyverse)
library(lubridate)
library(here)

data_raw <- here::here('data', 'NEW Fangraphs.csv') %>% 
  read_csv()

data_clean <- data_raw %>% 
  mutate(
    `FB%` = as.numeric(str_remove(`FB%`, " %")),
    `SL%` = as.numeric(str_remove(`SL%`, " %")),
    `CT%` = as.numeric(str_remove(`CT%`, " %")),
    `CB%` = as.numeric(str_remove(`CB%`, " %")),
    `CH%` = as.numeric(str_remove(`CH%`, " %")),
    `SF%` = as.numeric(str_remove(`SF%`, " %")),
    `KN%` = as.numeric(str_remove(`KN%`, " %")),
    `GB%` = as.numeric(str_remove(`GB%`, " %")),
    `FB%_1` = as.numeric(str_remove(`FB%_1`, " %")),
    `K%` = as.numeric(str_remove(`K%`, " %")),
    `BB%` = as.numeric(str_remove(`BB%`, " %"))
  ) %>% 
  arrange(Name, Season) %>% 
  group_by(Name) %>% 
  mutate(experience = row_number()) %>% 
  ungroup() 

names(data_clean)[28] <- "Groundball%"
names(data_clean)[29] <- "Flyball%"
names(data_clean)[36] <- "Experience"

data_season <- select(data_clean, 1:4,36,6:7,13,5,11,14,12,15,27,34,8:10,25:26,28:29,31:32,16:24,30,35)
names(data_season)

# xFIP
season_xfip <- fangraphs_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = xFIP, 
              names_prefix = "xFIP_")

experience_xfip <- fangraphs_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = experience, 
              values_from = xFIP, 
              names_prefix = "xFIP_")

# Player Data
fangraphs_player <- bind_cols(season_xfip, season_fbv, season_fbpct,
                              experience_xfip, experience_fbv, experience_fbpct)

# Remove intermediate data
remove(season_xfip, season_fbv, season_fbpct,
       experience_xfip, experience_fbv, experience_fbpct)
