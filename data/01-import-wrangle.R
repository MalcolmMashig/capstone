# install.packages('tidyverse')
library(tidyverse)

# install.packages('lubridate')
library(lubridate)

# install.packages('here')
library(here)

fangraphs_raw <- here::here('data', 'fangraphs.csv') %>% 
  read_csv()

fangraphs_clean <- fangraphs_raw %>% 
  mutate(
   `FB%` = as.numeric(str_remove(`FB%`, " %")),
   `SL%` = as.numeric(str_remove(`SL%`, " %")),
   `CT%` = as.numeric(str_remove(`CT%`, " %")),
   `CB%` = as.numeric(str_remove(`CB%`, " %")),
   `CH%` = as.numeric(str_remove(`CH%`, " %")),
   `SF%` = as.numeric(str_remove(`SF%`, " %")),
   `KN%` = as.numeric(str_remove(`KN%`, " %"))
  ) %>% 
  arrange(Name, Season) %>% 
  group_by(Name) %>% 
  # Add variables for experience (numbers of years meeting minimum use) and
  # seasons (number of years in league)
  mutate(experience = row_number(),
         seasons = Season - min(Season) + 1) %>% 
  ungroup() %>% 
  select(1:4, 26:27, 5:25)

# 1 row/player ----------------------------

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

# FBv

season_fbv <- fangraphs_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FBv, 
              names_prefix = "FBv_")

experience_fbv <- fangraphs_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = experience, 
              values_from = FBv, 
              names_prefix = "FBv_")

# FB%

season_fbpct <- fangraphs_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

experience_fbpct <- fangraphs_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = experience, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

# Player Data
fangraphs_player <- bind_cols(season_xfip, season_fbv, season_fbpct,
                              experience_xfip, experience_fbv, experience_fbpct)

# Remove intermediate data
remove(season_xfip, season_fbv, season_fbpct,
       experience_xfip, experience_fbv, experience_fbpct)

  
  
  
  
