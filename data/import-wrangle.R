# install.packages('tidyverse')
library(tidyverse)
# install.packages('lubridate')
library(lubridate)
# install.packages('here')
library(here)

players_raw <- here::here('data', 'Complete Fangraphs.csv') %>% 
  read_csv() %>% 
  select(playerid)

fangraphs_raw <- here::here('data', 'Full Careers.csv') %>% 
  read_csv() %>% 
  semi_join(players_raw)

ip_min <- 80

fangraphs_clean <- fangraphs_raw %>% 
  rename(Flyball_percent = `FB%_1`) %>% 
  mutate(
    `FB%` = as.numeric(str_remove(`FB%`, " %")),
    `SL%` = as.numeric(str_remove(`SL%`, " %")),
    `CT%` = as.numeric(str_remove(`CT%`, " %")),
    `CB%` = as.numeric(str_remove(`CB%`, " %")),
    `CH%` = as.numeric(str_remove(`CH%`, " %")),
    `SF%` = as.numeric(str_remove(`SF%`, " %")),
    `KN%` = as.numeric(str_remove(`KN%`, " %")),
    `GB%` = as.numeric(str_remove(`GB%`, " %")),
    `K%` = as.numeric(str_remove(`K%`, " %")),
    `BB%` = as.numeric(str_remove(`BB%`, " %")),
    Flyball_percent = as.double(str_remove(Flyball_percent, " %")),
    ipg = IP/GS
  ) %>% 
  arrange(Name, Season) %>% 
  group_by(Name) %>% 
  # Add variables for experience (numbers of years meeting minimum use) and
  # seasons (number of years in league)
  mutate(data_years = row_number(),
         seasons = Season - min(Season) + 1) %>% 
  ungroup() %>% 
  filter(IP >= ip_min) %>% 
  group_by(Name) %>% 
  mutate(experience = row_number()) %>% 
  ungroup()

fangraphs_stdz <- fangraphs_clean %>% 
  filter(Season > 2001) %>% 
  group_by(Season) %>% 
  mutate_at(
    .vars = vars(ERA, `K/9`:FIP, WAR:xFIP, `H/9`:`HR/9`, 
                 Flyball_percent, `K%`:`BB%`),
            .funs = scale
    ) %>% 
  ungroup()

# Add lag(xfip)
fangraphs_stdz <- fangraphs_stdz %>% 
  group_by(Name) %>% 
  mutate(lag_xfip = lag(xFIP)) %>% 
  ungroup()
