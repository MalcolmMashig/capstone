# install.packages('tidyverse')
library(tidyverse)

#install.packages('lubridate')
library(lubridate)

#install.packages('here')
library(here)

fangraphs_raw <- here::here('data', 'fangraphs.csv') %>% 
  read_csv()
