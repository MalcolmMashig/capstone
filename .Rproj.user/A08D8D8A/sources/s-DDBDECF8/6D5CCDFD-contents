# install.packages('tidyverse')
library(tidyverse)

#install.packages('lubridate')
library(lubridate)

#install.packages('here')
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
  )

fangraphs_stdz <- fangraphs_clean %>%
  mutate_at(vars(Age:`KN%`), scale)

colnames(fangraphs_stdz)[4:24] <- sapply(
  colnames(fangraphs_stdz %>% select(Age:`KN%`)),
  str_c,
  "_stdz"
  )

fangraphs_data <- fangraphs_clean %>%
  left_join(fangraphs_stdz)

