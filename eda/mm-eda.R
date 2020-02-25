library(tidyverse)

here::here('data', '01-import-wrangle.R') %>% 
  source()

fangraphs_clean %>% 
  group_by(playerid) %>% 
  mutate(d = cumsum(seasons - experience)) %>% 
  ungroup() %>% view()
