
library(tidyverse)
# install.packages("here")
library(here)
# install.packages("remotes")
# remotes::install_github("njtierney/brolgar")
library(brolgar)

here::here('data', '01-import-wrangle.R') %>% 
  source()

# Use as a function of WAR over time

fangraphs_clean %>% 
  group_by(Name) %>% 
  mutate(avg_fbv = mean(FBv, na.rm = TRUE),
         avg_fbpctg = mean(`FB%`, na.rm = TRUE),
         avg_ip = mean(IP, na.rm = TRUE),
         avg_gs = mean(GS, na.rm = TRUE)
         ) %>% 
  ungroup() %>% 
  filter(experience > 4, Season > 2010) %>% 
  as_tsibble(key = Name, index = experience) %>%
  ggplot() +
  geom_line(aes(experience, WAR, group = Name, 
                color = avg_ip > 160 & avg_gs > 20 & avg_fbv > 90 & avg_fbpctg > 55)) +
  facet_sample(n_per_facet = 30, n_facets = 4) +
  labs(color = "")
  # facet_wrap(~(avg_fbpctg > 60))


  