
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
         avg_gs = mean(GS, na.rm = TRUE),
         mx_experience = max(experience)
         ) %>% 
  ungroup() -> avgs

avgs %>% 
  filter(avg_ip >= 80, IP > 80, mx_experience > 5) %>% 
  as_tsibble(key = Name, index = experience) %>%
  ggplot() +
  geom_line(aes(experience, xFIP, group = Name, color = avg_fbpctg > 60)) +
  facet_strata(n_strata = 6, along = FBv) +
  labs(color = "FB Pitcher", title = "IP > 80, avg IP > 80, avg experience > 5")

avgs %>% 
  filter(mx_experience > 4) %>% 
  as_tsibble(key = Name, index = experience) %>%
  ggplot() +
  geom_line(aes(experience, WAR, group = Name, 
                color = avg_ip > 150 & avg_gs > 20 & avg_fbv > 90 & avg_fbpctg > 50)) +
  facet_sample(n_per_facet = 30, n_facets = 4) +
  labs(color = "")
  # facet_wrap(~(avg_fbpctg > 60))

fangraphs_clean %>% 
  filter(experience > 12) %>% 
  distinct(Name) -> seasoned_vets

fangraphs_clean %>% 
  filter(Name == sample(seasoned_vets$Name, 1)) %>% 
  ggplot() +
  geom_line(aes(experience, WAR))

lm(
  data = avgs,
  xFIP ~ lag(FBv) +
    lag(xFIP) +
    lag(`FB%`) +
    lag(IP) + 
    lag(GS)
    ) %>% 
  summary()
# Higher FB%, lower WAR
# Higher FBV, higher WAR -- large significant effect
# More Experience, lower WAR
# Higher IP, Higher WAR
# Higher GS, Lower WAR -- large significant effect

stdz <- fangraphs_clean %>% 
  group_by(Season, age) %>% 
  mutate_at(.vars = c(4, 7:26),
            .funs = scale) %>% 
  ungroup()

stdz %>% 
  filter(experience > 7) %>% 
  distinct(Name) -> seasoned_vets

stdz %>% 
  filter(Name == sample(seasoned_vets$Name, 1)) %>% 
  ggplot() +
  geom_line(aes(experience, WAR)) +
  geom_bar(aes(experience, FBv), stat = "identity", 
           fill = "transparent", color = "red")



