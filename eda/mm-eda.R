source('01-import-wrangle.R')

fangraphs_data %>%
  distinct(Name) %>%
  nrow()
fangraphs_data %>%
  group_by(Name) %>%
  summarise(count = n()) %>% arrange(desc(count)) %>% view()

fangraphs_data %>%
  group_by(Name) %>%
  summarise(count = n()) %>% arrange(desc(count)) %>%
  group_by(count) %>%
  summarise(n())

fangraphs_data %>%
  distinct(Season) %>%
  nrow()

fangraphs_data %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_histogram(aes(count), bins = 18)

