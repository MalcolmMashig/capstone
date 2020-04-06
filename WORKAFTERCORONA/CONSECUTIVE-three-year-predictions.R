library(here)
library(tidyverse)

here::here(
  'data', 'CONSECUTIVE-import-wrangle.R'
) %>% 
  source()

# One year predictions ----------------------

one_year <- fangraphs_stdz %>% 
  count(Name, consecutive_span) %>% 
  filter(n > 2) %>%
  select(Name, consecutive_span) %>% 
  inner_join(fangraphs_stdz) %>% 
  filter(!is.na(lag_xfip)) %>% 
  mutate(
    age_range = case_when(
      lag_age < 28 ~ "young",
      # between(lag_age, 28, 30) ~ "prime",
      lag_age >= 28 ~ "old"
    )
  )

# fbv submodel
fbv_submodel <- one_year %>% 
  lm(
    formula = FBv ~ age_range*lag_age + lag_fbv
  )

fbv_submodel %>% 
  summary()

one_year <- one_year %>% 
  mutate(
    predicted_fbv1 = fbv_submodel %>% predict()
  )

# fbp submodel
fbp_submodel <- one_year %>% 
  lm(
    formula = FBP ~ age_range*lag_age + lag_fbp
  )

fbp_submodel %>% 
  summary()

one_year <- one_year %>% 
  mutate(
    predicted_fbp1 = fbp_submodel %>% predict()
  )

# xfip model
xfip_model <- one_year %>% 
  lm(
    formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip
  ) 

xfip_model %>% 
  summary()

one_year <- one_year %>% 
  mutate(
    predicted_xfip1 = xfip_model %>% predict(),
    # 95% confidence interval
    lower_xfip1 = (xfip_model %>% predict(interval = "confidence"))[, 2],
    upper_xfip1 = (xfip_model %>% predict(interval = "confidence"))[, 3]
  ) # predicting xFIP in the row!!:)

# Two year predictions ----------------------

two_year <- one_year %>% 
  mutate(
    lag_age = lag_age + 1, # should be Age?
    lag_fbv = predicted_fbv1,
    lag_fbp = predicted_fbp1,
    age_range = case_when(
      lag_age < 28 ~ "young", 
      # between(lag_age, 28, 30) ~ "prime",
      lag_age >= 28 ~ "old"
    ),
    lag_xfip = predicted_xfip1
  )

two_year <- two_year %>% 
  mutate(
    predicted_fbv1 = fbv_submodel %>% predict(newdata = two_year),
    predicted_fbp1 = fbp_submodel %>% predict(newdata = two_year)
  )

# needs to be separate from above
two_year <- two_year %>% 
  mutate(
    predicted_xfip2 = xfip_model %>% predict(newdata = two_year),
    # 95% confidence interval
    lower_xfip2 = (xfip_model %>% predict(newdata = two_year,
                                          interval = "confidence"))[, 2],
    upper_xfip2 = (xfip_model %>% predict(newdata = two_year,
                                          interval = "confidence"))[, 3]
  )

# Three year predictions ----------------------

three_year <- two_year %>% 
  mutate(
    lag_age = lag_age + 1, # should be lead_age?
    lag_fbv = predicted_fbv1,
    lag_fbp = predicted_fbp1,
    age_range = case_when(
      lag_age < 28 ~ "young",
      # between(lag_age, 28, 30) ~ "prime",
      lag_age >= 28 ~ "old"
    ),
    lag_xfip = predicted_xfip2
  )

three_year <- three_year %>% 
  mutate(
    predicted_fbv1 = fbv_submodel %>% predict(newdata = three_year),
    predicted_fbp1 = fbp_submodel %>% predict(newdata = three_year)
  )

# needs to be separate from above
three_year <- three_year %>% 
  mutate(
    predicted_xfip3 = xfip_model %>% predict(newdata = three_year),
    # 95% confidence interval
    lower_xfip3 = (xfip_model %>% predict(newdata = three_year,
                                          interval = "confidence"))[, 2],
    upper_xfip3 = (xfip_model %>% predict(newdata = three_year,
                                          interval = "confidence"))[, 3]
  )

# Analysis of predictions -----------------

predictions <- three_year %>% 
  group_by(Name, consecutive_span) %>% 
  mutate(
    xFIP2 = lead(xFIP),
    xFIP3 = lead(xFIP, n = 2),
    Season2 = lead(Season),
    Season3 = lead(Season, n = 2)
  ) %>% 
  ungroup() %>% 
  select(
    Name, 
    Season, xFIP, predicted_xfip1, lower_xfip1, upper_xfip1,
    Season2, xFIP2, predicted_xfip2, lower_xfip2, upper_xfip2,
    Season3, xFIP3, predicted_xfip3, lower_xfip3, upper_xfip3
  )

# De-standardize xFIP -----------------

predictions1 <- fangraphs_clean %>% 
  group_by(Season) %>% 
  summarise(
    mean_xfip = mean(xFIP),
    sd_xfip = sd(xFIP)
  ) %>% 
  right_join(predictions) %>% 
  mutate(
    xFIP = xFIP * sd_xfip + mean_xfip,
    predicted_xfip1 = predicted_xfip1 * sd_xfip + mean_xfip,
    lower_xfip1 = lower_xfip1 * sd_xfip + mean_xfip,
    upper_xfip1 = upper_xfip1 * sd_xfip + mean_xfip
  ) %>% 
  select(
    Name,
    Season, xFIP, predicted_xfip1, lower_xfip1, upper_xfip1
  )

predictions2 <- fangraphs_clean %>% 
  rename(Season2 = Season) %>% 
  group_by(Season2) %>% 
  summarise(
    mean_xfip = mean(xFIP),
    sd_xfip = sd(xFIP)
  ) %>% 
  right_join(predictions) %>% 
  mutate(
    xFIP2 = xFIP2 * sd_xfip + mean_xfip,
    predicted_xfip2 = predicted_xfip2 * sd_xfip + mean_xfip,
    lower_xfip2 = lower_xfip2 * sd_xfip + mean_xfip,
    upper_xfip2 = upper_xfip2 * sd_xfip + mean_xfip
  ) %>% 
  select(
    Season2, xFIP2, predicted_xfip2, lower_xfip2, upper_xfip2
  )

predictions3 <- fangraphs_clean %>% 
  rename(Season3 = Season) %>% 
  group_by(Season3) %>% 
  summarise(
    mean_xfip = mean(xFIP),
    sd_xfip = sd(xFIP)
  ) %>% 
  right_join(predictions) %>% 
  mutate(
    xFIP3 = xFIP3 * sd_xfip + mean_xfip,
    predicted_xfip3 = predicted_xfip3 * sd_xfip + mean_xfip,
    lower_xfip3 = lower_xfip3 * sd_xfip + mean_xfip,
    upper_xfip3 = upper_xfip3 * sd_xfip + mean_xfip
  ) %>% 
  select(
    Season3, xFIP3, predicted_xfip3, lower_xfip3, upper_xfip3
  )

predictions <- cbind(predictions1, predictions2, predictions3)
