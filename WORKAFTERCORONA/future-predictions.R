
library(here)
library(tidyverse)

here::here(
  'data', 'CONSECUTIVE-import-wrangle.R'
) %>% 
  source()

here::here(
  'WORKAFTERCORONA', 'CONSECUTIVE-three-year-predictions.R'
) %>% 
  source()

future <- fangraphs_stdz %>% 
  filter(Season == 2019)

# First prediction ---------------------

future <- future %>% 
  mutate(
    lag_xfip = xFIP,
    lag_fbv = FBv,
    lag_fbp = FBP,
    lag_age = Age,
    age_range = case_when(
      lag_age < 28 ~ "young",
      # between(lag_age, 28, 30) ~ "prime",
      lag_age >= 28 ~ "old"
    )
  )

future <- future %>% 
  mutate(
    predicted_fbv1 = fbv_submodel %>% predict(newdata = future),
    predicted_fbp1 = fbp_submodel %>% predict(newdata = future)
  )

future1 <- future %>% 
  filter(is.na(lag_xfip2))

future1 <- future1 %>% 
  mutate(
    predicted_xfip1 = xfip_model %>% predict(newdata = future1)
  )

future2 <- future %>% 
  filter(!is.na(lag_xfip2))

future2 <- future2 %>% 
  mutate(
    predicted_xfip1 = xfip_model2 %>% predict(newdata = future2)
  )

future <- bind_rows(future1, future2)

# Second Prediction ------------------------

future <- future %>% 
  mutate(
    lag_xfip2 = lag_xfip,
    lag_xfip = predicted_xfip1,
    lag_fbv = predicted_fbv1,
    lag_fbp = predicted_fbp1,
    lag_age = lag_age + 1,
    age_range = case_when(
      lag_age < 28 ~ "young",
      # between(lag_age, 28, 30) ~ "prime",
      lag_age >= 28 ~ "old"
    )
  )

future <- future %>% 
  mutate(
    predicted_fbv2 = fbv_submodel %>% predict(newdata = future),
    predicted_fbp2 = fbp_submodel %>% predict(newdata = future)
  )

future <- future %>% 
  mutate(
    predicted_xfip2 = xfip_model2 %>% predict(newdata = future)
  )

# Third Prediction ------------------------

future <- future %>% 
  mutate(
    lag_xfip3 = lag_xfip2,
    lag_xfip2 = lag_xfip,
    lag_xfip = predicted_xfip2,
    lag_fbv = predicted_fbv2,
    lag_fbp = predicted_fbp2,
    lag_age = lag_age + 1,
    age_range = case_when(
      lag_age < 28 ~ "young",
      # between(lag_age, 28, 30) ~ "prime",
      lag_age >= 28 ~ "old"
    )
  )

future <- future %>% 
  mutate(
    predicted_fbv3 = fbv_submodel %>% predict(newdata = future),
    predicted_fbp3 = fbp_submodel %>% predict(newdata = future)
  )

future <- future %>% 
  mutate(
    predicted_xfip3 = xfip_model3 %>% predict(newdata = future)
  )

predictions <- future %>% 
  select(
    Name, Season, predicted_xfip1, predicted_xfip2, 
    predicted_xfip3
  )

# Un-standardize xFIP ----------------

# in 2019 terms

predictions <- fangraphs_clean %>% 
  group_by(Season) %>% 
  summarise(
    sd_xfip = sqrt((var(xFIP, na.rm = TRUE) + var(lag_xfip, na.rm = TRUE) +
      var(lag_xfip2, na.rm = TRUE)) / 3),
    mean_xfip = (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
      mean(lag_xfip2, na.rm = TRUE)) / 3
  ) %>% 
  right_join(predictions) %>% 
  mutate(
    # xFIP = xFIP * sd_xfip + mean_xfip, # NOT REAL de-standardized
    predicted_xfip1 = predicted_xfip1 * sd_xfip + mean_xfip,
    predicted_xfip2 = predicted_xfip2 * sd_xfip + mean_xfip,
    predicted_xfip3 = predicted_xfip3 * sd_xfip + mean_xfip
  ) %>% 
  select(-sd_xfip, -mean_xfip)

predictions <- fangraphs_clean %>% 
  filter(Season == 2019) %>% 
  select(Name, Team, lag_xfip, Age, FBv, FBP, xFIP) %>% 
  right_join(predictions)


help(colnames)

predictions = select(predictions, !contains("Season"))
colnames(predictions) = c("Name", "Team", "2018 xFIP", "2019 Age", "2019 FBV", "2019 FBP", "2019 xFIP", "2020 xFIP (Predicted)", "2021 xFIP (Predicted)", "2022 xFIP (Predicted)")
