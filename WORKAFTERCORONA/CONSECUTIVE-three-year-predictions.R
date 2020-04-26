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
      lag_age < 25 ~ "young",
      between(lag_age, 25, 31) ~ "prime",
      lag_age >= 32 ~ "old"
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

xfip_model2 <- one_year %>% 
  lm(
    formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip +
      lag_xfip2
  )

xfip_model3 <- one_year %>% 
  lm(
    formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip +
      lag_xfip2 + lag_xfip3
  )

xfip_model %>% 
  summary()

one_year1 <- one_year %>% 
  filter(is.na(lag_xfip2))

one_year1 <- one_year1 %>% 
  mutate(
    predicted_xfip1 = xfip_model %>% predict(newdata = one_year1),
    # 95% confidence interval
    lower_xfip1 = (xfip_model %>% predict(newdata = one_year1,
                                          interval = "confidence"))[, 2],
    upper_xfip1 = (xfip_model %>% predict(newdata = one_year1,
                                          interval = "confidence"))[, 3]
  )

one_year2 <- one_year %>% 
  filter(!is.na(lag_xfip2))

one_year2 %>% 
  mutate(
    predicted_xfip1 = xfip_model2 %>% predict(newdata = one_year2),
    # 95% confidence interval
    lower_xfip1 = (xfip_model2 %>% predict(newdata = one_year2,
                                           interval = "confidence"))[, 2],
    upper_xfip1 = (xfip_model2 %>% predict(newdata = one_year2,
                                           interval = "confidence"))[, 3]
  )

one_year <- bind_rows(one_year1, one_year2)

# Two year predictions ----------------------

two_year <- one_year %>% 
  mutate(
    lag_age = lag_age + 1, # should be Age?
    lag_fbv = predicted_fbv1,
    lag_fbp = predicted_fbp1,
    age_range = case_when(
      lag_age < 25 ~ "young",
      between(lag_age, 25, 31) ~ "prime",
      lag_age >= 32 ~ "old"
    ),
    lag_xfip2 = lag_xfip,
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
    predicted_xfip2 = xfip_model2 %>% predict(newdata = two_year),
    # 95% confidence interval
    lower_xfip2 = (xfip_model2 %>% predict(newdata = two_year,
                                          interval = "confidence"))[, 2],
    upper_xfip2 = (xfip_model2 %>% predict(newdata = two_year,
                                          interval = "confidence"))[, 3]
  )

# Three year predictions ----------------------

three_year <- two_year %>% 
  mutate(
    lag_age = lag_age + 1, # should be lead_age?
    lag_fbv = predicted_fbv1,
    lag_fbp = predicted_fbp1,
    age_range = case_when(
      lag_age < 25 ~ "young",
      between(lag_age, 25, 31) ~ "prime",
      lag_age >= 32 ~ "old"
    ),
    lag_xfip3 = lag_xfip2,
    lag_xfip2 = lag_xfip,
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
    predicted_xfip3 = xfip_model3 %>% predict(newdata = three_year),
    # 95% confidence interval
    lower_xfip3 = (xfip_model3 %>% predict(newdata = three_year,
                                          interval = "confidence"))[, 2],
    upper_xfip3 = (xfip_model3 %>% predict(newdata = three_year,
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
  mutate(Season = Season + 1) %>% 
  group_by(Season) %>% 
  summarise(
    # mean_xfip = mean(xFIP),
    # sd_xfip = sd(xFIP)
    sd_xfip = sqrt((var(xFIP, na.rm = TRUE) + var(lag_xfip, na.rm = TRUE) +
      var(lag_xfip2, na.rm = TRUE)) / 3),
    mean_xfip = (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
      mean(lag_xfip2, na.rm = TRUE)) / 3
  ) %>% 
  ungroup() %>% 
  right_join(predictions) %>%
  mutate(
    predicted_xfip1 = predicted_xfip1 * sd_xfip + mean_xfip,
    lower_xfip1 = lower_xfip1 * sd_xfip + mean_xfip,
    upper_xfip1 = upper_xfip1 * sd_xfip + mean_xfip
  ) %>% 
  select(-xFIP)

predictions1 <- fangraphs_clean %>% 
  select(Name, Season, xFIP) %>% 
  right_join(predictions1) %>% 
  select(
    Name,
    Season, xFIP, predicted_xfip1, lower_xfip1, upper_xfip1
  )

predictions2 <- fangraphs_clean %>% 
  mutate(Season = Season + 1) %>% 
  group_by(Season) %>% 
  summarise(
    # mean_xfip = mean(xFIP),
    # sd_xfip = sd(xFIP)
    sd_xfip = sqrt((var(xFIP, na.rm = TRUE) + var(lag_xfip, na.rm = TRUE) +
      var(lag_xfip2, na.rm = TRUE)) / 3),
    mean_xfip = (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
      mean(lag_xfip2, na.rm = TRUE)) / 3
  ) %>% 
  ungroup() %>% 
  right_join(predictions) %>% 
  mutate(
    predicted_xfip2 = predicted_xfip2 * sd_xfip + mean_xfip,
    lower_xfip2 = lower_xfip2 * sd_xfip + mean_xfip,
    upper_xfip2 = upper_xfip2 * sd_xfip + mean_xfip
  ) %>% 
  select(-xFIP2)

predictions2 <- fangraphs_clean %>% 
  select(Name, Season, xFIP2) %>% 
  right_join(predictions2) %>% 
  select(
    Name,
    Season, xFIP2, predicted_xfip2, lower_xfip2, upper_xfip2
  )

predictions3 <- fangraphs_clean %>% 
  mutate(Season = Season + 1) %>% 
  group_by(Season) %>% 
  summarise(
    # mean_xfip = mean(xFIP),
    # sd_xfip = sd(xFIP)
    sd_xfip = sqrt((var(xFIP, na.rm = TRUE) + var(lag_xfip, na.rm = TRUE) +
      var(lag_xfip2, na.rm = TRUE)) / 3),
    mean_xfip = (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
      mean(lag_xfip2, na.rm = TRUE)) / 3
  ) %>% 
  ungroup() %>% 
  right_join(predictions) %>% 
  mutate(
    predicted_xfip3 = predicted_xfip3 * sd_xfip + mean_xfip,
    lower_xfip3 = lower_xfip3 * sd_xfip + mean_xfip,
    upper_xfip3 = upper_xfip3 * sd_xfip + mean_xfip
  ) %>% 
  select(-xFIP3)

predictions3 <- fangraphs_clean %>% 
  select(Name, Season, xFIP3) %>% 
  right_join(predictions3) %>% 
  select(
    Name,
    Season, xFIP3, predicted_xfip3, lower_xfip3, upper_xfip3
  )

predictions <- predictions1 %>% 
  left_join(predictions2) %>% 
  left_join(predictions3)

RMSE <- function(predicted, actual){
  sqrt(mean((predicted - actual)^2, na.rm = TRUE))
}

MeanError<- function(predicted, actual){
  mean(abs(predicted - actual), na.rm = TRUE)
}

RMSE(predictions$xFIP, predictions$predicted_xfip1)
RMSE(predictions$xFIP2, predictions$predicted_xfip2)
RMSE(predictions$xFIP3, predictions$predicted_xfip3)

Year1ME = MeanError(predictions$xFIP, predictions$predicted_xfip1)
Year2ME = MeanError(predictions$xFIP2, predictions$predicted_xfip2)
Year3ME = MeanError(predictions$xFIP3, predictions$predicted_xfip3)

TrainError <- data.frame(c(Year1ME, Year2ME, Year3ME))

TestError <- data.frame(c(0.3398959, 0.4423486, 0.4583183)) 

rownames(TrainError) = c("Year1ME", "Year2ME", "Year3ME")

t(TrainError)
