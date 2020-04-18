
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

here::here(
  'shiny-xfip', 'url format.R'
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
    # sd_xfip = sd(xFIP),
    # mean_xfip = mean(xFIP)
  ) %>% 
  right_join(predictions) %>% 
  mutate(
    # xFIP = xFIP * sd_xfip + mean_xfip, # NOT REAL de-standardized
    predicted_xfip1 = round(predicted_xfip1 * sd_xfip + mean_xfip, 2),
    predicted_xfip2 = round(predicted_xfip2 * sd_xfip + mean_xfip, 2),
    predicted_xfip3 = round(predicted_xfip3 * sd_xfip + mean_xfip, 2)
  ) %>% 
  select(-sd_xfip, -mean_xfip)

predictions <- fangraphs_clean %>% 
  filter(Season == 2019) %>% 
  select(Name, Team, lag_xfip, Age, FBv, FBP, xFIP) %>% 
  right_join(predictions)

predictions <-  predictions %>% 
  select(
    Name, Team,
    "2018 xFIP" = lag_xfip,
    "2019 Age" = Age,
    "2019 FBV" = FBv,
    "2019 FBP" = FBP,
    "2019 xFIP" = xFIP,
    "2020 xFIP (Predicted)" = predicted_xfip1,
    "2021 xFIP (Predicted)" = predicted_xfip2,
    "2022 xFIP (Predicted)" = predicted_xfip3
  ) %>% 
  mutate(
    "2019 FBP" = str_c(`2019 FBP`, " %"),
    "2019 FBV" = str_c(`2019 FBV`, " mph")
  )

## Have access without URLs
predictionsURLS <- merge(predictions, mlbdata, by = "Name")



## Manually update names
predictions[14, 2] <- "Brewers"
predictions[73,2] <- "- - -"
predictions[75,2] <- "Blue Jays"
predictions[79,2] <- "Braves"
predictions[80,2] <- "White Sox"
predictions[21,2] <- "Dodgers"
predictions[23,2] <- "Giants"
predictions[82,2] <- "Angels"
predictions[24,2] <- "Brewers"
predictions[85,2] <- "Yankees"
predictions[86,2] <- "White Sox"
predictions[87,2] <- "Twins"
predictions[88,2] <- "Dodgers"
predictions[89,2] <- "Tigers"
predictions[97,2] <- "Twins"
predictions[33,2] <- "Rangers"
predictions[103,2] <- "Angels"
predictions[105,2] <- "Twins"
predictions[106,2] <- "Giants"
predictions[108,2] <- "Rangers"
predictions[112,2] <- "Diamondbacks"
predictions[113,2] <- "Mets"
predictions[37,2] <- "Red Sox"
predictions[117,2] <- "Mets"
predictions[121,2] <- "Diamondbacks"
predictions[123,2] <- "Mets"
predictions[127,2] <- "Blue Jays"
predictions[128,2] <- "Reds"
predictions[57,2] <- "Rays"
predictions[130,2] <- "Reds"
predictions[65,2] <- "Diamondbacks"
predictions[66,2] <- "Padres"
predictions[131,2] <- "Astros"
predictions[132,2] <- "Phillies"

## league average is only the 2019 value for now!
leagueAverageXFIP = mean(predictions$`2019 xFIP`, na.rm = TRUE)
