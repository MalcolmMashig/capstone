
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
    lag_xfip2 = lag_xfip,
    lag_xfip = xFIP,
    lag_fbv = FBv,
    lag_fbp = FBP,
    lag_age = Age,
    age_range = case_when(
      lag_age < 25 ~ "young",
      between(lag_age, 25, 31) ~ "prime",
      lag_age >= 32 ~ "old"
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
      lag_age < 25 ~ "young",
      between(lag_age, 25, 31) ~ "prime",
      lag_age >= 32 ~ "old"
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
      lag_age < 25 ~ "young",
      between(lag_age, 25, 31) ~ "prime",
      lag_age >= 32 ~ "old"
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
    "2019 FBV" = str_c(`2019 FBV`, " mph"),
    Team = ifelse(Team == "- - -", "Free Agent", Team)
  )

here::here(
  'shiny-xfip', 'url format.R'
) %>% 
  source()

## Have access without URLs
predictionsURLS <- merge(predictions, mlbdata, by = "Name")


# MIGHT NEED THIS
# #fangraphs_cleanALL <- merge(fangraphs_cleanALL, mlbdata, on = "Name")
# 
# fangraphs_cleanALL <- fangraphs_cleanALL%>%
#   filter(
#     playerid %in% list(fangraphs_clean$playerid)
#   )


## Manually update names

# FORMAT...
predictions[which(predictions$Name == "player"), 2] <- "new team"

predictions[which(predictions$Name == "Gerrit Cole"), 2] <- "Yankees"
predictions[which(predictions$Name == "Julio Teheran"), 2] <- "Angels"
predictions[which(predictions$Name == "Madison Bumgarner"), 2] <- "Diamondbacks"
predictions[which(predictions$Name == "Cole Hamels"), 2] <- "Braves"
predictions[which(predictions$Name == "Zack Wheeler"), 2] <- "Phillies"
predictions[which(predictions$Name == "Kenta Maeda"), 2] <- "Twins"
predictions[which(predictions$Name == "Rick Porcello"), 2] <- "Mets"
predictions[which(predictions$Name == "David Price"), 2] <- "Dodgers"
predictions[which(predictions$Name == "Drew Smyly"), 2] <- "Giants"
predictions[which(predictions$Name == "Kevin Gausman"), 2] <- "Giants"
predictions[which(predictions$Name == "Brett Anderson"), 2] <- "Brewers"
predictions[which(predictions$Name == "CC Sabathia"), 2] <- "Free Agent"
predictions[which(predictions$Name == "Chase Anderson"), 2] <- "Blue Jays"
predictions[which(predictions$Name == "Dallas Keuchel"), 2] <- "White Sox"
predictions[which(predictions$Name == "Dylan Bundy"), 2] <- "Angels"
predictions[which(predictions$Name == "Gio Gonzalez"), 2] <- "White Sox"
predictions[which(predictions$Name == "Homer Bailey"), 2] <- "Twins"
predictions[which(predictions$Name == "Hyun-Jin Ryu"), 2] <- "Blue Jays"
predictions[which(predictions$Name == "Ivan Nova"), 2] <- "Tigers"
predictions[which(predictions$Name == "Jhoulys Chacin"), 2] <- "Twins"
predictions[which(predictions$Name == "Jordan Lyles"), 2] <- "Rangers"
predictions[which(predictions$Name == "Kyle Gibson"), 2] <- "Rangers"
predictions[which(predictions$Name == "Marcus Stroman"), 2] <- "Mets"
predictions[which(predictions$Name == "Martin Perez"), 2] <- "Red Sox"
predictions[which(predictions$Name == "Michael Wacha"), 2] <- "Mets"
predictions[which(predictions$Name == "Mike Leake"), 2] <- "Diamondbacks"
predictions[which(predictions$Name == "Tanner Roark"), 2] <- "Blue Jays"
predictions[which(predictions$Name == "Trevor Bauer"), 2] <- "Reds"
predictions[which(predictions$Name == "Trevor Richards"), 2] <- "Rays"
predictions[which(predictions$Name == "Wade Miley"), 2] <- "Reds"
predictions[which(predictions$Name == "Zac Gallen"), 2] <- "Diamondbacks"
predictions[which(predictions$Name == "Zach Davies"), 2] <- "Padres"
predictions[which(predictions$Name == "Zack Greinke"), 2] <- "Astros"

## league average for 2017-2019
League1719 <- filter(fangraphs_clean, Season == "2019" | Season == "2018" | Season == "2017")

leagueAverageXFIP <- League1719 %>% 
  group_by(Season) %>% 
  summarise(
    (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
    mean(lag_xfip2, na.rm = TRUE)) / 3
  )

colnames(leagueAverageXFIP) = c("Season", "avgxFIP")

leagueAverageXFIP = leagueAverageXFIP[3,2]
