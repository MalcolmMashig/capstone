library(here)
library(tidyverse)

here::here(
  'data', 'CONSECUTIVE-import-wrangle.R'
) %>% 
  source()

Year1ME = 0
Year2ME = 0
Year3ME = 0

for (i in 1:100){
  ## Split into training testing data
  sample.data<-sample.int(nrow(fangraphs_stdz), floor(.5*nrow(fangraphs_stdz)), replace = F)
  fangraphs_stdzTRAIN<-fangraphs_stdz[sample.data,]
  fangraphs_stdzTEST <- fangraphs_stdz[-sample.data,]
  
  fangraphs_cleanTRAIN<-fangraphs_clean[sample.data,]
  fangraphs_cleanTEST <- fangraphs_clean[-sample.data,]
  
  
  
  # One year predictions ---------------------- on TRAIN
  
  one_yearTRAIN <- fangraphs_stdzTRAIN %>% 
    count(Name, consecutive_span) %>% 
    filter(n > 2) %>%
    select(Name, consecutive_span) %>% 
    inner_join(fangraphs_stdzTRAIN) %>% 
    filter(!is.na(lag_xfip)) %>% 
    mutate(
      age_range = case_when(
        lag_age < 25 ~ "young",
        between(lag_age, 25, 31) ~ "prime",
        lag_age >= 32 ~ "old"
      )
    )
  
  # fbv submodel
  fbv_submodelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = FBv ~ age_range*lag_age + lag_fbv
    )
  
  fbv_submodelTRAIN %>% 
    summary()
  
  one_yearTRAIN <- one_yearTRAIN %>% 
    mutate(
      predicted_fbv1 = fbv_submodelTRAIN %>% predict()
    )
  
  # fbp submodel
  fbp_submodelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = FBP ~ age_range*lag_age + lag_fbp
    )
  
  fbp_submodelTRAIN %>% 
    summary()
  
  one_yearTRAIN <- one_yearTRAIN %>% 
    mutate(
      predicted_fbp1 = fbp_submodelTRAIN %>% predict()
    )
  
  # kbb submodel
  kbb_submodelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = `K/BB` ~ age_range*lag_age + lag_kbb
    )
  
  kbb_submodelTRAIN %>% 
    summary()
  
  one_yearTRAIN <- one_yearTRAIN %>% 
    mutate(
      predicted_kbb1 = kbb_submodelTRAIN %>% predict()
    )
  
  # xfip model
  xfip_modelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip + predicted_kbb1
    )
  
  xfip_model2TRAIN <- one_yearTRAIN %>% 
    lm(
      formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip +
        lag_xfip2 + predicted_kbb1
    )
  
  xfip_model3TRAIN <- one_yearTRAIN %>% 
    lm(
      formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip +
        lag_xfip2 + lag_xfip3 + predicted_kbb1
    )
  
  xfip_modelTRAIN %>% 
    summary()
  
  one_year1TRAIN <- one_yearTRAIN %>% 
    filter(is.na(lag_xfip2))
  
  one_year1TRAIN <- one_year1TRAIN %>% 
    mutate(
      predicted_xfip1 = xfip_modelTRAIN %>% predict(newdata = one_year1TRAIN),
      # 95% confidence interval
      lower_xfip1 = (xfip_modelTRAIN %>% predict(newdata = one_year1TRAIN,
                                                 interval = "confidence"))[, 2],
      upper_xfip1 = (xfip_modelTRAIN %>% predict(newdata = one_year1TRAIN,
                                                 interval = "confidence"))[, 3]
    )
  
  one_year2TRAIN <- one_yearTRAIN %>% 
    filter(!is.na(lag_xfip2))
  
  one_year2TRAIN %>% 
    mutate(
      predicted_xfip1 = xfip_model2TRAIN %>% predict(newdata = one_year2TRAIN),
      # 95% confidence interval
      lower_xfip1 = (xfip_model2TRAIN %>% predict(newdata = one_year2TRAIN,
                                                  interval = "confidence"))[, 2],
      upper_xfip1 = (xfip_model2TRAIN %>% predict(newdata = one_year2TRAIN,
                                                  interval = "confidence"))[, 3]
    )
  
  one_yearTRAIN <- bind_rows(one_year1TRAIN, one_year2TRAIN)
  
  # Two year predictions ----------------------
  
  two_yearTRAIN <- one_yearTRAIN %>% 
    mutate(
      lag_age = lag_age + 1, # should be Age?
      lag_fbv = predicted_fbv1,
      lag_fbp = predicted_fbp1,
      lag_kbb = predicted_kbb1,
      age_range = case_when(
        lag_age < 25 ~ "young",
        between(lag_age, 25, 31) ~ "prime",
        lag_age >= 32 ~ "old"
      ),
      lag_xfip2 = lag_xfip,
      lag_xfip = predicted_xfip1
    )
  
  two_yearTRAIN <- two_yearTRAIN %>% 
    mutate(
      predicted_fbv1 = fbv_submodelTRAIN %>% predict(newdata = two_yearTRAIN),
      predicted_fbp1 = fbp_submodelTRAIN %>% predict(newdata = two_yearTRAIN),
      predicted_kbb1 = kbb_submodelTRAIN %>% predict(newdata = two_yearTRAIN)
    )
  
  # needs to be separate from above
  two_yearTRAIN <- two_yearTRAIN %>% 
    mutate(
      predicted_xfip2 = xfip_model2TRAIN %>% predict(newdata = two_yearTRAIN),
      # 95% confidence interval
      lower_xfip2 = (xfip_model2TRAIN %>% predict(newdata = two_yearTRAIN,
                                                  interval = "confidence"))[, 2],
      upper_xfip2 = (xfip_model2TRAIN %>% predict(newdata = two_yearTRAIN,
                                                  interval = "confidence"))[, 3]
    )
  
  # Three year predictions ----------------------
  
  three_yearTRAIN <- two_yearTRAIN %>% 
    mutate(
      lag_age = lag_age + 1, # should be lead_age?
      lag_fbv = predicted_fbv1,
      lag_fbp = predicted_fbp1,
      lag_kbb = predicted_kbb1,
      age_range = case_when(
        lag_age < 25 ~ "young",
        between(lag_age, 25, 31) ~ "prime",
        lag_age >= 32 ~ "old"
      ),
      lag_xfip3 = lag_xfip2,
      lag_xfip2 = lag_xfip,
      lag_xfip = predicted_xfip2
    )
  
  three_yearTRAIN <- three_yearTRAIN %>% 
    mutate(
      predicted_fbv1 = fbv_submodelTRAIN %>% predict(newdata = three_yearTRAIN),
      predicted_fbp1 = fbp_submodelTRAIN %>% predict(newdata = three_yearTRAIN),
      predicted_kbb1 = kbb_submodelTRAIN %>% predict(newdata = three_yearTRAIN)
    )
  
  # needs to be separate from above
  three_yearTRAIN <- three_yearTRAIN %>% 
    mutate(
      predicted_xfip3 = xfip_model3TRAIN %>% predict(newdata = three_yearTRAIN),
      # 95% confidence interval
      lower_xfip3 = (xfip_model3TRAIN %>% predict(newdata = three_yearTRAIN,
                                                  interval = "confidence"))[, 2],
      upper_xfip3 = (xfip_model3TRAIN %>% predict(newdata = three_yearTRAIN,
                                                  interval = "confidence"))[, 3]
    )
  
  # Analysis of predictions -----------------
  
  predictionsTRAIN <- three_yearTRAIN %>% 
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
  
  predictions1TRAIN <- fangraphs_cleanTRAIN %>% 
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
    right_join(predictionsTRAIN) %>%
    mutate(
      predicted_xfip1 = predicted_xfip1 * sd_xfip + mean_xfip,
      lower_xfip1 = lower_xfip1 * sd_xfip + mean_xfip,
      upper_xfip1 = upper_xfip1 * sd_xfip + mean_xfip
    ) %>% 
    select(-xFIP)
  
  predictions1TRAIN <- fangraphs_cleanTRAIN %>% 
    select(Name, Season, xFIP) %>% 
    right_join(predictions1TRAIN) %>% 
    select(
      Name,
      Season, xFIP, predicted_xfip1, lower_xfip1, upper_xfip1
    )
  
  predictions2TRAIN <- fangraphs_cleanTRAIN %>% 
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
    right_join(predictionsTRAIN) %>% 
    mutate(
      predicted_xfip2 = predicted_xfip2 * sd_xfip + mean_xfip,
      lower_xfip2 = lower_xfip2 * sd_xfip + mean_xfip,
      upper_xfip2 = upper_xfip2 * sd_xfip + mean_xfip
    ) %>% 
    select(-xFIP2)
  
  predictions2TRAIN <- fangraphs_cleanTRAIN %>% 
    select(Name, Season, xFIP2) %>% 
    right_join(predictions2TRAIN) %>% 
    select(
      Name,
      Season, xFIP2, predicted_xfip2, lower_xfip2, upper_xfip2
    )
  
  predictions3TRAIN <- fangraphs_cleanTRAIN %>% 
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
    right_join(predictionsTRAIN) %>% 
    mutate(
      predicted_xfip3 = predicted_xfip3 * sd_xfip + mean_xfip,
      lower_xfip3 = lower_xfip3 * sd_xfip + mean_xfip,
      upper_xfip3 = upper_xfip3 * sd_xfip + mean_xfip
    ) %>% 
    select(-xFIP3)
  
  predictions3TRAIN <- fangraphs_cleanTRAIN %>% 
    select(Name, Season, xFIP3) %>% 
    right_join(predictions3TRAIN) %>% 
    select(
      Name,
      Season, xFIP3, predicted_xfip3, lower_xfip3, upper_xfip3
    )
  
  predictionsTRAIN <- predictions1TRAIN %>% 
    left_join(predictions2TRAIN) %>% 
    left_join(predictions3TRAIN)
  
  RMSE <- function(predicted, actual){
    sqrt(mean((predicted - actual)^2, na.rm = TRUE))
  }
  
  MeanError<- function(predicted, actual){
    mean(abs(predicted - actual), na.rm = TRUE)
  }
  
  RMSE(predictionsTRAIN$xFIP, predictionsTRAIN$predicted_xfip1)
  RMSE(predictionsTRAIN$xFIP2, predictionsTRAIN$predicted_xfip2)
  RMSE(predictionsTRAIN$xFIP3, predictionsTRAIN$predicted_xfip3)
  
  MeanError(predictionsTRAIN$xFIP, predictionsTRAIN$predicted_xfip1)
  MeanError(predictionsTRAIN$xFIP2, predictionsTRAIN$predicted_xfip2)
  MeanError(predictionsTRAIN$xFIP3, predictionsTRAIN$predicted_xfip3)
  
  
  
  #######################################################################
  ## Now use training models on testing data
  
  
  one_yearTEST <- fangraphs_stdzTEST %>% 
    count(Name, consecutive_span) %>% 
    filter(n > 2) %>%
    select(Name, consecutive_span) %>% 
    inner_join(fangraphs_stdzTEST) %>% 
    filter(!is.na(lag_xfip)) %>% 
    mutate(
      age_range = case_when(
        lag_age < 25 ~ "young",
        between(lag_age, 25, 31) ~ "prime",
        lag_age >= 32 ~ "old"
      )
    )
  
  # fbv submodel-- keep training models
  fbv_submodelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = FBv ~ age_range*lag_age + lag_fbv
    )
  
  fbv_submodelTRAIN %>% 
    summary()
  
  ## Use training model to predict test data
  one_yearTEST <- one_yearTEST %>% 
    mutate(
      predicted_fbv1 = fbv_submodelTRAIN %>% predict(newdata = one_yearTEST)
    )
  
  # fbp submodel- train
  fbp_submodelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = FBP ~ age_range*lag_age + lag_fbp
    )
  
  fbp_submodelTRAIN %>% 
    summary()
  
  ## Use training model to predict test data
  one_yearTEST <- one_yearTEST %>% 
    mutate(
      predicted_fbp1 = fbp_submodelTRAIN %>% predict(newdata = one_yearTEST)
    )
  
  # kbb submodel- train
  kbb_submodelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = `K/BB` ~ age_range*lag_age + lag_kbb
    )
  
  kbb_submodelTRAIN %>% 
    summary()
  
  ## Use training model to predict test data
  one_yearTEST <- one_yearTEST %>% 
    mutate(
      predicted_kbb1 = kbb_submodelTRAIN %>% predict(newdata = one_yearTEST)
    )
  
  # xfip model -- train
  xfip_modelTRAIN <- one_yearTRAIN %>% 
    lm(
      formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip + predicted_kbb1
    )
  
  xfip_model2TRAIN <- one_yearTRAIN %>% 
    lm(
      formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip +
        lag_xfip2 + predicted_kbb1
    )
  
  xfip_model3TRAIN <- one_yearTRAIN %>% 
    lm(
      formula = xFIP ~ predicted_fbp1 + predicted_fbv1 + lag_age + lag_xfip +
        lag_xfip2 + lag_xfip3 + predicted_kbb1
    )
  
  xfip_modelTRAIN %>% 
    summary()
  
  ## Use training model to predict test data
  one_year1TEST <- one_yearTEST %>% 
    filter(is.na(lag_xfip2))
  
  ## Use training model to predict test data
  one_year1TEST <- one_year1TEST %>% 
    mutate(
      predicted_xfip1 = xfip_modelTRAIN %>% predict(newdata = one_year1TEST),
      # 95% confidence interval
      lower_xfip1 = (xfip_modelTRAIN %>% predict(newdata = one_year1TEST,
                                                 interval = "confidence"))[, 2],
      upper_xfip1 = (xfip_modelTRAIN %>% predict(newdata = one_year1TEST,
                                                 interval = "confidence"))[, 3]
    )
  
  one_year2TEST <- one_yearTEST %>% 
    filter(!is.na(lag_xfip2))
  
  one_year2TEST %>% 
    mutate(
      predicted_xfip1 = xfip_model2TRAIN %>% predict(newdata = one_year2TEST),
      # 95% confidence interval
      lower_xfip1 = (xfip_model2TRAIN %>% predict(newdata = one_year2TEST,
                                                  interval = "confidence"))[, 2],
      upper_xfip1 = (xfip_model2TRAIN %>% predict(newdata = one_year2TEST,
                                                  interval = "confidence"))[, 3]
    )
  
  one_yearTEST <- bind_rows(one_year1TEST, one_year2TEST)
  
  # Two year predictions ----------------------
  
  two_yearTEST <- one_yearTEST %>% 
    mutate(
      lag_age = lag_age + 1, # should be Age?
      lag_fbv = predicted_fbv1,
      lag_fbp = predicted_fbp1,
      lag_kbb = predicted_kbb1,
      age_range = case_when(
        lag_age < 25 ~ "young",
        between(lag_age, 25, 32) ~ "prime",
        lag_age >= 33 ~ "old"
      ),
      lag_xfip2 = lag_xfip,
      lag_xfip = predicted_xfip1
    )
  
  two_yearTEST <- two_yearTEST %>% 
    mutate(
      predicted_fbv1 = fbv_submodelTRAIN %>% predict(newdata = two_yearTEST),
      predicted_fbp1 = fbp_submodelTRAIN %>% predict(newdata = two_yearTEST),
      predicted_kbb1 = kbb_submodelTRAIN %>% predict(newdata = two_yearTEST)
    )
  
  # needs to be separate from above
  two_yearTEST <- two_yearTEST %>% 
    mutate(
      predicted_xfip2 = xfip_model2TRAIN %>% predict(newdata = two_yearTEST),
      # 95% confidence interval
      lower_xfip2 = (xfip_model2TRAIN %>% predict(newdata = two_yearTEST,
                                                  interval = "confidence"))[, 2],
      upper_xfip2 = (xfip_model2TRAIN %>% predict(newdata = two_yearTEST,
                                                  interval = "confidence"))[, 3]
    )
  
  # Three year predictions ----------------------
  
  three_yearTEST <- two_yearTEST %>% 
    mutate(
      lag_age = lag_age + 1, 
      lag_fbv = predicted_fbv1,
      lag_fbp = predicted_fbp1,
      lag_kbb = predicted_kbb1,
      age_range = case_when(
        lag_age < 25 ~ "young",
        between(lag_age, 25, 31) ~ "prime",
        lag_age >= 32 ~ "old"
      ),
      lag_xfip3 = lag_xfip2,
      lag_xfip2 = lag_xfip,
      lag_xfip = predicted_xfip2
    )
  
  three_yearTEST <- three_yearTEST %>% 
    mutate(
      predicted_fbv1 = fbv_submodelTRAIN %>% predict(newdata = three_yearTEST),
      predicted_fbp1 = fbp_submodelTRAIN %>% predict(newdata = three_yearTEST),
      predicted_kbb1 = kbb_submodelTRAIN %>% predict(newdata = three_yearTEST)
    )
  
  # needs to be separate from above
  three_yearTEST <- three_yearTEST %>% 
    mutate(
      predicted_xfip3 = xfip_model3TRAIN %>% predict(newdata = three_yearTEST),
      # 95% confidence interval
      lower_xfip3 = (xfip_model3TRAIN %>% predict(newdata = three_yearTEST,
                                                  interval = "confidence"))[, 2],
      upper_xfip3 = (xfip_model3TRAIN %>% predict(newdata = three_yearTEST,
                                                  interval = "confidence"))[, 3]
    )
  
  # Analysis of predictions -----------------
  
  predictionsTEST <- three_yearTEST %>% 
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
  
  predictions1TEST <- fangraphs_cleanTEST %>% 
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
    right_join(predictionsTEST) %>%
    mutate(
      predicted_xfip1 = predicted_xfip1 * sd_xfip + mean_xfip,
      lower_xfip1 = lower_xfip1 * sd_xfip + mean_xfip,
      upper_xfip1 = upper_xfip1 * sd_xfip + mean_xfip
    ) %>% 
    select(-xFIP)
  
  predictions1TEST <- fangraphs_cleanTEST %>% 
    select(Name, Season, xFIP) %>% 
    right_join(predictions1TEST) %>% 
    select(
      Name,
      Season, xFIP, predicted_xfip1, lower_xfip1, upper_xfip1
    )
  
  predictions2TEST <- fangraphs_cleanTEST %>% 
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
    right_join(predictionsTEST) %>% 
    mutate(
      predicted_xfip2 = predicted_xfip2 * sd_xfip + mean_xfip,
      lower_xfip2 = lower_xfip2 * sd_xfip + mean_xfip,
      upper_xfip2 = upper_xfip2 * sd_xfip + mean_xfip
    ) %>% 
    select(-xFIP2)
  
  predictions2TEST <- fangraphs_cleanTEST %>% 
    select(Name, Season, xFIP2) %>% 
    right_join(predictions2TEST) %>% 
    select(
      Name,
      Season, xFIP2, predicted_xfip2, lower_xfip2, upper_xfip2
    )
  
  predictions3TEST <- fangraphs_cleanTEST %>% 
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
    right_join(predictionsTEST) %>% 
    mutate(
      predicted_xfip3 = predicted_xfip3 * sd_xfip + mean_xfip,
      lower_xfip3 = lower_xfip3 * sd_xfip + mean_xfip,
      upper_xfip3 = upper_xfip3 * sd_xfip + mean_xfip
    ) %>% 
    select(-xFIP3)
  
  predictions3TEST <- fangraphs_cleanTEST %>% 
    select(Name, Season, xFIP3) %>% 
    right_join(predictions3TEST) %>% 
    select(
      Name,
      Season, xFIP3, predicted_xfip3, lower_xfip3, upper_xfip3
    )
  
  predictionsTEST <- predictions1TEST %>% 
    left_join(predictions2TEST) %>% 
    left_join(predictions3TEST)
  
  RMSE <- function(predicted, actual){
    sqrt(mean((predicted - actual)^2, na.rm = TRUE))
  }
  
  MeanError<- function(predicted, actual){
    mean(abs(predicted - actual), na.rm = TRUE)
  }
  
  RMSE(predictionsTEST$xFIP, predictionsTEST$predicted_xfip1)
  RMSE(predictionsTEST$xFIP2, predictionsTEST$predicted_xfip2)
  RMSE(predictionsTEST$xFIP3, predictionsTEST$predicted_xfip3)
  
  Year1ME = Year1ME + MeanError(predictionsTEST$xFIP, predictionsTEST$predicted_xfip1)
  Year2ME = Year2ME + MeanError(predictionsTEST$xFIP2, predictionsTEST$predicted_xfip2)
  Year3ME = Year3ME + MeanError(predictionsTEST$xFIP3, predictionsTEST$predicted_xfip3)
  
} ## end of for loop

Year1ME = Year1ME / 100
Year2ME = Year2ME / 100
Year3ME = Year3ME / 100


TestError <- data.frame(c(Year1ME, Year2ME, Year3ME))

#TestError <- data.frame(c(0.3516178, 0.4445261, 0.4783301)) 
## Hardcoded so we can remember them

rownames(TestError) = c("Year1TestME", "Year2TestME", "Year3TestME")

t(TestError)


# In order to compare to my experimenting I ran both 100 times on 100 splits
# Test Error 1-3:: 0.3511142   0.4395727   0.4606305