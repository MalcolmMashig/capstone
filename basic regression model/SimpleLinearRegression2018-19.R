library(tidyverse)

# predicting 2019 data with 2018 stats

data_2018_2019 <- filter(data_player_year, Team_2018 != "NA" & Team_2019 != "NA")
names(data_2018_2019)

# perceived most important stats

model1 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018, data=data_2018_2019)
summary(model1)

# all stats 

model2 <- lm(xFIP_2019 ~ Age_2018 + GS_2018 + IP_2018 + StartIP_2018 + ERA_2018 + WHIP_2018 + WAR_2018 
             + xFIP_2018 + BABIP_2018 + FRM_2018 + `K/9_2018` + `BB/9_2018` + `K/BB_2018` + `H/9_2018` 
             + `HR/9_2018` + `Groundball%_2018` + `Flyball%_2018` + `K%_2018` + `BB%_2018` + `FB%_2018`
             + FBv_2018 + wFB_2018, data=data_2018_2019)
summary(model2)

# performance metrics

model3 <- lm(xFIP_2019 ~ ERA_2018 + WHIP_2018 + WAR_2018 + xFIP_2018 + BABIP_2018 + FRM_2018, data=data_2018_2019)
summary(model3)

# rates

model4 <- lm(xFIP_2019 ~ `K/9_2018` + `BB/9_2018` + `K/BB_2018` + `H/9_2018` + `HR/9_2018`, data=data_2018_2019)
summary(model4)

# percentages 

model5 <- lm(xFIP_2019 ~ `Groundball%_2018` + `Flyball%_2018` + `K%_2018` + `BB%_2018` + `FB%_2018`
            + FBv_2018 + wFB_2018, data=data_2018_2019)
summary(model5)

# most significant varibles from all prior models

model6 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `K/9_2018` + `HR/9_2018`+ `K%_2018` 
             + WAR_2018 + `Flyball%_2018` + BABIP_2018 + `FB%_2018`, data=data_2018_2019)
summary(model6)

# remove K/9

model7 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `HR/9_2018`+ `K%_2018` 
             + WAR_2018 + `Flyball%_2018` + BABIP_2018 + `FB%_2018`, data=data_2018_2019)
summary(model7)

# remove K%

model8 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `HR/9_2018`+ WAR_2018 
             + `Flyball%_2018` + BABIP_2018 + `FB%_2018`, data=data_2018_2019)
summary(model8)

# remove WAR and FB%

model9 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `HR/9_2018` + `Flyball%_2018` 
             + BABIP_2018, data=data_2018_2019)
summary(model9)

# remove WHIP and BABIP

model10 <- lm(xFIP_2019 ~ xFIP_2018 + FBv_2018 + `HR/9_2018` + `Flyball%_2018`, 
              data=data_2018_2019)
summary(model10)

# remove Flyball% 

model11 <- lm(xFIP_2019 ~ xFIP_2018 + FBv_2018, data=data_2018_2019)
summary(model11)  # all variables very significant


# find RMSE for models
# try for experience in years
# try a span of more than 2 years








## Jordan will try adding multiple years





# predicting 2019 data with 2018 and 2017 stats

data_2017_2018_2019 <- filter(data_player_year, Team_2018 != "NA" & Team_2019 != "NA" & Team_2017 != "NA") 
names(data_2017_2018_2019)

# perceived most important stats

Years2model1 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + Age_2017 + xFIP_2017
                   + FBv_2017, data=data_2017_2018_2019)
summary(Years2model1)

# all stats 

Years2model2 <- lm(xFIP_2019 ~ Age_2018 + GS_2018 + IP_2018 + StartIP_2018 + ERA_2018 + WHIP_2018 + WAR_2018 
             + xFIP_2018 + BABIP_2018 + FRM_2018 + `K/9_2018` + `BB/9_2018` + `K/BB_2018` + `H/9_2018` 
             + `HR/9_2018` + `Groundball%_2018` + `Flyball%_2018` + `K%_2018` + `BB%_2018` + `FB%_2018`
             + `wFB_2018` + `Age_2017` + `GS_2017` + `IP_2017` + `StartIP_2017` + `ERA_2017` + `WHIP_2017` + `WAR_2017` 
             + `xFIP_2017` + `BABIP_2017` + `FRM_2017` + `K/9_2017` + `BB/9_2017` + `K/BB_2017` + `H/9_2017` 
             + `HR/9_2017` + `Groundball%_2017` + `Flyball%_2017` + `K%_2017` + `BB%_2017` + `FB%_2017`
             + `FBv_2017` + `wFB_2017`, data=data_2017_2018_2019)
summary(Years2model2)

# performance metrics

Years2model3 <- lm(xFIP_2019 ~ ERA_2018 + WHIP_2018 + WAR_2018 + xFIP_2018 + BABIP_2018 + FRM_2018 + 
                     ERA_2017 + WHIP_2017 + WAR_2017 + xFIP_2017 + BABIP_2017 + FRM_2017, data=data_2017_2018_2019)
summary(Years2model3)

# rates

Years2model4 <- lm(xFIP_2019 ~ `K/9_2018` + `BB/9_2018` + `K/BB_2018` + `H/9_2018` + `HR/9_2018` +
                     `K/9_2017` + `BB/9_2017` + `K/BB_2017` + `H/9_2017` + `HR/9_2017`, data=data_2017_2018_2019)
summary(Years2model4)

# percentages 

Years2model5 <- lm(xFIP_2019 ~ `Groundball%_2018` + `Flyball%_2018` + `K%_2018` + `BB%_2018` + `FB%_2018`
             + FBv_2018 + wFB_2018 + `Groundball%_2017` + `Flyball%_2017` + `K%_2017` + `BB%_2017` + `FB%_2017`
             + FBv_2017 + wFB_2017, data=data_2017_2018_2019)
summary(Years2model5)

# most significant varibles from all prior models

Years2model6 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `K/9_2018` + `HR/9_2018`+ `K%_2018` 
             + WAR_2018 + `Flyball%_2018` + BABIP_2018 + `FB%_2018` + `BB/9_2017` + `K/BB_2017` + `H/9_2017` +
               `HR/9_2017` + `xFIP_2017` + `BABIP_2017`, data=data_2017_2018_2019)
summary(Years2model6)

# remove K/9

Years2model7 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `HR/9_2018`+ `K%_2018` 
             + WAR_2018 + `Flyball%_2018` + BABIP_2018 + `FB%_2018`, data=data_data_2017_2018_2019)
summary(Years2model7)

# remove K%

Years2model8 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `HR/9_2018`+ WAR_2018 
             + `Flyball%_2018` + BABIP_2018 + `FB%_2018`, data=data_2017_2018_2019)
summary(Years2model8)

# remove WAR and FB%

Years2model9 <- lm(xFIP_2019 ~ WHIP_2018 + xFIP_2018 + FBv_2018 + `HR/9_2018` + `Flyball%_2018` 
             + BABIP_2018, data=data_2017_2018_2019)
summary(Years2model9)

# remove WHIP and BABIP

Years2model10 <- lm(xFIP_2019 ~ xFIP_2018 + FBv_2018 + `HR/9_2018` + `Flyball%_2018`, 
              data=data_data_2017_2018_2019)
summary(Years2model10)

# remove Flyball% 

model11 <- lm(xFIP_2019 ~ xFIP_2018 + FBv_2018, data=data_2017_2018_2019)
summary(Years2model11)  # all variables very significant
