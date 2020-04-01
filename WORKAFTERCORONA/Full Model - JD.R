install.packages("dplyr")
library(dplyr)
library(tidyverse)

fangraphs_clean


##### USE LAG FUNCTION FOR YEAR BEFORE
### group by players
### mutate a new column


## Overall model
fangraphs_clean <- fangraphs_clean %>% rename(FBP = "FB%")

basic_model <- lm(xFIP~Age + FBv + FBP, data=fangraphs_clean)

summary(basic_model)

## Overall model standardized

fangraphs_stdz<- fangraphs_stdz %>% rename(FBP = "FB%")

basic_modelstdz <- lm(xFIP~Age + FBv + FBP, data=fangraphs_stdz)

summary(basic_modelstdz)


help(lag)
lag(xFIP)


### Add lag without fixing it yet for when it gets the wrong player

basic_modeLag <- lm(xFIP~Age + FBv + FBP + lag(xFIP), data=fangraphs_clean)

summary(basic_modeLag)

basic_modelstdzLag <- lm(xFIP~Age + FBv + FBP + lag(xFIP), data=fangraphs_stdz)

summary(basic_modelstdzLag)



## NEXT STEP

## Create function that adds a new column of the 
##previous year's xFIP for every player in the data set


### Linear relationship for age seems inaccurate



### Try making new model for every age

## Try making models for each age




