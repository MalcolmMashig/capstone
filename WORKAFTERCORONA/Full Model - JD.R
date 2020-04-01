install.packages("dplyr")
library(dplyr)
library(tidyverse)

fangraphs_clean


##### USE LAG FUNCTION FOR YEAR BEFORE
### group by players
### mutate a new column


## NEXT STEP

## Create function that adds a new column of the 
##previous year's xFIP for every player in the data set

### Linear relationship for age seems inaccurate



### Try making new model for every age




### NEW WORK AFTER 3/30

fangraphs_clean <- fangraphs_clean %>% rename(FBP = "FB%")

fangraphs_stdz <- fangraphs_stdz %>% rename(FBP = "FB%")


fangraphs_stdz <- fangraphs_stdz %>% rename(FBP = "FB%")

## Just 1 year back lag
basic_modelstdzLag1 <- lm(xFIP~Age + FBv + FBP + lag_xfip, data=fangraphs_stdz)

summary(basic_modelstdzLag1)


## 1 year and 2 years back
basic_modelstdzLag12 <- lm(xFIP~Age + FBv + FBP + lag_xfip + lag_xfip2, data=fangraphs_stdz)

summary(basic_modelstdzLag12)


## Just 2 year back
basic_modelstdzLag2 <- lm(xFIP~Age + FBv + FBP + lag_xfip2, data=fangraphs_stdz)

summary(basic_modelstdzLag2)


## 1 and 2 and 3 years
basic_modelstdzLag123 <- lm(xFIP~Age + FBv + FBP + lag_xfip + lag_xfip2 + lag_xfip3, data=fangraphs_stdz)

summary(basic_modelstdzLag123)

### Sample size gets smaller but we can see lagxfip 3 years back is NOT significant






#### Try for each age range - age is insignificant because only looking at one year at time
#### 

