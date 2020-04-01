##install.packages("dplyr")
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



## 3 years
basic_modelstdzLag3 <- lm(xFIP~Age + FBv + FBP + lag_xfip3, data=fangraphs_stdz)

summary(basic_modelstdzLag3)

#### Just 3 years


## Add lags for fbv and fbp

basic_modelstdzLag111 <- lm(xFIP~Age + lag_fbv + lag_fbp + lag_xfip, data=fangraphs_stdz)

summary(basic_modelstdzLag111)




## Just 2 year back for everything
basic_modelstdzLag2 <- lm(xFIP~Age + lag_fbv + lag_fbp2 + lag_fbv2 + lag_fbp+ lag_xfip2, data=fangraphs_stdz)

summary(basic_modelstdzLag2)





### Try ages


below30 <- filter(fangraphs_stdz, Age <= 30)

## 1 year and 2 years back
basic_modelstdzLag12 <- lm(xFIP~Age + FBv + FBP + lag_xfip + lag_xfip2, data=below30)

summary(basic_modelstdzLag12)




above30 <- filter(fangraphs_stdz, Age > 30)

## 1 year and 2 years back
basic_modelstdzLag12 <- lm(xFIP~Age + FBv + FBP + lag_xfip + lag_xfip2, data=above30)

summary(basic_modelstdzLag12)

#### Try for each age range - age is insignificant because only looking at one year at time





### Make training and test data
sample.names = unique(fangraphs_stdz$Name)
trainNames = sample(sample.names, size = length(sample.names) / 1.25)
trainNames = as.list(trainNames)
train<- filter(fangraphs_stdz, Name%in%trainNames)  ## 80%
test<- filter(fangraphs_stdz, !Name%in%trainNames) ## 20%


## Perform model
## 1 year and 2 years back
trainLag12 <- lm(xFIP~Age + FBv + FBP + lag_xfip + lag_xfip2, data=train)

summary(trainLag12)



## Look at predictions
predictions <- predict(trainLag12, newdata = test, interval = "confidence")
predictions <- as.tibble(predictions)

dfTest <- as.data.frame(test)
prediColumns <- select(test, c(Name, Season, xFIP) )

RESULTS <- cbind(prediColumns, predictions)
RESULTS <- drop_na(RESULTS)




#############


lda.test <- predict(trainLag12, newdata = test)
table(test$chd69,lda.test$class)
mean(test$chd69 == lda.test$class)

lda.test$class

table(test$chd69, lda.test$posterior[,2]>0.1)


preds<-lda.test$posterior[,2]
rates<-prediction(preds, test$chd69)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Cardio Death")
lines(x = c(0,1), y = c(0,1), col="red")
auc<-performance(rates, measure = "auc")
auc

