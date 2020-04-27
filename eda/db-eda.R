library(tidyverse)
setwd("~/Google Drive/2nd Year/STAT 4996/Github Folder/data")
fulldata <-  read.csv("fangraphs.csv")
dim(fulldata)
colnames(fulldata)

ggplot(fangraphs_clean, aes(x=Age, y=ERA)) + geom_point()
ggplot(fangraphs_clean, aes(x=Age, y=IP)) + geom_point()
ggplot(fangraphs_clean, aes(x=Age, y=FBv)) + geom_point()
ggplot(fulldata, aes(x=Age, y=WAR)) + geom_point()
ggplot(fulldata, aes(x=Age, y=FBv)) + geom_point()

sampledata <- sample_n(fulldata, 100)
ggplot(fangraphs_clean, aes(x=Age, y=FBv)) + geom_point()

meanFBv_by_age <- group_by(fangraphs_clean, Age) %>% summarize(meanFBv = mean(FBv), medianFBv = median(FBv))
ggplot(meanFBv_by_age, aes(x=Age, y=meanFBv)) + geom_point()
ggplot(meanFBv_by_age, aes(x=Age, y=medianFBv)) + geom_point()
meanFBv_by_age$meanFBv
meanFBv_by_age$medianFBv
