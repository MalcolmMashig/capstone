library(tidyverse)
setwd("~/Google Drive/2nd Year/STAT 4996/Github Folder/data")
fulldata <-  read.csv("fangraphs.csv")

data2018 <- filter(fulldata, Season==2018)
data2019 <- filter(fulldata, Season==2019)

common_pitchers <- intersect(data2018$playerid, data2019$playerid)
common_pitchers

common2018 <- filter(data2018, playerid %in% common_pitchers)
common2019 <- filter(data2019, playerid %in% common_pitchers)
common2018
common2019

combined20182019 <- cbind(common2018, common2019$WAR, common2019$xFIP)
combined20182019
colnames(combined20182019)
?lm
basic_model <- lm(common2019$xFIP~Age + xFIP + FBv, data=combined20182019)
summary(basic_model)
