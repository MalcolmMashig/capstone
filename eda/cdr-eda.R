

pitchers <- read.csv("fangraphs.csv")

 pitchers <- read.csv("fangraphs.csv")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# AGES of PITCHERS
age_hist <- ggplot(pitchers, aes(Age)) + geom_histogram(bins=20)
age_hist

# Looking at average fastball velocity by age
avg_fbv <- function(age){
  df <- pitchers %>%
    filter(Age == age)
  fb <- df$FBv
  mean(fb)
}

ages <- seq(20,47,1)
vel <- sapply(ages, avg_fbv)
vel
df2 <- data.frame("AGE" = ages, "Velocity" = vel)
df2
ggplot(df2, aes(x = AGE, y=Velocity)) + geom_point() + geom_line()

#Median fastball velocity by age
med_fbv <- function(age){
  df <- pitchers %>%
    filter(Age == age)
  fb <- df$FBv
  median(fb)
}
medvel <- sapply(ages, med_fbv)
medvel
df3 <- data.frame("AGE" = ages, "Median_Velocity" = vel)
ggplot(df3, aes(x = AGE, y=Median_Velocity)) + geom_point() + geom_line()

tw1819 <- pitchers %>% 
  filter(Season == 2019 | Season == 2018)
ids <- tw1819$playerid

# Code to get common players - from Devan Bose
p2018 <- filter(pitchers, Season==2018)
p2019 <- filter(pitchers, Season==2019)
common <- intersect(p2018$playerid, p2019$playerid)
common18 <- filter(p2018, playerid %in% common)
common19 <- filter(p2019, playerid %in% common)
combined1819 <- cbind(common18, common19$WAR, common19$xFIP)

#How does WAR correlate over larger samples
p2017 <- filter(pitchers, Season==2017)
p2016 <- filter(pitchers, Season==2016)
p2015 <- filter(pitchers, Season==2015)
common2 <- intersect(common, p2017$playerid)
common3 <- intersect(common2, p2016$playerid)
common4 <- intersect(common3, p2015$playerid)

#33 pitchers have 5 consecutive season's worth of data
# Create WAR/100 IP metric to standardize
#2019
war19 <- p2019$WAR
ip19 <- p2019$IP
war_per19 <- (war19/ip19) * 100 
p2019 <- cbind(p2019, war_per19)

war18 <- p2018$WAR
ip18 <- p2018$IP
war_per18 <- (war18/ip18) * 100 
p2018 <- cbind(p2018, war_per18)

war17 <- p2017$WAR
ip17 <- p2017$IP
war_per17 <- (war17/ip17) * 100 
p2017 <- cbind(p2017, war_per17)

war16 <- p2016$WAR
ip16 <- p2016$IP
war_per16 <- (war16/ip16) * 100 
p2016 <- cbind(p2016, war_per16)

war15 <- p2015$WAR
ip15 <- p2015$IP
war_per15 <- (war15/ip15) * 100 
p2015 <- cbind(p2015, war_per15)

# combine it all into one table
corr2019 <- filter(p2019, playerid %in% common4)
corr2018 <- filter(p2018, playerid %in% common4)
corr2017 <- filter(p2017, playerid %in% common4)
corr2016 <- filter(p2016, playerid %in% common4)
corr2015 <- filter(p2015, playerid %in% common4)
combined_corr <- cbind(corr2019, corr2018$war_per18, corr2018$xFIP, corr2017$war_per17, corr2017$xFIP, 
                       corr2016$war_per16, corr2016$xFIP, corr2015$war_per15, corr2015$xFIP)

#How do WAR and xFip correlate over the years?

#2018-2019 WAR = 0.65
cor(combined_corr$`corr2018$war_per18`, combined_corr$war_per19) 

#2018-2019 xFip = 0.81
cor(combined_corr$`corr2018$xFIP`, combined_corr$xFIP)

#2017-2019 WAR = 0.37
cor(combined_corr$`corr2017$war_per17`, combined_corr$war_per19) 

#2017-2019 xFip = 0.65
cor(combined_corr$`corr2017$xFIP`, combined_corr$xFIP)

#2016-2019 WAR = 0.37
cor(combined_corr$`corr2016$war_per16`, combined_corr$war_per19) 

#2016-2019 xFip = 0.46
cor(combined_corr$`corr2016$xFIP`, combined_corr$xFIP)

#2015-2019 WAR = 0.52
cor(combined_corr$`corr2015$war_per15`, combined_corr$war_per19) 

#2015-2019 xFip = 0.55
cor(combined_corr$`corr2015$xFIP`, combined_corr$xFIP)

## Conclusions: xFip more predictive than WAR, previous season is the best but earlier seasons also have value, could
## be small sample size but 5 years ago may be just as predictive as 2 years ago

## What if we increase sample size for 2 years ago?
corr2019_2 <- filter(p2019, playerid %in% common2)
corr2018_2 <- filter(p2018, playerid %in% common2)
corr2017_2 <- filter(p2017, playerid %in% common2)
combined_corr_2 <- cbind(corr2019_2, corr2018_2$war_per18, corr2018_2$xFIP, corr2017_2$war_per17, corr2017_2$xFIP)

#2018-2019 WAR pt2 = 0.53
cor(combined_corr_2$`corr2018_2$war_per18`, combined_corr_2$war_per19) 

#2018-2019 xFip pt2 = 0.76
cor(combined_corr_2$`corr2018_2$xFIP`, combined_corr_2$xFIP)

#2018-2019 WAR pt2 = 0.32
cor(combined_corr_2$`corr2017_2$war_per17`, combined_corr_2$war_per19) 

#2018-2019 xFip pt2 = 0.56
cor(combined_corr_2$`corr2017_2$xFIP`, combined_corr_2$xFIP)

## Didn't seem to help much...

## NEXT STEPS: Try to test correlation for all 1/2/3/4/5 year splits between players (increase sample size tremendously)



