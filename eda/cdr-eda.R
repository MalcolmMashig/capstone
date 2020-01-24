setwd("/Users/christianrogers/Desktop/capstone/data")
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
