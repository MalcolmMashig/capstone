getwd()
setwd("/Users/christianrogers/Desktop/capstone/data")
library(tidyverse)
library(ggplot2)
library(gridExtra)

# First run complete data wrange file

# 1. Fastball velocity over time (seasons) - how has it changed
years <- seq(2002,2019,1)

## function to get 25th percentile
first_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.25)
}

## 50th percentile function
med_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.5)
}

## 75th percentile function
third_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.75)
}

## 90th percentile function
ninety_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.9)
}

## applying each function to the data
first <- sapply(years, first_func)
first <- as.numeric(first)
second <- sapply(years, med_func)
second <- as.numeric(second)
third <- sapply(years, third_func)
third <- as.numeric(third)
ninety <- sapply(years, ninety_func)
ninety <- as.numeric(ninety)

## preparing data for DF and plot
years3 <- rep(years, 4)
vel <- c(first, second, third, ninety)
t5 <- rep("25th", 18)
f0 <- rep("50th", 18)
s5 <- rep("75th", 18)
n0 <-  rep("90th", 18)
percentile <- c(t5, f0, s5, n0)

## DF to plot
percentiles <- data.frame("Year" = years3, "Velocity" = vel, "Percentile"  = percentile)

## plot lines grouped by percentile
plot1 <- ggplot(percentiles, aes(x=Year, y=Velocity, color = Percentile)) + geom_line() + geom_point() +
  labs(title="Fastball Velocity Percentiles by Year", y="Fastball Velocity") +
  scale_x_continuous(breaks=seq(2002,2018,2)) + scale_y_continuous(breaks=seq(87, 95, 1))
plot1

## CONCLUSION: It appears that fastball velocities across the spectrum of pitchers are going up together.
## Therefore, standardization will probably be sufficient. However,  it looks like the 90th percentile may
## be separating itself post 2016 - something to maybe look into.

# 2. Fastball velocity distributions over time

## get dataframe and  histogram for each  year
df2002 <- filter(careers_clean, Season==2002)
df2002 <- filter(df2002, IP >= 30)
plot2002 <- ggplot(df2002, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2002 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5))
plot2002

df2010 <- filter(careers_clean, Season==2010)
df2010 <- filter(df2010, IP >= 30)
plot2010 <- ggplot(df2010, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2010 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(75,95,5))
plot2010

df2015 <- filter(careers_clean, Season==2015)
df2015 <- filter(df2015, IP >= 30)
plot2015 <- ggplot(df2015, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2015 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(75,95,5))
plot2015

df2019 <- filter(careers_clean, Season==2019)
df2019 <- filter(df2019, IP >= 30)
plot2019 <- ggplot(df2019, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2019 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(75,95,5))
plot2019

hists <- grid.arrange(plot2002, plot2010, plot2015, plot2019)
hists

# 3. has xFip changed over time

## Same analysis/process as #1
years <- seq(2002,2019,1)

## function to get 25th percentile
first_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.75)
}

## 50th percentile function
med_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.5)
}

## 75th percentile function
third_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.25)
}

## 90th percentile function
ninety_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.1)
}

## applying each function to the data
first <- sapply(years, first_func)
first <- as.numeric(first)
second <- sapply(years, med_func)
second <- as.numeric(second)
third <- sapply(years, third_func)
third <- as.numeric(third)
ninety <- sapply(years, ninety_func)
ninety <- as.numeric(ninety)

## preparing data for DF and plot
years3 <- rep(years, 4)
xfip <- c(first, second, third, ninety)
t5 <- rep("25th", 18)
f0 <- rep("50th", 18)
s5 <- rep("75th", 18)
n0 <-  rep("90th", 18)
percentile <- c(t5, f0, s5, n0)

## DF to plot
percentiles <- data.frame("Year" = years3, "xFIP" = xfip, "Percentile"  = percentile)

## plot lines grouped by percentile
plot3 <- ggplot(percentiles, aes(x=Year, y=xFIP, color = Percentile)) + geom_line() + geom_point() +
  labs(title="xFIP Percentiles by Year", y="xFIP") +
  scale_x_continuous(breaks=seq(2002,2018,2)) 
plot3

## Really interesting - seems like FBv the league tends to go in the same direction - fell off hard from 2006 to
## 2014 or so, then jumped back up. Either random variation, or home run boom has hurt pitchers in xFIP (since
## home run rate is one of the factors in it)

# 4. How each of the variables correlates with itself over all 1-year, 2-year, 3-year, 5-year spans

## Function to calculate year 1 of any span in the data - x is how far apart the years are
span_func1 <- function(year, x, col){
  year1 <- as.character(year)
  year2 <- as.character((year+x))
  yr1str <- paste((col), year1, sep="")
  yr2str <- paste((col), year2, sep="")
  df <- select(data_by_year, c(yr1str, yr2str))
  df <- na.omit(df)
  df[,1]
}

## Calculate year 2 of any span in the data
span_func2 <- function(year, x, col){
  year1 <- as.character(year)
  year2 <- as.character((year+x))
  yr1str <- paste((col), year1, sep="")
  yr2str <- paste((col), year2, sep="")
  df <- select(data_by_year, c(yr1str, yr2str))
  df <- na.omit(df)
  df[,2]
}

## set years, apply for 1 year spans of xFip
years <- seq(2002,2018,1)
xfip1 <- sapply(years, span_func1, x=1, col='xFIP_')
xfip2 <- sapply(years, span_func2, x=1, col='xFIP_')

## get vectors of year 1 and year 2 and correlation
yr1xfip <- c(xfip1$xFIP_2002, xfip1$xFIP_2003, xfip1$xFIP_2004, xfip1$xFIP_2005, xfip1$xFIP_2006, xfip1$xFIP_2007,
             xfip1$xFIP_2008, xfip1$xFIP_2009, xfip1$xFIP_2010, xfip1$xFIP_2011, xfip1$xFIP_2012, xfip1$xFIP_2013,
             xfip1$xFIP_2014, xfip1$xFIP_2015, xfip1$xFIP_2016, xfip1$xFIP_2017, xfip1$xFIP_2018)
yr2xfip <- c(xfip2$xFIP_2003, xfip2$xFIP_2004, xfip2$xFIP_2005, xfip2$xFIP_2006, xfip2$xFIP_2007,
             xfip2$xFIP_2008, xfip2$xFIP_2009, xfip2$xFIP_2010, xfip2$xFIP_2011, xfip2$xFIP_2012, 
             xfip2$xFIP_2013, xfip2$xFIP_2014, xfip2$xFIP_2015, xfip2$xFIP_2016, xfip2$xFIP_2017, 
             xfip2$xFIP_2018, xfip2$xFIP_2019)
cor(yr1xfip, yr2xfip)
cor(xfip1$xFIP_2018, xfip2$xFIP_2019)
## only 0.39!!


# Looking at the distribution in change in xFip by age
# start with age 25 to 26

df25 <- filter(fangraphs_stdz, Age == 25)
df26 <- filter(fangraphs_stdz, Age == 26)
common <- intersect(df25$Name, df26$Name)
df25 <- filter(df25, Name %in% common)
df26 <- filter(df26, Name %in% common)
df25$xFIP2 <- df26$xFIP
df25 <- select(df25, c('Name', 'xFIP', 'xFIP2'))
change <- df25$xFIP2 - df25$xFIP
df25$change <- change
hist <- ggplot(df25, aes(x=change)) + geom_histogram(bins = 15, fill='white', color='black') +
  labs(title="Change in Standardized xFIP from age 25 to 26") + theme(plot.title = element_text(hjust = 0.5))
hist

# age 29 to 30
df29 <- filter(fangraphs_stdz, Age == 29)
df30 <- filter(fangraphs_stdz, Age == 30)
common2 <- intersect(df29$Name, df30$Name)
df29 <- filter(df29, Name %in% common2)
df30 <- filter(df30, Name %in% common2)
df29$xFIP2 <- df30$xFIP
df29 <- select(df29, c('Name', 'xFIP', 'xFIP2'))
change <- df29$xFIP2 - df29$xFIP
df29$change <- change
hist2 <- ggplot(df29, aes(x=change)) + geom_histogram(bins = 15, fill='white', color='black') +
  labs(title="Change in Standardized xFIP from age 29 to 30") + theme(plot.title = element_text(hjust = 0.5))
hist2

# age 33 to 34
df33 <- filter(fangraphs_stdz, Age == 33)
df34 <- filter(fangraphs_stdz, Age == 34)
common3 <- intersect(df25$Name, df26$Name)
df33 <- filter(df33, Name %in% common3)
df34 <- filter(df34, Name %in% common3)
df33$xFIP2 <- df34$xFIP
df33 <- select(df33, c('Name', 'xFIP', 'xFIP2'))
change <- df33$xFIP2 - df33$xFIP
df33$change <- change
hist3 <- ggplot(df33, aes(x=change)) + geom_histogram(bins = 15, fill='white', color='black') +
  labs(title="Change in Standardized xFIP from age 33 to 34") + theme(plot.title = element_text(hjust = 0.5))
hist3

