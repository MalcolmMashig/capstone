# Need to run import-wrangle.r file to get standardized vars
library(dplyr)
library(tidyverse)

## Function to cluster based off of one column - returns names of pitchers that can be put in for analysis
## df = ALWAYS fangraphs_stdz
## column1 = first column to cluster on
## two = 'na' if only one column, anything else for two columns
## column2 = second to cluster on (if used)
## cutoff1 = standardized cutoff for column1
## cutoff2 = standardized cutoff for column2
## dir1 = direction for column1 (>=/<= cutoff)
## dir2 = direction for column2 (>=/<= cutoff)
## season_low = first season to get data from
## season_high = last season to get data from
## age_low = youngest age to get data from
## age_high = oldest age to get data from
cluster <- function(df, column1, two, column2, cutoff1, cutoff2, dir1, dir2, season_low, season_high, age_low, age_high){
  if (dir1 == 'l'){
    if(two == 'na'){
      df <- df %>%
        filter(Season >= season_low) %>%
        filter(Season <= season_high) %>%
        filter(Age >= age_low) %>%
        filter(Age <= age_high) %>%
        filter({{column1}} <= cutoff1)
      return(df$Name)
    } else {
      if (dir2 == 'l'){
      df <- df %>%
        filter(Season >= season_low) %>%
        filter(Season <= season_high) %>%
        filter(Age >= age_low) %>%
        filter(Age <= age_high) %>%
        filter({{column1}} <= cutoff1) %>%
        filter({{column2}} <= cutoff2)
      return(df$Name)
      } else {
        df <- df %>%
          filter(Season >= season_low) %>%
          filter(Season <= season_high) %>%
          filter(Age >= age_low) %>%
          filter(Age <= age_high) %>%
          filter({{column1}} <= cutoff1) %>%
          filter({{column2}} >= cutoff2)
        return(df$Name)
      }
    }
  } else {
    if(two == 'na'){
      df <- df %>%
        filter(Season >= season_low) %>%
        filter(Season <= season_high) %>%
        filter(Age >= age_low) %>%
        filter(Age <= age_high) %>%
        filter({{column1}} >= cutoff1)
      return(df$Name)
    } else {
      if (dir2 == 'l'){
        df <- df %>%
          filter(Season >= season_low) %>%
          filter(Season <= season_high) %>%
          filter(Age >= age_low) %>%
          filter(Age <= age_high) %>%
          filter({{column1}} >= cutoff1) %>%
          filter({{column2}} <= cutoff2)
        return(df$Name)
      } else {
        df <- df %>%
          filter(Season >= season_low) %>%
          filter(Season <= season_high) %>%
          filter(Age >= age_low) %>%
          filter(Age <= age_high) %>%
          filter({{column1}} >= cutoff1) %>%
          filter({{column2}} >= cutoff2)
        return(df$Name)
      }
    }
  }
}

## example: decent or better old pitchers with above avg fastball velocity
oldest <- cluster(fangraphs_stdz, xFIP, 'yes', FBv, 1, 0, 'l', 'g', 2002, 2019, 35, 50)




  