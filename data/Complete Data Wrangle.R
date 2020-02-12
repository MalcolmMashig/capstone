# install.packages('tidyverse')
library(tidyverse)

# install.packages('lubridate')
library(lubridate)

# install.packages('here')
library(here)

# importing raw data

pitchers_raw <- here::here('data', 'Complete Fangraphs.csv') %>% 
  read_csv() %>% 
  select(playerid)

careers_raw <- here::here('data', 'Full Careers.csv') %>% 
  read_csv() %>% 
  semi_join(pitchers_raw)

# cleaning percentage format 

careers_clean <- careers_raw %>% 
  mutate(
    `FB%` = as.numeric(str_remove(`FB%`, " %")),
    `SL%` = as.numeric(str_remove(`SL%`, " %")),
    `CT%` = as.numeric(str_remove(`CT%`, " %")),
    `CB%` = as.numeric(str_remove(`CB%`, " %")),
    `CH%` = as.numeric(str_remove(`CH%`, " %")),
    `SF%` = as.numeric(str_remove(`SF%`, " %")),
    `KN%` = as.numeric(str_remove(`KN%`, " %")),
    `GB%` = as.numeric(str_remove(`GB%`, " %")),
    `FB%_1` = as.numeric(str_remove(`FB%_1`, " %")),
    `K%` = as.numeric(str_remove(`K%`, " %")),
    `BB%` = as.numeric(str_remove(`BB%`, " %")),
  ) %>% 
  arrange(Name, Season) %>% 
  group_by(Name) %>% 
  # Add variables for experience (numbers of years meeting minimum use) and
  # seasons (number of years in league)
  mutate(Experience = row_number(),
         Seasons = Season - min(Season) + 1) %>% 
  ungroup()

remove(pitchers_raw, careers_raw)

# cleaning column names

names(careers_clean)[28] <- "Groundball%"
names(careers_clean)[29] <- "Flyball%"
names(careers_clean)[35] <- "PlayerID"
names(careers_clean)

# cleaning column order

careers_clean <- select(careers_clean, 2,1,4,36:37,3,6:7,13,5,11,14,12,15,27,34,8:10,25:26,28:29,31:32,16:24,30,35)  
names(careers_clean)

## data by season (year)

# Age

Season_Age <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Age, 
              names_prefix = "Age_")

Season_Age <- select(Season_Age, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Experience

Season_Experience <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Experience, 
              names_prefix = "Experience_")

Season_Experience <- select(Season_Experience, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Seasons

Season_Seasons <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Seasons, 
              names_prefix = "Seasons_")

Season_Seasons <- select(Season_Seasons, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Team

Season_Team <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Team, 
              names_prefix = "Team_")

Season_Team <- select(Season_Team, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# GS

Season_GS <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = GS, 
              names_prefix = "GS_")

Season_GS <- select(Season_GS, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# IP

Season_IP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = IP, 
              names_prefix = "IP_")

Season_IP <- select(Season_IP, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Start-IP

Season_StartIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `Start-IP`, 
              names_prefix = "Start-IP_")

Season_StartIP <- select(Season_StartIP, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# ERA

Season_ERA <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = ERA, 
              names_prefix = "ERA_")

Season_ERA <- select(Season_ERA, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# WHIP

Season_WHIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = WHIP, 
              names_prefix = "WHIP_")

Season_WHIP <- select(Season_WHIP, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# WAR

Season_WAR <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = WAR, 
              names_prefix = "WAR_")

Season_WAR <- select(Season_WAR, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# FIP

Season_FIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FIP, 
              names_prefix = "FIP_")

Season_FIP <- select(Season_FIP, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# xFIP

Season_xFIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = xFIP, 
              names_prefix = "xFIP_")

Season_xFIP <- select(Season_xFIP, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# BABIP

Season_BABIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = BABIP, 
              names_prefix = "BABIP_")

Season_BABIP <- select(Season_BABIP, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# FRM

Season_FRM <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FRM, 
              names_prefix = "FRM_")

Season_FRM <- select(Season_FRM, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# K/9

Season_K9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `K/9`, 
              names_prefix = "K/9_")

Season_K9 <- select(Season_K9, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# BB/9

Season_BB9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `BB/9`, 
              names_prefix = "BB/9_")

Season_BB9 <- select(Season_BB9, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# K/BB

Season_KBB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `K/BB`, 
              names_prefix = "K/BB_")

Season_KBB <- select(Season_KBB, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# H/9

Season_H9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `H/9`, 
              names_prefix = "H/9_")

Season_H9 <- select(Season_H9, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# HR/9

Season_HR9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `HR/9`, 
              names_prefix = "HR/9_")

Season_HR9 <- select(Season_HR9, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Groundball%

Season_Groundball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `Groundball%`, 
              names_prefix = "Groundball%_")

Season_Groundball <- select(Season_Groundball, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Flyball%

Season_Flyball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `Flyball%`, 
              names_prefix = "Flyball%_")

Season_Flyball <- select(Season_Flyball, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# K%

Season_K <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `K%`, 
              names_prefix = "K%_")

Season_K <- select(Season_K, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# BB%

Season_BB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `BB%`, 
              names_prefix = "BB%_")

Season_BB <- select(Season_BB, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# FB% 

Season_FB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

Season_FB <- select(Season_FB, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# FBv

Season_FBv <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FBv, 
              names_prefix = "FBv_")

Season_FBv <- select(Season_FBv, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# wFB

Season_wFB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = wFB, 
              names_prefix = "wFB_")

Season_wFB <- select(Season_wFB, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# SL%

Season_SL <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `SL%`, 
              names_prefix = "SL%_")

Season_SL <- select(Season_SL, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# CT%

Season_CT <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `CT%`, 
              names_prefix = "CT%_")

Season_CT <- select(Season_CT, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# CB%

Season_CB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `CB%`, 
              names_prefix = "CB%_")

Season_CB <- select(Season_CB, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# CH%

Season_CH <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `CH%`, 
              names_prefix = "CH%_")

Season_CH <- select(Season_CH, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# SF%

Season_SF <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `SF%`, 
              names_prefix = "SF%_")

Season_SF <- select(Season_SF, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# KN%

Season_KN <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `KN%`, 
              names_prefix = "KN%_")

Season_KN <- select(Season_KN, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

# Dollars

Season_Dollars <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Dollars, 
              names_prefix = "Dollars_")

Season_Dollars <- select(Season_Dollars, 1,36:37,35,29:31,34,32:33,23:28,2:20,22,21)

data_by_year <- bind_cols(Season_Age, Season_Experience, Season_Seasons, Season_Team, Season_GS, 
                            Season_IP, Season_StartIP, Season_ERA, Season_WHIP, Season_WAR, Season_FIP, 
                            Season_xFIP, Season_BABIP, Season_FRM, Season_K9, Season_BB9, Season_KBB, 
                            Season_H9, Season_HR9,Season_Groundball, Season_Flyball, Season_K, Season_BB, 
                            Season_FB,Season_FBv, Season_wFB, Season_SL, Season_CT, Season_CB, Season_CH, 
                            Season_SF, Season_KN, Season_Dollars)

remove(Season_Age, Season_Experience, Season_Seasons, Season_Team, Season_GS, 
       Season_IP, Season_StartIP, Season_ERA, Season_WHIP, Season_WAR, Season_FIP, 
       Season_xFIP, Season_BABIP, Season_FRM, Season_K9, Season_BB9, Season_KBB, 
       Season_H9, Season_HR9,Season_Groundball, Season_Flyball, Season_K, Season_BB, 
       Season_FB,Season_FBv, Season_wFB, Season_SL, Season_CT, Season_CB, Season_CH, 
       Season_SF, Season_KN, Season_Dollars)

## data by age

# Season

Age_Season <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Season`, 
              names_prefix = "Season_")

Age_Season <- select(Age_Season, 1,28,27,19,2:18,20:26,29,30,31)


# Experience

Age_Experience <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Experience`, 
              names_prefix = "Experience_")

Age_Experience <- select(Age_Experience, 1,28,27,19,2:18,20:26,29,30,31)

# Seasons

Age_Seasons <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Seasons`, 
              names_prefix = "Seasons_")

Age_Seasons <- select(Age_Seasons, 1,28,27,19,2:18,20:26,29,30,31)

# Team

Age_Team <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Team`, 
              names_prefix = "Team_")

Age_Team <- select(Age_Team, 1,28,27,19,2:18,20:26,29,30,31)

# GS

Age_GS <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `GS`, 
              names_prefix = "GS_")

Age_GS <- select(Age_GS, 1,28,27,19,2:18,20:26,29,30,31)

# IP

Age_IP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `IP`, 
              names_prefix = "IP_")

Age_IP <- select(Age_IP, 1,28,27,19,2:18,20:26,29,30,31)

# Start-IP

Age_StartIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Start-IP`, 
              names_prefix = "Start-IP_")

Age_StartIP <- select(Age_StartIP, 1,28,27,19,2:18,20:26,29,30,31)

# ERA

Age_ERA <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `ERA`, 
              names_prefix = "ERA_")

Age_ERA <- select(Age_ERA, 1,28,27,19,2:18,20:26,29,30,31)

# WHIP

Age_WHIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `WHIP`, 
              names_prefix = "WHIP_")

Age_WHIP <- select(Age_WHIP, 1,28,27,19,2:18,20:26,29,30,31)

# WAR

Age_WAR <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `WAR`, 
              names_prefix = "WAR_")

Age_WAR <- select(Age_WAR, 1,28,27,19,2:18,20:26,29,30,31)

# FIP

Age_FIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `FIP`, 
              names_prefix = "FIP_")

Age_FIP <- select(Age_FIP, 1,28,27,19,2:18,20:26,29,30,31)

# xFIP

Age_xFIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `xFIP`, 
              names_prefix = "xFIP_")

Age_xFIP <- select(Age_xFIP, 1,28,27,19,2:18,20:26,29,30,31)

# BABIP

Age_BABIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `BABIP`, 
              names_prefix = "BABIP_")

Age_BABIP <- select(Age_BABIP, 1,28,27,19,2:18,20:26,29,30,31)

# FRM

Age_FRM <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `FRM`, 
              names_prefix = "FRM_")

Age_FRM <- select(Age_FRM, 1,28,27,19,2:18,20:26,29,30,31)

# K/9

Age_K9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `K/9`, 
              names_prefix = "K/9_")

Age_K9 <- select(Age_K9, 1,28,27,19,2:18,20:26,29,30,31)

# BB/9

Age_BB9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `BB/9`, 
              names_prefix = "BB/9_")

Age_BB9 <- select(Age_BB9, 1,28,27,19,2:18,20:26,29,30,31)

# K/BB

Age_KBB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `K/BB`, 
              names_prefix = "K/BB_")

Age_KBB <- select(Age_KBB, 1,28,27,19,2:18,20:26,29,30,31)

# H/9

Age_H9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `H/9`, 
              names_prefix = "H/9_")

Age_H9 <- select(Age_H9, 1,28,27,19,2:18,20:26,29,30,31)

# HR/9

Age_HR9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `HR/9`, 
              names_prefix = "HR/9_")

Age_HR9 <- select(Age_HR9, 1,28,27,19,2:18,20:26,29,30,31)

# Groundball%

Age_Groundball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Groundball%`, 
              names_prefix = "Groundball%_")

Age_Groundball <- select(Age_Groundball, 1,28,27,19,2:18,20:26,29,30,31)

# Flyball%

Age_Flyball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Flyball%`, 
              names_prefix = "Flyball%_")

Age_Flyball <- select(Age_Flyball, 1,28,27,19,2:18,20:26,29,30,31)

# K%

Age_K <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `K%`, 
              names_prefix = "K%_")

Age_K <- select(Age_K, 1,28,27,19,2:18,20:26,29,30,31)

# BB%

Age_BB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `BB%`, 
              names_prefix = "BB%_")

Age_BB <- select(Age_BB, 1,28,27,19,2:18,20:26,29,30,31)

# FB% 

Age_FB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

Age_FB <- select(Age_FB, 1,28,27,19,2:18,20:26,29,30,31)

# FBv

Age_FBv <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `FBv`, 
              names_prefix = "FBv_")

Age_FBv <- select(Age_FBv, 1,28,27,19,2:18,20:26,29,30,31)

# wFB

Age_wFB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `wFB`, 
              names_prefix = "wFB_")

Age_wFB <- select(Age_wFB, 1,28,27,19,2:18,20:26,29,30,31)

# SL%

Age_SL <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `SL%`, 
              names_prefix = "SL%_")

Age_SL <- select(Age_SL, 1,28,27,19,2:18,20:26,29,30,31)

# CT%

Age_CT <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `CT%`, 
              names_prefix = "CT%_")

Age_CT <- select(Age_CT, 1,28,27,19,2:18,20:26,29,30,31)

# CB%

Age_CB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `CB%`, 
              names_prefix = "CB%_")

Age_CB <- select(Age_CB, 1,28,27,19,2:18,20:26,29,30,31)

# CH%

Age_CH <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `CH%`, 
              names_prefix = "CH%_")

Age_CH <- select(Age_CH, 1,28,27,19,2:18,20:26,29,30,31)

# SF%

Age_SF <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `SF%`, 
              names_prefix = "SF%_")

Age_SF <- select(Age_SF, 1,28,27,19,2:18,20:26,29,30,31)

# KN%

Age_KN <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `KN%`, 
              names_prefix = "KN%_")

Age_KN <- select(Age_KN, 1,28,27,19,2:18,20:26,29,30,31)

# Dollars

Age_Dollars <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Age, 
              values_from = `Dollars`, 
              names_prefix = "Dollars_")

Age_Dollars <- select(Age_Dollars, 1,28,27,19,2:18,20:26,29,30,31)

data_by_age <- bind_cols(Age_Season, Age_Experience, Age_Seasons, Age_Team, Age_GS, Age_IP, Age_StartIP, 
                         Age_ERA, Age_WHIP, Age_WAR, Age_FIP, Age_xFIP, Age_BABIP, Age_FRM, Age_K9, Age_BB9, 
                         Age_KBB, Age_H9, Age_HR9, Age_Groundball, Age_Flyball, Age_K, Age_BB, Age_FB, 
                         Age_FBv, Age_wFB, Age_SL, Age_CT, Age_CB, Age_CH, Age_SF, Age_KN, Age_Dollars)

remove(Age_Season, Age_Experience, Age_Seasons, Age_Team, Age_GS, Age_IP, Age_StartIP, Age_ERA, Age_WHIP, 
       Age_WAR, Age_FIP, Age_xFIP, Age_BABIP, Age_FRM, Age_K9, Age_BB9, Age_KBB, Age_H9, Age_HR9, 
       Age_Groundball, Age_Flyball, Age_K, Age_BB, Age_FB, Age_FBv, Age_wFB, Age_SL, Age_CT, Age_CB, 
       Age_CH, Age_SF, Age_KN, Age_Dollars)

## data by experience

# Season

Experience_Season <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Season`, 
              names_prefix = "Season_")

# Age

Experience_Age <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Age`, 
              names_prefix = "Age_")

# Seasons

Experience_Seasons <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Seasons`, 
              names_prefix = "Seasons_")

# Team

Experience_Team <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Team`, 
              names_prefix = "Team_")

# GS

Experience_GS <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `GS`, 
              names_prefix = "GS_")

# IP

Experience_IP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `IP`, 
              names_prefix = "IP_")

# Start-IP

Experience_StartIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Start-IP`, 
              names_prefix = "Start-IP_")

# ERA

Experience_ERA <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `ERA`, 
              names_prefix = "ERA_")

# WHIP

Experience_WHIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `WHIP`, 
              names_prefix = "WHIP_")

# WAR

Experience_WAR <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `WAR`, 
              names_prefix = "WAR_")

# FIP

Experience_FIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `FIP`, 
              names_prefix = "FIP_")

# xFIP

Experience_xFIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `xFIP`, 
              names_prefix = "xFIP_")

# BABIP

Experience_BABIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `BABIP`, 
              names_prefix = "BABIP_")

# FRM

Experience_FRM <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `FRM`, 
              names_prefix = "FRM_")

# K/9

Experience_K9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `K/9`, 
              names_prefix = "K/9_")

# BB/9

Experience_BB9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `BB/9`, 
              names_prefix = "BB/9_")

# K/BB

Experience_KBB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `K/BB`, 
              names_prefix = "K/BB_")

# H/9

Experience_H9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `H/9`, 
              names_prefix = "H/9_")

# HR/9

Experience_HR9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `HR/9`, 
              names_prefix = "HR/9_")

# Groundball%

Experience_Groundball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Groundball%`, 
              names_prefix = "Groundball%_")

# Flyball%

Experience_Flyball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Flyball%`, 
              names_prefix = "Flyball%_")

# K%

Experience_K <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `K%`, 
              names_prefix = "K%_")

# BB%

Experience_BB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `BB%`, 
              names_prefix = "BB%_")

# FB% 

Experience_FB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

# FBv

Experience_FBv <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `FBv`, 
              names_prefix = "FBv_")

# wFB

Experience_wFB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `wFB`, 
              names_prefix = "wFB_")

# SL%

Experience_SL <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `SL%`, 
              names_prefix = "SL%_")

# CT%

Experience_CT <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `CT%`, 
              names_prefix = "CT%_")

# CB%

Experience_CB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `CB%`, 
              names_prefix = "CB%_")

# CH%

Experience_CH <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `CH%`, 
              names_prefix = "CH%_")

# SF%

Experience_SF <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `SF%`, 
              names_prefix = "SF%_")

# KN%

Experience_KN <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `KN%`, 
              names_prefix = "KN%_")

# Dollars

Experience_Dollars <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Dollars`, 
              names_prefix = "Dollars_")

data_by_experience <- bind_cols(Experience_Season, Experience_Age, Experience_Seasons, Experience_Team, 
                                Experience_GS, Experience_IP, Experience_StartIP, Experience_ERA, 
                                Experience_WHIP, Experience_WAR, Experience_FIP, Experience_xFIP, 
                                Experience_BABIP, Experience_FRM, Experience_K9, Experience_BB9, 
                                Experience_KBB, Experience_H9, Experience_HR9, Experience_Groundball, 
                                Experience_Flyball, Experience_K, Experience_BB, Experience_FB, 
                                Experience_FBv, Experience_wFB, Experience_SL, Experience_CT, Experience_CB, 
                                Experience_CH, Experience_SF, Experience_KN, Experience_Dollars)

remove(Experience_Season, Experience_Age, Experience_Seasons, Experience_Team, Experience_GS, Experience_IP, 
       Experience_StartIP, Experience_ERA, Experience_WHIP, Experience_WAR, Experience_FIP, Experience_xFIP, 
       Experience_BABIP, Experience_FRM, Experience_K9, Experience_BB9, Experience_KBB, Experience_H9, 
       Experience_HR9, Experience_Groundball, Experience_Flyball, Experience_K, Experience_BB, Experience_FB, 
       Experience_FBv, Experience_wFB, Experience_SL, Experience_CT, Experience_CB, Experience_CH, 
       Experience_SF, Experience_KN, Experience_Dollars)

## data by seasons played

# Season

Seasons_Season <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Season`, 
              names_prefix = "Season_")

# Age

Seasons_Age <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Age`, 
              names_prefix = "Age_")

# Experience

Seasons_Experience <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Experience`, 
              names_prefix = "Experience_")

# Team

Seasons_Team <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Team`, 
              names_prefix = "Team_")

# GS

Seasons_GS <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `GS`, 
              names_prefix = "GS_")

# IP

Seasons_IP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `IP`, 
              names_prefix = "IP_")

# Start-IP

Seasons_StartIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Start-IP`, 
              names_prefix = "Start-IP_")

# ERA

Seasons_ERA <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `ERA`, 
              names_prefix = "ERA_")

# WHIP

Seasons_WHIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `WHIP`, 
              names_prefix = "WHIP_")

# WAR

Seasons_WAR <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `WAR`, 
              names_prefix = "WAR_")

# FIP

Seasons_FIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `FIP`, 
              names_prefix = "FIP_")

# xFIP

Seasons_xFIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `xFIP`, 
              names_prefix = "xFIP_")

# BABIP

Seasons_BABIP <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `BABIP`, 
              names_prefix = "BABIP_")

# FRM

Seasons_FRM <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `FRM`, 
              names_prefix = "FRM_")

# K/9

Seasons_K9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `K/9`, 
              names_prefix = "K/9_")

# BB/9

Seasons_BB9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `BB/9`, 
              names_prefix = "BB/9_")

# K/BB

Seasons_KBB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `K/BB`, 
              names_prefix = "K/BB_")

# H/9

Seasons_H9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `H/9`, 
              names_prefix = "H/9_")

# HR/9

Seasons_HR9 <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `HR/9`, 
              names_prefix = "HR/9_")

# Groundball%

Seasons_Groundball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Groundball%`, 
              names_prefix = "Groundball%_")

# Flyball%

Seasons_Flyball <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Flyball%`, 
              names_prefix = "Flyball%_")

# K%

Seasons_K <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `K%`, 
              names_prefix = "K%_")

# BB%

Seasons_BB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `BB%`, 
              names_prefix = "BB%_")

# FB% 

Seasons_FB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

# FBv

Seasons_FBv <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `FBv`, 
              names_prefix = "FBv_")

# wFB

Seasons_wFB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `wFB`, 
              names_prefix = "wFB_")

# SL%

Seasons_SL <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `SL%`, 
              names_prefix = "SL%_")

# CT%

Seasons_CT <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `CT%`, 
              names_prefix = "CT%_")

# CB%

Seasons_CB <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `CB%`, 
              names_prefix = "CB%_")

# CH%

Seasons_CH <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `CH%`, 
              names_prefix = "CH%_")

# SF%

Seasons_SF <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `SF%`, 
              names_prefix = "SF%_")

# KN%

Seasons_KN <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `KN%`, 
              names_prefix = "KN%_")

# Dollars

Seasons_Dollars <- careers_clean %>% 
  pivot_wider(id_cols = Name, 
              names_from = Seasons, 
              values_from = `Dollars`, 
              names_prefix = "Dollars_")

data_by_seasons <- bind_cols(Seasons_Season, Seasons_Age, Seasons_Experience, Seasons_Team, Seasons_GS, Seasons_IP, 
                Seasons_StartIP, Seasons_ERA, Seasons_WHIP, Seasons_WAR, Seasons_FIP, Seasons_xFIP, 
                Seasons_BABIP, Seasons_FRM, Seasons_K9, Seasons_BB9, Seasons_KBB, Seasons_H9, Seasons_HR9, 
                Seasons_Groundball, Seasons_Flyball, Seasons_K, Seasons_BB, Seasons_FB, Seasons_FBv, 
                Seasons_wFB, Seasons_SL, Seasons_CT, Seasons_CB, Seasons_CH, Seasons_SF, Seasons_KN, 
                Seasons_Dollars)

remove(Seasons_Season, Seasons_Age, Seasons_Experience, Seasons_Team, Seasons_GS, Seasons_IP, 
       Seasons_StartIP, Seasons_ERA, Seasons_WHIP, Seasons_WAR, Seasons_FIP, Seasons_xFIP, Seasons_BABIP, 
       Seasons_FRM, Seasons_K9, Seasons_BB9, Seasons_KBB, Seasons_H9, Seasons_HR9, Seasons_Groundball, 
       Seasons_Flyball, Seasons_K, Seasons_BB, Seasons_FB, Seasons_FBv, Seasons_wFB, Seasons_SL, Seasons_CT, 
       Seasons_CB, Seasons_CH, Seasons_SF, Seasons_KN, Seasons_Dollars)


