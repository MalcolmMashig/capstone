library(tidyverse)
library(lubridate)

# importing raw data

data_raw <- read_csv('NEW Fangraphs.csv')

# cleaning percentage format

data_clean <- data_raw %>% 
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
    `BB%` = as.numeric(str_remove(`BB%`, " %"))
  ) %>% 
  arrange(Name, Season) %>% 
  group_by(Name) %>% 
  mutate(experience = row_number()) %>% 
  ungroup() 

# cleaning column names

names(data_clean)[13] <- "StartIP"
names(data_clean)[28] <- "Groundball%"
names(data_clean)[29] <- "Flyball%"
names(data_clean)[36] <- "Experience"

data_season <- select(data_clean, 1:4,36,6:7,13,5,11,14,12,15,27,34,8:10,25:26,28:29,31:32,16:24,30,35)
names(data_season)

# Team

season_Team <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Team, 
              names_prefix = "Team_")

season_Team <- select(season_Team, 1:2, 17, 3:15, 18:19, 16)

experience_Team <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = Team, 
              names_prefix = "Team_")

# GS

season_GS <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = GS, 
              names_prefix = "GS_")

season_GS <- select(season_GS, 1:2, 17, 3:15, 18:19, 16)

experience_GS <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = GS, 
              names_prefix = "GS_")

# IP

season_IP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = IP, 
              names_prefix = "IP_")

season_IP <- select(season_IP, 1:2, 17, 3:15, 18:19, 16)

experience_IP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = IP, 
              names_prefix = "IP_")

# Start-IP

season_StartIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = StartIP, 
              names_prefix = "StartIP_")

season_StartIP <- select(season_StartIP, 1:2, 17, 3:15, 18:19, 16)

experience_StartIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = StartIP, 
              names_prefix = "StartIP_")

# ERA

season_ERA <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = ERA, 
              names_prefix = "ERA_")

season_ERA <- select(season_ERA, 1:2, 17, 3:15, 18:19, 16)

experience_ERA <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = ERA, 
              names_prefix = "ERA_")

# WHIP

season_WHIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = WHIP, 
              names_prefix = "WHIP_")

season_WHIP <- select(season_WHIP, 1:2, 17, 3:15, 18:19, 16)

experience_WHIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = WHIP, 
              names_prefix = "WHIP_")

# WAR

season_WAR <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = WAR, 
              names_prefix = "WAR_")

season_WAR <- select(season_WAR, 1:2, 17, 3:15, 18:19, 16)

experience_WAR <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = WAR, 
              names_prefix = "WAR_")

# FIP

season_FIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FIP, 
              names_prefix = "FIP_")

season_FIP <- select(season_FIP, 1:2, 17, 3:15, 18:19, 16)

experience_FIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = FIP, 
              names_prefix = "FIP_")

# xFIP

season_xFIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = xFIP, 
              names_prefix = "xFIP_")

season_xFIP <- select(season_xFIP, 1:2, 17, 3:15, 18:19, 16)

experience_xFIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = xFIP, 
              names_prefix = "xFIP_")

# BABIP

season_BABIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = BABIP, 
              names_prefix = "BABIP_")

season_BABIP <- select(season_BABIP, 1:2, 17, 3:15, 18:19, 16)

experience_BABIP <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = BABIP, 
              names_prefix = "BABIP_")

# FRM

season_FRM <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FRM, 
              names_prefix = "FRM_")

season_FRM <- select(season_FRM, 1:2, 17, 3:15, 18:19, 16)

experience_FRM <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = FRM, 
              names_prefix = "FRM_")

# K/9

season_K9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `K/9`, 
              names_prefix = "K/9_")

season_K9 <- select(season_K9, 1:2, 17, 3:15, 18:19, 16)

experience_K9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `K/9`, 
              names_prefix = "K/9_")

# BB/9

season_BB9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `BB/9`, 
              names_prefix = "BB/9_")

season_BB9 <- select(season_BB9, 1:2, 17, 3:15, 18:19, 16)

experience_BB9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `BB/9`, 
              names_prefix = "BB/9_")

# K/BB

season_KBB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `K/BB`, 
              names_prefix = "K/BB_")

season_KBB <- select(season_KBB, 1:2, 17, 3:15, 18:19, 16)

experience_KBB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `K/BB`, 
              names_prefix = "K/BB_")

# H/9

season_H9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `H/9`, 
              names_prefix = "H/9_")

season_H9 <- select(season_H9, 1:2, 17, 3:15, 18:19, 16)

experience_H9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `H/9`, 
              names_prefix = "H/9_")

# HR/9

season_HR9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `HR/9`, 
              names_prefix = "HR/9_")

season_HR9 <- select(season_HR9, 1:2, 17, 3:15, 18:19, 16)

experience_HR9 <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `HR/9`, 
              names_prefix = "HR/9_")

# Groundball%

season_Groundball <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `Groundball%`, 
              names_prefix = "Groundball%_")

season_Groundball <- select(season_Groundball, 1:2, 17, 3:15, 18:19, 16)

experience_Groundball <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Groundball%`, 
              names_prefix = "Groundball%_")

# Flyball%

season_Flyball <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `Flyball%`, 
              names_prefix = "Flyball%_")

season_Flyball <- select(season_Flyball, 1:2, 17, 3:15, 18:19, 16)

experience_Flyball <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `Flyball%`, 
              names_prefix = "Flyball%_")

# K%

season_K <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `K%`, 
              names_prefix = "K%_")

season_K <- select(season_K, 1:2, 17, 3:15, 18:19, 16)

experience_K <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `K%`, 
              names_prefix = "K%_")

# BB%

season_BB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `BB%`, 
              names_prefix = "BB%_")

season_BB <- select(season_BB, 1:2, 17, 3:15, 18:19, 16)

experience_BB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `BB%`, 
              names_prefix = "BB%_")

# FB%

season_FB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

season_FB <- select(season_FB, 1:2, 17, 3:15, 18:19, 16)

experience_FB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `FB%`, 
              names_prefix = "FB%_")

# FBv

season_FBv <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = FBv, 
              names_prefix = "FBv_")

season_FBv <- select(season_FBv, 1:2, 17, 3:15, 18:19, 16)

experience_FBv <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = FBv, 
              names_prefix = "FBv_")

# wFB

season_wFB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = wFB, 
              names_prefix = "wFB_")

season_wFB <- select(season_wFB, 1:2, 17, 3:15, 18:19, 16)

experience_wFB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = wFB, 
              names_prefix = "wFB_")

# SL%

season_SL <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `SL%`, 
              names_prefix = "SL%_")

season_SL <- select(season_SL, 1:2, 17, 3:15, 18:19, 16)

experience_SL <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `SL%`, 
              names_prefix = "SL%_")

# CT%

season_CT <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `CT%`, 
              names_prefix = "CT%_")

season_CT <- select(season_CT, 1:2, 17, 3:15, 18:19, 16)

experience_CT <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `CT%`, 
              names_prefix = "CT%_")

# CB% 

season_CB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `CB%`, 
              names_prefix = "CB%_")

season_CB <- select(season_CB, 1:2, 17, 3:15, 18:19, 16)

experience_CB <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `CB%`, 
              names_prefix = "CB%_")

# CH%

season_CH <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `CH%`, 
              names_prefix = "CH%_")

season_CH <- select(season_CH, 1:2, 17, 3:15, 18:19, 16)

experience_CH <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `CH%`, 
              names_prefix = "CH%_")

# SF%

season_SF <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `SF%`, 
              names_prefix = "SF%_")

season_SF <- select(season_SF, 1:2, 17, 3:15, 18:19, 16)

experience_SF <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `SF%`, 
              names_prefix = "SF%_")

# KN% 

season_KN <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = `KN%`, 
              names_prefix = "KN%_")

season_KN <- select(season_KN, 1:2, 17, 3:15, 18:19, 16)

experience_KN <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = `KN%`, 
              names_prefix = "KN%_")

# Dollars

season_Dollars <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Season, 
              values_from = Dollars, 
              names_prefix = "Dollars_")

season_Dollars <- select(season_Dollars, 1:2, 17, 3:15, 18:19, 16)

experience_Dollars <- data_season %>% 
  pivot_wider(id_cols = Name, 
              names_from = Experience, 
              values_from = Dollars, 
              names_prefix = "Dollars_")

# Player Data by Year

data_player_year <- bind_cols(season_Team, season_GS, season_IP, season_StartIP, season_ERA, 
                              season_WHIP, season_WAR, season_FIP, season_xFIP, season_BABIP, 
                              season_FRM, season_K9, season_BB9, season_KBB, season_H9, season_HR9, 
                              season_Groundball, season_Flyball, season_K, season_BB, season_FB, 
                              season_FBv, season_wFB, season_SL, season_CT, season_CB, season_CH, 
                              season_SF, season_KN, season_Dollars)

# Remove intermediate data

remove(season_Team, season_GS, season_IP, season_StartIP, season_ERA, season_WHIP, season_WAR, 
       season_FIP, season_xFIP, season_BABIP, season_FRM, season_K9, season_BB9, season_KBB, 
       season_H9, season_HR9, season_Groundball, season_Flyball, season_K, season_BB, season_FB, 
       season_FBv, season_wFB, season_SL, season_CT, season_CB, season_CH, season_SF, season_KN, 
       season_Dollars)

# Player Data by Experience

data_player_experience <- bind_cols(experience_Team, experience_GS, experience_IP, experience_StartIP, 
                                    experience_ERA, experience_WHIP, experience_WAR, experience_FIP, 
                                    experience_xFIP, experience_BABIP, experience_FRM, experience_K9, 
                                    experience_BB9, experience_KBB, experience_H9, experience_HR9, 
                                    experience_Groundball, experience_Flyball, experience_K, 
                                    experience_BB, experience_FB, experience_FBv, experience_wFB, 
                                    experience_SL, experience_CT, experience_CB, experience_CH, 
                                    experience_SF, experience_KN, experience_Dollars)

# Remove intermediate data

remove(experience_Team, experience_GS, experience_IP, experience_StartIP, experience_ERA, 
       experience_WHIP, experience_WAR, experience_FIP, experience_xFIP, experience_BABIP, 
       experience_FRM, experience_K9, experience_BB9, experience_KBB, experience_H9, experience_HR9, 
       experience_Groundball, experience_Flyball, experience_K, experience_BB, experience_FB, 
       experience_FBv, experience_wFB, experience_SL, experience_CT, experience_CB, experience_CH, 
       experience_SF, experience_KN, experience_Dollars)

