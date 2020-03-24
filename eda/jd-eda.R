library(here)
here::here("data","import-wrangle.R") %>% 
  source()


fangraphs_stdz



## xFip clusters
topHalfxFip = fangraphs_stdz[fangraphs_stdz$xFIP < 0,]

botHalfxFip = fangraphs_stdz[fangraphs_stdz$xFIP > 0,]

top15Perc = fangraphs_stdz[fangraphs_stdz$xFIP < 1,]


## FBv clusters
topHalfFBv = fangraphs_stdz[fangraphs_stdz$FBv > 0,]

botHalfFBv = fangraphs_stdz[fangraphs_stdz$FBv < 0,]


## FB% clusters
topHalfFB. = fangraphs_stdz[fangraphs_stdz["FB%"] > 0,]

botHalfFB. = fangraphs_stdz[fangraphs_stdz["FB%"] < 0,]


## Split by time

EarlyYears = fangraphs_stdz[fangraphs_stdz$Season <= 2010,]
  
LateYears = fangraphs_stdz[fangraphs_stdz$Season > 2010,]








