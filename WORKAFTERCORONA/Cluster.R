# Need to run import-wrangle.r file to get standardized vars

## Function to cluster based off of one column - returns subset of data_by_age dataframe
## CHANGE COLUMN WITIHN FUNCTION
# cutoff = std cutoff for the cluster (0 for 50th percentile, 1 for ~85th percentile, 2 for ~98th percentile)
# direction = l for less than the cutoff, anything else for greater than the cutoff
# season_low = enter first season to get data for (starts at 2002)
# season_high = enter last season to get data for
# age_low = enter lowest player age to get data for
# age_high = enter highest player age to get data for
cluster1 <- function(cutoff, dir, season_low, season_high, age_low, age_high){
  df <- filter(fangraphs_stdz, Season >= season_low)
  df <- filter(df, Season <= season_high)
  df <- filter(df, Age >= age_low)
  df <- filter(df, Age <= age_high)
  if (dir == 'l'){
    df <- filter(df, xFIP <= cutoff)  # CHANGE IF DIFF COLUMN
  } else {
    df <- filter(df, xFIP >= cutoff) # CHANGE IF DIFF COLUMN
    }
  names <- df$Name
  ret <- filter(data_by_age, Name %in% names)
  return(ret)
} 

cluster2 <- function(cutoff, dir, season_low, season_high, age_low, age_high){
  df <- filter(fangraphs_stdz, Season >= season_low)
  df <- filter(df, Season <= season_high)
  df <- filter(df, Age >= age_low)
  df <- filter(df, Age <= age_high)
  if (dir == 'l'){
    df <- filter(df, FBv <= cutoff)
    df <- filter(df, `FB%` <= cutoff)
  } else {
    df <- filter(df, FBv >= cutoff)
    df <- filter(df, `FB%` >= cutoff)
  }
  names <- df$Name
  ret <- filter(data_by_age, Name %in% names)
  return(ret)
} 

## REMEMBER TO CHANGE COLUMN NAMES IN ABOVE FUNCTIONS

## above average in fastball % and FBv since 2010
power <- cluster2(0, 'g', 2010, 2019, 0, 100) ## (n = 222, a lot of guys)

## above 1 std in fastball % and FBv since 2002
xtra_power <- cluster2(1, 'g', 2002, 2019, 0, 100) ## (n = 44, pretty small)

## pitchers who registered high velocity in their 30's
old_power <- cluster1(1, 'g', 2002, 2019, 30, 50) ## pretty small sample! (22 pitchers)

## The best pitchers 
best <- cluster1(-2, 'l', 2002, 2019, 15, 50) ## n = 39
best$Name ## names for analysis

## Pitchers still pitching at a reasonable level 35+
oldest <- cluster1(1, 'l', 2002, 2019, 35, 50) ## n = 68

## gonna run into sample size issues pretty quicky - cluster analysis may have to focus on broader clusters,
## or the entire range of years

## big question: how to turn clusters into analysis? 



  