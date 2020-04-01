# run import-r

df29 <- filter(fangraphs_stdz, Age == 29)
df30 <- filter(fangraphs_stdz, Age == 30)
common2 <- intersect(df29$Name, df30$Name)
df29 <- filter(df29, Name %in% common2)
df30 <- filter(df30, Name %in% common2)
df29$xFIP2 <- df30$xFIP
df29 <- select(df29, c('Name', 'xFIP', 'xFIP2'))
change <- df29$xFIP2 - df29$xFIP
change

samp <- sample(change, size=10000, replace=TRUE)
quantile(samp, 0.05)
quantile(samp, 0.95)

df30 <- filter(fangraphs_stdz, Age == 30)
df31 <- filter(fangraphs_stdz, Age == 31)
common2 <- intersect(df30$Name, df31$Name)
df30 <- filter(df30, Name %in% common2)
df31 <- filter(df31, Name %in% common2)
df30$xFIP2 <- df31$xFIP
df30 <- select(df30, c('Name', 'xFIP', 'xFIP2'))
change2 <- df30$xFIP2 - df30$xFIP
change2

samp2 <- sample(change2, size=10000, replace=TRUE)
quantile(samp2, 0.05)
quantile(samp2, 0.95)







