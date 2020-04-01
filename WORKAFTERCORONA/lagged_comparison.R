# predicting 2019 xFIP from 2018 stats

data_18_19 <- filter(data_by_year, Team_2018 != "NA" & Team_2019 != "NA")

model1 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018`, data=data_18_19)
summary(model1)

# predicting 2019 xFIP from 2017 stats

data_17_19 <- filter(data_by_year, Team_2017 != "NA" & Team_2019 != "NA")

model2 <- lm(xFIP_2019 ~ Age_2017 + xFIP_2017 + FBv_2017 + `FB%_2017`, data=data_17_19)
summary(model2) 

# predicting 2014 xFIP from 2013 stats

data_13_14 <- filter(data_by_year, Team_2013 != "NA" & Team_2014 != "NA")

model3 <- lm(xFIP_2014 ~ Age_2013 + xFIP_2013 + FBv_2013 + `FB%_2013`, data=data_13_14)
summary(model3)

# predicting 2014 xFIP from 2012 stats

data_12_14 <- filter(data_by_year, Team_2012 != "NA" & Team_2014 != "NA")

model4 <- lm(xFIP_2014 ~ Age_2012 + xFIP_2012 + FBv_2012 + `FB%_2012`, data=data_12_14)
summary(model4) 

# predicting 2009 xFIP from 2008 stats

data_08_09 <- filter(data_by_year, Team_2008 != "NA" & Team_2009 != "NA")

model5 <- lm(xFIP_2009 ~ Age_2008 + xFIP_2008 + FBv_2008 + `FB%_2008`, data=data_08_09)
summary(model5)

# predicting 2009 xFIP from 2007 stats

data_07_09 <- filter(data_by_year, Team_2007 != "NA" & Team_2009 != "NA")

model6 <- lm(xFIP_2009 ~ Age_2007 + xFIP_2007 + FBv_2007 + `FB%_2007`, data=data_07_09)
summary(model6)

# predicting 2005 xFIP from 2004 stats

data_04_05 <- filter(data_by_year, Team_2004 != "NA" & Team_2005 != "NA")

model7 <- lm(xFIP_2005 ~ Age_2004 + xFIP_2004 + FBv_2004 + `FB%_2004`, data=data_04_05)
summary(model7)

# predicting 2005 xFIP from 2003 stats

data_03_05 <- filter(data_by_year, Team_2003 != "NA" & Team_2005 != "NA")

model8 <- lm(xFIP_2005 ~ Age_2003 + xFIP_2003 + FBv_2003 + `FB%_2003`, data=data_03_05)
summary(model8)
