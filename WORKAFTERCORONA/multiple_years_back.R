# predicting 2019 xFIP from 2018 stats

data_18_19 <- filter(data_by_year, Team_2018 != "NA" & Team_2019 != "NA")

model1 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018`, data=data_18_19)
summary(model1)

# predicting 2019 xFIP from 2018 and 2017 stats

data_17_19 <- filter(data_by_year, Team_2017 != "NA" & Team_2018 != "NA" & Team_2019 != "NA")

model2 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018` + Age_2017 + xFIP_2017 + FBv_2017 + `FB%_2017`, data=data_17_19)
summary(model2) # better

# predicting 2019 xFIP from 2018 and 2017 and 2016 stats

data_16_19 <- filter(data_by_year, Team_2016 != "NA" & Team_2017 != "NA" & Team_2018 != "NA" & Team_2019 != "NA")

model3 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018` + Age_2017 + xFIP_2017 + FBv_2017 + `FB%_2017` + Age_2016 + xFIP_2016 + FBv_2016 + `FB%_2016`, data=data_16_19)
summary(model3) # better

# predicting 2019 xFIP from 2018 and 2017 and 2016 and 2015 stats

data_15_19 <- filter(data_by_year, Team_2015 != "NA" & Team_2016 != "NA" & Team_2017 != "NA" & Team_2018 != "NA" & Team_2019 != "NA")

model4 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018` + Age_2017 + xFIP_2017 + FBv_2017 + `FB%_2017` + Age_2016 + xFIP_2016 + FBv_2016 + `FB%_2016` + Age_2015 + xFIP_2015 + FBv_2015 + `FB%_2015`, data=data_15_19)
summary(model4) # better

# predicting 2019 xFIP from 2018 and 2017 and 2016 and 2015 and 2014 stats

data_14_19 <- filter(data_by_year, Team_2014 != "NA" & Team_2015 != "NA" & Team_2016 != "NA" & Team_2017 != "NA" & Team_2018 != "NA" & Team_2019 != "NA")

model5 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018` + Age_2017 + xFIP_2017 + FBv_2017 + `FB%_2017` + Age_2016 + xFIP_2016 + FBv_2016 + `FB%_2016` + Age_2015 + xFIP_2015 + FBv_2015 + `FB%_2015` + Age_2014 + xFIP_2014 + FBv_2014 + `FB%_2014`, data=data_14_19)
summary(model5) # better

# predicting 2019 xFIP from 2018 and 2017 and 2016 and 2015 and 2014 and 2013 stats

data_13_19 <- filter(data_by_year, Team_2013 != "NA" & Team_2014 != "NA" & Team_2015 != "NA" & Team_2016 != "NA" & Team_2017 != "NA" & Team_2018 != "NA" & Team_2019 != "NA")

model6 <- lm(xFIP_2019 ~ Age_2018 + xFIP_2018 + FBv_2018 + `FB%_2018` + Age_2017 + xFIP_2017 + FBv_2017 + `FB%_2017` + Age_2016 + xFIP_2016 + FBv_2016 + `FB%_2016` + Age_2015 + xFIP_2015 + FBv_2015 + `FB%_2015` + Age_2014 + xFIP_2014 + FBv_2014 + `FB%_2014` + Age_2013 + xFIP_2013 + FBv_2013 + `FB%_2013`, data=data_13_19)
summary(model6) # worse

