#setwd('/Users/christianrogers/Desktop/capstone/data')

data_0203 <- filter(data_player_year, Age_2002 != "NA" & Age_2003 != "NA")

# trying same stats as 18-19 regression
model1 <- lm(xFIP_2003 ~ Age_2002 + xFIP_2002 + FBv_2002, data=data_0203)

# x-fip by far the most useful, fastball velocity means nothing?
summary(model1)

# all variables
model2 <- lm(xFIP_2003 ~ Age_2002 + GS_2002 + IP_2002 + StartIP_2002 + ERA_2002 + WHIP_2002 + WAR_2002 
             + xFIP_2002 + BABIP_2002 + `K/9_2002` + `BB/9_2002` + `K/BB_2002` + `H/9_2002` 
             + `HR/9_2002` + `Groundball%_2002` + `Flyball%_2002` + `K%_2002` + `BB%_2002` + `FB%_2002`
             + FBv_2002 + wFB_2002, data=data_0203)
summary(model2)

# performane metrics
model3 <- lm(xFIP_2003 ~ Age_2002 + GS_2002 + IP_2002 + StartIP_2002 + ERA_2002 + WHIP_2002 + WAR_2002 
              + xFIP_2002 + BABIP_2002, data=data_0203)
summary(model3)
#xFIP and War the only significant metrics

# Rates
model4 <- lm(xFIP_2003  ~ `K/9_2002` + `BB/9_2002` + `K/BB_2002` + `H/9_2002` + `HR/9_2002`, data=data_0203)
summary(model4)
## k/9 and hr/9 significant, model not as good

# Percentages
model5 <- lm(xFIP_2003 ~ `Groundball%_2002` + `Flyball%_2002` + `K%_2002` + `BB%_2002` + `FB%_2002`, data=data_0203)
summary(model5)
# K%, walk%, flyball%

# all significant vars
model6 <- lm(xFIP_2003 ~ WAR_2002 + xFIP_2002 + `K/9_2002` + `HR/9_2002` + `Flyball%_2002` + `K%_2002` + `BB%_2002`, data=data_0203)
summary(model6)
# nothing significant!!

#take out k% since k/9 already in
model7 <- lm(xFIP_2003 ~ WAR_2002 + xFIP_2002 + `K/9_2002` + `HR/9_2002` + `Flyball%_2002` + `BB%_2002`, data=data_0203)
summary(model7)
# still nothing significant

#splaying around
model8 <- lm(xFIP_2003 ~ Age_2002 + GS_2002 + ERA_2002 + WHIP_2002 + WAR_2002 
                + xFIP_2002 + BABIP_2002 + `K/9_2002` + `BB/9_2002` + `K/BB_2002` + `H/9_2002` 
                + `HR/9_2002` + `K%_2002` + `BB%_2002` + `FB%_2002`
                + FBv_2002, data=data_0203)
summary(model8)

model9 <- lm(xFIP_2003 ~ xFIP_2002 + `FB%_2002`, data=data_0203)
summary(model9)
## Takeaway: Fastball velocity doesn't seem to matter much at all for this period


#######################
# two year prediction #
#######################

data_0204 <- filter(data_player_year, Age_2002 != "NA" & Age_2004 != "NA")

# start with same regression as one-year prediction
model10 <- lm(xFIP_2004 ~ xFIP_2002 + `FB%_2002`, data=data_0204)
summary(model10)
# still significant, not as predictive though

# trying to add some more variables
model11 <- lm(xFIP_2004 ~ xFIP_2002 + `FB%_2002`  + `K/9_2002`, data=data_0204)
summary(model11)
# adding k/9 helps, Fastball velocity not important




