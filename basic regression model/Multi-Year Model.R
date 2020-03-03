library(tidyverse)

experience8 <- filter(data_by_experience, Season_1 != "NA" & Season_2 != "NA" & Season_3 != "NA" & Season_4 != "NA" & Season_5 != "NA" & Season_6 != "NA" & Season_7 != "NA" & Season_8 != "NA")

model1 <- lm(xFIP_6 ~ xFIP_1 + xFIP_2 + xFIP_3 + xFIP_4 + xFIP_5, data=experience8)
summary(model1)

model2 <- lm(xFIP_6 ~ xFIP_5, data=experience8)
summary(model2)

model3 <- lm(xFIP_7 ~ xFIP_1 + xFIP_2 + xFIP_3 + xFIP_4 + xFIP_5, data=experience8)
summary(model3)
