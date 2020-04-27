



meanFBv_by_age <- fangraphs_clean%>%
  group_by(Age) %>% 
  mutate(
    meanFBv = mean(FBv, na.rm = TRUE)
  )

ggplot(meanFBv_by_age, aes(x=Age, y=meanFBv)) + geom_point() + 
  ggtitle("Mean Fastball Velocity by Age") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 25, face = "bold")) +
  scale_x_continuous(
    breaks = seq(20, 40, 2),
    limits = c(20,40)
  ) + 
  scale_y_continuous(
    breaks = seq(88, 96, 0.5),
    limits = c(88, 96)
  )




meanFBv_by_year <- fangraphs_clean%>%
  group_by(Season) %>% 
  mutate(
    meanFBv = mean(FBv, na.rm = TRUE)
  )

ggplot(meanFBv_by_year, aes(x=Season, y=meanFBv)) + geom_point() + 
  ggtitle("Mean Fastball Velocity by Season") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 25, face = "bold")) +
  scale_x_continuous(
    breaks = seq(2000, 2020, 2),
    limits = c(2000, 2020)
  ) + 
  scale_y_continuous(
    breaks = seq(88, 93, 0.5),
    limits = c(88, 93)
  )
