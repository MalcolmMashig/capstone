



meanFBv_by_age <- fangraphs_clean%>%
  group_by(Age) %>% 
  mutate(
    meanFBv = mean(FBv, na.rm = TRUE)
  )

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 8c1575b513e2fe243a65541265fabce61e4fae1f
ggplot(meanFBv_by_age, aes(x=Age, y=meanFBv)) + geom_line(color='blue') + geom_point(color='blue') + 
    labs(title="Mean Fastball Velocity by Age", y="Mean Fastball Velocity") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 16, face = "bold")) +
<<<<<<< HEAD

=======
=======
>>>>>>> 8c1575b513e2fe243a65541265fabce61e4fae1f
ggplot(meanFBv_by_age, aes(x=Age, y=meanFBv)) + geom_point() + 
  ggtitle("Mean Fastball Velocity by Age") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 25, face = "bold")) +
<<<<<<< HEAD
=======
>>>>>>> daff8a4a31416d90bf5ab68470a9e90381febc9b
>>>>>>> 8c1575b513e2fe243a65541265fabce61e4fae1f
  scale_x_continuous(
    breaks = seq(20, 40, 2),
    limits = c(20,40)
  ) + 
  scale_y_continuous(
<<<<<<< HEAD
    breaks = seq(85, 96, 1),
    limits = c(85, 96)
  ) + theme(plot.title = element_text(hjust = 0.5))
=======
<<<<<<< HEAD
    breaks = seq(85, 96, 1),
    limits = c(85, 96)
  ) + theme(plot.title = element_text(hjust = 0.5))
=======
>>>>>>> 8c1575b513e2fe243a65541265fabce61e4fae1f
    breaks = seq(88, 96, 0.5),
    limits = c(88, 96)
  )

<<<<<<< HEAD
=======
>>>>>>> daff8a4a31416d90bf5ab68470a9e90381febc9b
>>>>>>> 8c1575b513e2fe243a65541265fabce61e4fae1f



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
