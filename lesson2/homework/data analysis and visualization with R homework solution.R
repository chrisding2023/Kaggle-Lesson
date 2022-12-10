# Question 1
setwd("~/Desktop/kaggle")
CL = read.csv("Champions.csv")
colnames(CL)

library(dplyr)
# 1.1
df1 <- filter(CL, HomeGoal > AwayGoal)
head(df1, 5)
# 1.2
df2 <- filter(CL, HomeTeam %in% c("Barcelona", "Real Madrid"))
head(df2, 5)
#1.3
df3 = dplyr::select(CL, contains("Team"), contains("Goal"), contains("Corner"))
head(df3, 5)
# 1.4
arrange(CL, desc(HomeGoal)) 
# 1.5
CL %>% group_by(HomeTeam) %>%
  summarise(average_HomeGoal=mean(HomeGoal), possession_rate=mean(HomePossession), 
            number_of_yellow_cards=mean(HomeYellow))


# Qustion 2
library(ggplot2)
data(cars)
ggplot(data = cars , aes(x = speed, y=dist))+
  geom_point(col="red",pch = 17)+
  xlab("Speed (mpg)")+
  ylab("Stopping Distance (ft)")+
  ggtitle("Relationship between Speed and Stopping Distance")
