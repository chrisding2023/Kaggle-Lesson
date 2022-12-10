# statistics
# Author:Shuo Zhang

# Question 1 The  [01] Temp.txt dataset records the body temperatures, 
#gender, and heart rate of 130 individuals;
# you may assume that the observations are independent of one another.

setwd("~/Desktop/kaggle")
Temp=read.csv('Temp.txt', header = T, stringsAsFactors = F, sep=' ')
View(Temp)
dim(Temp)
str(Temp)
summary(Temp)

#1 You have heard that the average human body temperature is 98.6 degrees Fahrenheit.
# Does this data support this claim?
#I am told that the true the average human body temperature is 
#98.6 degrees Fahrenheit;however, we do not believe this is the case. 
#We want to test the notion that the true body Temperature should be 
# centered at a value lower than 98.6 degrees. The average body Temperature 
#of our dataset is 98.25 degrees. Is this significantly different for
#us to change our belief? To decide, I use a One-Sample T-Test:

t.test(Temp$Body.Temp, mu = 98.6, alternative = "two.sided")
mean(Temp$Body.Temp)

#The p-value for this test is extremely small (<.0005); thus, we reject the null
#hypothesis that the average human body temperature is 98.6 degrees F in favor of
#the alternative that the average temperature is different than 98.6 degrees F at the
#95% confidence level. 

#We are 95% confident that the true average human body
#temperature is between 98.122 degrees F and 98.377 degrees F.

#2 Is there a significant difference in body temperature between males and females?
#To decide, I conduct ant two sample t-test or ANOVA test:

t.test(Temp$Body.Temp[Temp$Gender=='Female'],
         Temp$Body.Temp[Temp$Gender=='Male'],
         alternative = "two.sided")

summary(aov(Body.Temp ~ Gender, data = Temp))

TukeyHSD(aov(Body.Temp ~ Gender, data = Temp))


#The p-value for this test is about 0.024, which is less than our cutoff value
#of 0.05; thus, we reject the null hypothesis that the average human body
#temperature is the same for males and females in favor of the alternative that
#they differ in some manner at the 95% confidence level. We are 95% confident
#that the true difference is between 0.039 and 0.540 degrees F, with Females
#having a higher average body temperature.
t.test(Temp$Heart.Rate[Temp$Gender=='Female'],
       Temp$Heart.Rate[Temp$Gender=='Male'],
       alternative = "two.sided")
TukeyHSD(aov(Heart.Rate~ Gender, data = Temp))

#3 I am told that the variances of male heart rate and female heart rate are different; 
#specifically,females have a lower heart rate variance. To determine, I conduct
#an F-test:

var.test(Temp$Heart.Rate[Temp$Gender=='Female'],
         Temp$Heart.Rate[Temp$Gender=='Male'],
         alternative = "two.sided")
# The p-value for this test is 0.011, which is smaller than 0.05.
#Thus, the data support the alternative hypothesis that the variances of male 
#heart rate and female heart rate are different.

var.test(Temp$Heart.Rate[Temp$Gender=='Female'],
         Temp$Heart.Rate[Temp$Gender=='Male'],
         alternative = "less")
#Under the null hypothesis, males and females have the same heart rate variance;
#we test whether females have a statistically significantly lower heart rate
#variance than males. We find that the p-value is extremely high (0.9945). Thus,
#we retain our null hypothesis. We are 95% confident that the true ratio is
#between  1.16086 and 3.12029.
var.test(Temp$Body.Temp[Temp$Gender=='Female'],
       Temp$Body.Temp[Temp$Gender=='Male'],
       alternative = "greater")


#Question 2
# Load the  HairEyeColordataset located in the  datasetslibrary; 
# this is a three dimensional dataset that records the hair color, 
# eye color, and gender of 592 different statistics students.
dim(HairEyeColor)
str(HairEyeColor)
mosaicplot(HairEyeColor,shade=TRUE)
#blue means there are more observations in that cell than would be expected 
#under the null model (independence). 
#Red means there are fewer observations than would have been expected.

#Reduce your dataset to focus on just females.
# Conduct a hypothesis test to see if hair and eye color are independent 
# of one another for this reduced dataset.
new=HairEyeColor[, , 'Female']
mosaicplot(new,shade=TRUE)

chisq.test(new)

# The p-value for this test is < 2.2e-16, which is smaller than 0.05.
# I conclude in favor of the alternative hypothesis that 95% confident hair and eye color are 
# not independent of one another for this reduced dataset.

