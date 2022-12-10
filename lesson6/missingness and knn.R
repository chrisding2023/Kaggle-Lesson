# Missingness, Imputation, & KNN
# author: shuo zhang

#Load the  titanic3 dataset from the  PASWRlibrary; this dataset describes 
# the survival status of individual passengers from the Titanic voyage. 
#There are 14 different variables, some of which have quite a bit of missingness.

library(PASWR)
data("titanic3")
dim(titanic3)
str(titanic3)
unique(titanic3$sibsp)
titanic3$sibsp=as.factor(titanic3$sibsp)
unique(titanic3$parch)
titanic3$parch=as.factor(titanic3$parch)
unique(titanic3$body)
titanic3$body=as.factor(titanic3$body)
str(titanic3)
summary(titanic3)
View(titanic3)
titanic3[titanic3=='']=NA
summary(titanic3)
library(mice)
md.pattern(titanic3)
# 7 variables contain at least one missing value 
# and they are fare, embarked, age, home.dest, boat, cabin, body 
colSums(is.na(titanic3)) * 100 / nrow(titanic3)

colMeans(is.na(titanic3)) * 100

sum(!complete.cases(titanic3))
sum(!complete.cases(titanic3))/ nrow(titanic3)

sum(is.na(titanic3))
mean(is.na(titanic3)) * 100

library(VIM)
aggr(titanic3)

table(titanic3$survived, is.na(titanic3$body))
hist(titanic3$age)
#-MCAR: We only have one missing fare observation. It could be the case that this
#       one value is missing because of human error or a simple mistake in data
#       entry.
#-MAR: There is a lot of missingness in the body identification number variable.
#      We notice that there is a relationship between the survived variable and
#      the body variable. It appears as though observations only received a body
#      id if the individual did not survive.
#-MNAR: (Supposition) Elder voyagers might have perished more easily because of
#       their lower mobility. As a result, their ages were not captured as
#       easily by the researchers. The missing values of age depend on the value
#       of age itself.
plot(density(titanic3$age, na.rm = T), col='blue', main = "Age Distribution")
#delete method
df=titanic3[complete.cases(titanic3),]
#Impute using mean value imputation for the age variable.
library(Hmisc)
imputed.age = impute(titanic3$age, what=c("mean"))

plot(density(imputed.age), col='blue', main = "Age Distribution")
lines(density(titanic3$age, na.rm = T), col='red')
legend("topright", c("before imputation", "after imputation"), lwd = 1,
       lty = 1, col = c("red", "blue"))
#Mean Value Imputation: The mean value inputation tends to skew the distribution
#heavily towards the average value of the original dataset. This makes a local
#mode right at the mean, and can make our data appear numerically more centralized
#than it actually is in reality.
# medium substitution
df=titanic3
df$age[is.na(df$age)] <- median(df$age, na.rm = TRUE)
plot(density(df$age), col='blue', main = "Age Distribution")
lines(density(titanic3$age, na.rm = T), col='red')
legend("topright", c("before imputation", "after imputation"), lwd = 1,
       lty = 1, col = c("red", "blue"))
# calculate mean and median of age before and after imputation
mean(titanic3$age, na.rm = TRUE)
mean(imputed.age)
median(titanic3$age, na.rm = TRUE)
median(df$age)
#Impute using simple random imputation for the age variable.
set.seed(0)
imputed.age2 = impute(titanic3$age, "random") 
plot(density(imputed.age2), col='blue', main = "Age Distribution")
lines(density(titanic3$age, na.rm = T), col='red')
legend("topright", c("before imputation", "after imputation"), lwd = 1,
       lty = 1, col = c("red", "blue"))
#Simple Random Imputation: The resulting distribution is quite similar to the
#original. While structure is retained, if data are MNAR than the resulting imputed
#data will be biased towards values that actually do exist within our dataset.

#create a new data frame that includes:
#a. The pclass, survived, sex, age, sibsp, and parch variables from the original titanic3dataset.
#b. The simple random imputation of the fare variable you created above.
set.seed(0)
imputed.fare = impute(titanic3$fare, "random") 
new.titanic3=as.data.frame(titanic3$pclass)
colnames(new.titanic3)=c('pclass')
new.titanic3$survived=titanic3$survived
new.titanic3$sex=titanic3$sex
new.titanic3$age=titanic3$age

new.titanic3$sibsp=titanic3$sibsp
new.titanic3$parch=titanic3$parch

new.titanic3$fare=imputed.fare
str(new.titanic3)
summary(new.titanic3)


# Separate this new data frame into two separate data frames as follows
#  (note that there should be no observations that appear in both data frames):
# a. For observations that are totally complete: all variables.
# b. For observations that are missing a value for age: all variables except age.
new.titanic3.com=new.titanic3[complete.cases(new.titanic3),]
dim(new.titanic3.com)
new.titanic3.incom=new.titanic3[!complete.cases(new.titanic3),-4]
dim(new.titanic3.incom)
# Use 1 Nearest Neighbor to impute using:
#a. Manhattan distance.
#b. Euclidean distance.
#c. Minkowski distance with p = 10 .
library(kknn)
new.titanic3.manhattan = kknn(age ~ ., new.titanic3.com, 
                              new.titanic3.incom, k = 1, distance = 1)
fit1=fitted(new.titanic3.manhattan)

new.titanic3.euclidean = kknn(age ~ ., new.titanic3.com, 
                              new.titanic3.incom, k = 1, distance = 2)
fit2=fitted(new.titanic3.euclidean)


new.titanic3.minkowski=kknn(age ~ ., new.titanic3.com, 
                            new.titanic3.incom, k = 1, distance = 10)
fit3=fitted(new.titanic3.minkowski)


plot(density(new.titanic3.com$age), col='blue', main='Age distribution', ylim=c(0,0.07))
lines(density(fit1), col='red')
lines(density(fit2), col='yellow')
lines(density(fit3), col='green')
legend("topleft", c("com", "manhattan", "euclidean", "minkowski"),lwd = 1,
       lty = 1, col = c("blue", "red", "yellow", "green"), cex = .75)

#The different distance measures are each generally matching the age distribution,
#but each are pulling out different local modes because of calculation differences.

# Use the √n Nearest Neighbor rule to impute using:
#a. Manhattan distance.
#b. Euclidean distance.
#c. Minkowski distance with p = 10 .
new.titanic3.manhattan1 = kknn(age ~ ., new.titanic3.com, 
                              new.titanic3.incom, k = 32, distance = 1)
fit4=fitted(new.titanic3.manhattan1)

new.titanic3.euclidean1 = kknn(age ~ ., new.titanic3.com, 
                              new.titanic3.incom, k = 32, distance = 2)
fit5=fitted(new.titanic3.euclidean1)

new.titanic3.minkowski1=kknn(age ~ ., new.titanic3.com, 
                            new.titanic3.incom, k = 32, distance = 10)
fit6=fitted(new.titanic3.minkowski1)

plot(density(new.titanic3.com$age), col='blue', main='Age distribution', ylim=c(0,0.1))
lines(density(fit4), col='red')
lines(density(fit5), col='yellow')
lines(density(fit6), col='green')
legend("topleft", c("com", "manhattan", "euclidean", "minkowski"),lwd = 1,
       lty = 1, col = c("blue", "red", "yellow", "green"), cex = .75)

#As we increase the number of considered neighbors, we see that we tend to
#smooth over local modes and highlight the global aspects of the distribution. As
#a result, the various distance measures tend to appear much similar than they
#did when we only used one neighbor.