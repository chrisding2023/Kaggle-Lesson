# logistic regression
# author: shuo zhang

# Load the  Sleuth2 library and extract the  case2002 dataset.
# This dataset reports results of a survey conducted from 1972 to 1981 in 
#the Netherlands aiming to see if birdkeeping is a risk factor for lung cancer.
# Variables include whether or not an individual had lung cancer, 
#whether or not they were birdkeeping, their gender, socioeconomic status, age,
# years of smoking, and average rate of smoking.
library(Sleuth2)
str(case2002)
dim(case2002)
summary(case2002)
View(case2002)

# correlation between the numeric predictors: no multicolinearity
library(dplyr)
df=dplyr::select(case2002, AG, YR, CD)
library(corrplot)
M <- cor(df)
corrplot(M, method="circle")

# split the data to training/test
set.seed(0)
train_row = sample(1:nrow(case2002), 8*nrow(case2002)/10)
length(train_row)
train = case2002[train_row,]
test = case2002[-train_row,]
y_test=test[,1]

#Fit a logistic regression
logit.overall = glm(LC ~., family = "binomial", data = train)
summary(logit.overall)
# the coeffcients of BKBird and YR are significant.
# Null deviance:  148.94
#Residual deviance:118.49
# McFadden’s Pseudo Rl2:
(148.94-118.49)/148.94
# 0.2044447

library(car)
plot(logit.overall)
# break some assumptions: constant variance and normality
#The average of the deviance residuals seems to be centered about 0. There might
#be one observation to be concerned about (28) because its leverage and residual
#values are somewhat large.
vif(logit.overall)
##VIFs are all below 5, no multicollinearity.
influencePlot(logit.overall)
#outliners: observation 21, 28, 117, and 32

# Interpret the coefficient of gender on the log odds scale.

logit.overall$coefficients

#The log odds of having lung cancer are increased by about 0.484 if you are a female
#as compared to a male, holding all other variables constant.

# Interpret the coefficient of socioeconomic status on the odds scale

exp(logit.overall$coefficients)

#The odds of having lung cancer are multiplied by about 1.60 for someone who has
#a high socioeconomic status as compared to someone who has a low socioeconomic
#status, holding all other variables constant.

# Interpret the 95% confidence interval based on standard errors for the
#  birdkeeping indicator on the log odds scale.

confint.default(logit.overall)

#We are 95% confident that the log odds of having lung cancer are increased
#between about 0.43 and 2.32 if someone is a birdkeeper rather than not, holding
#all other variables constant.


# Interpret the 95% confidence interval based on standard errors for the years of
#  smoking variable on the odds scale.

exp(confint.default(logit.overall))

#We are 95% confident that the odds of having lung cancer are multiplied between
#about 1.02 and 1.16 for each additional year an individual was smoking prior to
#diagnosis and examination, holding all other variables constant.

# make a prediction on test and evaluate the model
y_test <- ifelse(test$LC=='LungCancer', 1, 0)
y_test 
fitted.results <- predict(logit.overall,newdata=test[,-1],type='response')
fitted.results
fitted.results <- ifelse(fitted.results > 0.5 | fitted.results==0.5,1,0)
fitted.results
table(truth = y_test, prediction =fitted.results)
#accuracy:
(16+3)/(16+4+7+3)
#63.33%

# auc
library(ROCR)
p <- predict(logit.overall,newdata=test[,-1],type='response')
p
pr <- prediction(p, y_test)
pr
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#0.695

# if we remove unsignificant predictors, only leave BK, YR  in the model
logit.overall = glm(LC ~ BK+YR, family = "binomial", data = train)
summary(logit.overall)
# Null deviance:  148.94
#Residual deviance:122.64 , higher than before,wose
# McFadden’s Pseudo R2:
(148.94-122.64)/122.64 
# 0.2144488, higher than before, better
# make a prediction on test and evaluate the model
y_test <- ifelse(test$LC=='LungCancer', 1, 0)
fitted.results <- predict(logit.overall,newdata=test[,-1],type='response')
fitted.results <- ifelse(fitted.results > 0.5 | fitted.results==0.5,1,0)
table(truth = y_test, prediction =fitted.results)
#accuracy:
(17+4)/(17+3+6+4)
# 70%, higher than before
# auc
p <- predict(logit.overall,newdata=test[,-1],type='response')
pr <- prediction(p, y_test)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# 0.66, lower than before
# based on accuracy, we should pick model 2
# based on auc, we should pick model1