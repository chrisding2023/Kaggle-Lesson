# ridge/lasso regression
# author: shuo zhang

# Read in the Prostate.txt dataset into your workspace. 
#This dataset comes from a study by Stamey et a. (1989) of prostate cancer, 
#measuring the correlation between the level of a prostate-specific antigen 
#and some covariates. The included variables are the log-cancer volume, 
#log-prostate weight, age of patient, log-amount of benign hyperplasia, 
#seminal vesicle invasion, log-capsular penetration, Gleason score, 
#and percent of Gleason scores 4 or 5; the response variable is the log-psa.

setwd("~/Desktop/kaggle")
Prostate=read.csv('Prostate.txt', header = T, stringsAsFactors = F, sep='')
View(Prostate)
dim(Prostate)
str(Prostate)
summary(Prostate)

#1 Creating the data matrices for the glmnet() function.
library(ISLR)
library(glmnet)
x=model.matrix(lpsa~., Prostate)[, -1]
y=Prostate$lpsa

# separate to training/test by ratio 4:1
set.seed(0)
train = sample(1:nrow(x), 8*nrow(x)/10)
y.test = y[-train]
y.train=y[train]
length(train)/nrow(x) #0.8
length(y.test)/nrow(x) #0.2

#2 Fit a slew of ridge regression models  on your training data by checking 
# across a wide range of lambda values
grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")
#The coefficients all seem to shrink towards 0 as lambda gets quite large. Most
#coefficients are very close to 0 once the log lambda value gets to about 5;
#however, in ridge regression coefficients are never exactly 0.

#3 Perform 10-fold cross-validation on your training data 
# and save the output as an object
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)

plot(cv.ridge.out, main = "Ridge Regression\n")

#The error seems to be reduced with a log lambda value of around -1.84, This is the value of lambda
#we should move forward with in our future analyses.
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
# best lambda: 0.1592283
log(bestlambda.ridge)
# -1.837416

ridge.out = glmnet(x[train,], y[train], alpha = 0, lambda = bestlambda.ridge)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)

#3 test MSE
ridge.bestlambdatrain = predict(ridge.out, s = bestlambda.ridge, newx = x[-train, ])
mean((ridge.bestlambdatrain - y.test)^2)
# test MSE:0.598976
ridge.bestlambdatrain = predict(ridge.out, s = bestlambda.ridge, newx = x[train, ])
mean((ridge.bestlambdatrain - y.train)^2)
# train MSE: 0.4506629

#4 lasso regression
lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")
#The coefficients all seem to shrink towards 0 as lambda gets quite large. All
#coefficients seem to go to 0 once the log lambda value gets to about 0. We
#note that in the lasso regression scenario, coefficients are necessarily set
#to exactly 0.
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
# 0.05994843
log(bestlambda.lasso)
# -2.814271
#The error seems to be reduced with a log lambda value of around -2.81, This is the value of lambda
#we should move forward with in our future analyses.

lasso.out = glmnet(x[train,], y[train], alpha = 1, lambda = bestlambda.lasso)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)
# lcp,age are ignored

lasso.bestlambdatrain = predict(lasso.out, s = bestlambda.lasso, newx = x[-train, ])
mean((lasso.bestlambdatrain - y.test)^2)
# MSE test:0.6367398
lasso.bestlambdatrain = predict(lasso.out, s = bestlambda.lasso, newx = x[train, ])
mean((lasso.bestlambdatrain - y.train)^2)
# MSE train:0.4600595

# Ultimately, the ridge regression MSE was slightly lower than that of the lasso. 
#Although this might be due to random variability among our dataset, 
# if we are strictly talking about predictive power, we might choose to 
# move forth with the ridge regression in this scenario.
#On the other hand, the final lasso regression model "kicked out" the lcp and age
#variables altogether (setting its coefficient value to 0). If we are particularly
#interested in dimension reduction and variable selection, we might choose to
#move forth with the lasso regression.