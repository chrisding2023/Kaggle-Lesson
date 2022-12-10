# multiple linear regression
# author: shuo zhang

#Load the  NYC Restaurants.txt dataset into your workspace. 
#This dataset contains survey results from customers of 168 different Italian restaurants 
#in the New York City area. The data are in the form of the average of customer views 
#on various attributes (food, decor, and service) scored on a scale from 1 to 30, 
#along with the average price of dinner. 
#There is also a categorical variable for the location of the restaurant.
setwd("~/Desktop/kaggle")
restaurant=read.csv('NYC Restaurants.txt', header = T, stringsAsFactors = F, sep=' ')
head(restaurant)
dim(restaurant)
str(restaurant)
summary(restaurant)

library(dplyr)

#1 Create a scatterplot matrix of all continuous variables 
restaurant1=dplyr::select(restaurant, Food, Decor, Service )
library(corrplot)
M <- cor(restaurant1)
corrplot(M, method="circle")

unique(restaurant$Location)


#2 Fit a multiple linear regression predicting the price of a meal based on the customer 
# views and location of the restaurant.
model1= lm(Price ~ Food+Decor+Service+Location, data = restaurant)
summary(model1)
#The equation of the model can be constructed from the output:
#Predicted Price = -21.955750+ 1.538120*Food+1.910087*Decor-0.002727*Service-2.068050*LocationWest 

# intercept: when food, decor, service and locationwest is zero, the food price is -21.955750.
# food: With a 1 increase in food score and and all other variables are held constant,
#the price, on average, increases by approximately 1.538120.
# decor:With a 1 increase in decor score and and all other variables are held constant,
#the price, on average, increases by approximately 1.910087.
# service:With a 1 increase in service score and all other variables are held constant,
#the price, on average, decreases by approximately 0.002727.
# locationwest: With a choice in west and and all other variables are held constant,
#the price, on average, decreases by approximately 2.068050.

# intercept coefficient is significant (p-value smaller than 0.05).
# food coefficient is significant (p-value smaller than 0.05).
# decor coefficient is significant (p-value smaller than 0.05).
# service coefficient is not significant (p-value larger than 0.05).
# locationwest coefficient is significant (p-value smaller than 0.05).

# The overall regression is significant (p-value smaller than 0.05).

#Residual standard error: 5.738 on 163 degrees of freedom
# The residual sum of squares is about 5.738; this is an approximation of how much
# the sum of the squared vertical distances tend to deviate from the observations 
#to the regression surface.

#	Adjusted R-squared:  0.6187，61.87 % of the variability
#in price is explained by the parameter values.

#3 assumption of MLR
plot(model1)
# no violation

#4 the influence plot: outliner
library(car)
influencePlot(model1)
restaurant[30,1]
restaurant[56,1]
restaurant[117,1]
restaurant[130,1]
restaurant[168,1]
# we should be concerned about the five restaurants:
# "Nello", "Rainbow Grill" "Gennaro", "Harry Cipriani", "Veronica"

#5 VIF
vif(model1)
# Food    Decor  Service Location 
# 2.714261 1.744851 3.558735 1.064985
#VIFs are all below 5. The corresponding regression coefficients are well estimated 
# due to no multicollinearity.

#6 added-variable plot
avPlots(model1)
#food, decor have distinct patterns. they are good contributions to the model; 
# service has absent patterns and it could be dropped from the model.

#7 improvement of model: remove service variable
model3=lm(Price ~ Food+Decor+Location, data = restaurant) 

summary(model3)
#The equation of the model can be constructed from the output:
#Predicted Price =  -21.9599+ 1.5363*Food+1.9094 *Decor-2.0670*LocationWest 

# intercept: when food, decor, service and locationwest is zero, the food price is -21.9599.
# food: With a 1 increase in food score and and all other variables are held constant,
#the price, on average, increases by approximately 1.5363.
# decor:With a 1 increase in decor score and and all other variables are held constant,
#the price, on average, increases by approximately 1.9094.
# locationwest: With choose in west and and all other variables are held constant,
#the price, on average, decreases by approximately 2.0670.

# intercept coefficient is significant (p-value smaller than 0.05).
# food coefficient is significant (p-value smaller than 0.05).
# decor coefficient is significant (p-value smaller than 0.05).
# locationwest coefficient is significant (p-value smaller than 0.05).

# The overall regression is significant (p-value smaller than 0.05).

#Residual standard error: 5.72 on 164 degrees of freedom
# The residual sum of squares is about  5.72; this is an approximation of how much
# the sum of the squared vertical distances tend to deviate from the observations 
#to the regression surface.

#	Adjusted R-squared:  0.6211 

plot(model3)
# no violation

influencePlot(model3)
restaurant[30,1]
restaurant[56,1]
restaurant[117,1]
restaurant[130,1]
restaurant[168,1]
# we should be concerned about the five restaurants:
# "Nello", "Rainbow Grill" "Gennaro", "Harry Cipriani", "Veronica"

vif(model3)
# Food    Decor Location 
# 1.389515 1.346030 1.038000 
#VIFs are all below 5. The corresponding regression coefficients are well estimated 
# due to no multicollinearity.

avPlots(model3)
#food, decor have distinct patterns. they are good contributions to the model; 
# locationwest has absent patterns and it could be dropped from the model.

#8 improvement of model: remove location variable
model4=lm(Price ~ Food+Decor, data = restaurant)
summary(model4)
# the intercept, food and decor coefficients and the overall regression is significant
# (p-values less than 0.05).
#Residual standard error: 5.788 on 165 degrees of freedom
#Adjusted R-squared:  0.6121 
plot(model4)
# no violation to the assumptions

influencePlot(model4) 
# 6 outlier

vif(model4)
# vifs are below 5

avPlots(model4)
# food and decor have distinct patterns.

#9 model selection
AIC(model1, model3, model4)
# df      AIC
# model1  6 1070.711
# model3  5 1068.711
# model4  4 1071.677


BIC(model1, model3, model4)
# df      BIC
# model1  6 1089.454
# model3  5 1084.330
# model4  4 1084.173

# for AIC, choose model3 (remove service)
# for BIC, choose model4 (remove service and location)
# when the primary goal of multiple regression is prediction, I will choose model3.
# when the primary goal of multiple regression is descriptive, I will choose model4.


#We can use stepwise regression to help automate the variable selection process.
#Here we define the minimal model, the full model, and the scope of the models
#through which to search:
model.empty = lm(Price ~ 1, data = restaurant[,-1]) #The model with an intercept ONLY.
model.full = lm(Price ~. , data = restaurant[,-1]) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2) 
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)
# remove service

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(168))
backwardBIC = step(model.full, scope, direction = "backward", k = log(168))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(168))
bothBIC.full = step(model.full, scope, direction = "both", k = log(168))
#remove service and location
