# neural network
# author: shuo zhang

setwd("~/Desktop/kaggle")

abalone = read.csv("[14] Abalone.csv")
str(abalone)
summary(abalone)
View(abalone)

#1 Normalize your data so all values fall between 0 and 1.

# Normalization rescales the values from to a range of [0,1]. 
# This might useful in some cases where all parameters need to have the same positive scale, 
# but outliers from data set are lost.

# What's the difference between Normalization and Standardization? 
# http://stats.stackexchange.com/questions/10289/whats-the-difference-between-normalization-and-standardization

normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

abalone = as.data.frame(lapply(abalone, normalize))
summary(abalone)

#2 Split the data into a training and test set with an 20% - 80% split, respectively.

set.seed(0)
train = sample(1:nrow(abalone), nrow(abalone)/5)
abalone.train = abalone[train, ]
abalone.test = abalone[-train, ]

#3 Create the following formula object to streamline your model fitting procedure.
formula = as.formula(paste("Rings ~", paste(names(abalone[, -8]), collapse = " + ")))
formula
#4 Train and save as objects six different neural networks on the training data with the following specifications
# a. Basic Model: 1 hidden layer with 1 hidden node.
# b. Moderate Model: 1 hidden layer with 25 hidden nodes.
# c. Complex Model: 1 hidden layer with 50 hidden nodes.
# d. Multilayer Model #1: 3 hidden layers with 10 hidden nodes each.
# e. Multilayer Model #2: 3 hidden layers with 10, 5, and 2 hidden nodes,
# respectively.
# f. Multilayer Model #3: 3 hidden layers with 5 hidden nodes each.

#5 For each of the models in part 4, do the following:
# a. Plot the network topology.
# b. Record the training error.
# c. Record the testing error; manually calculate the sum of squared errors using the following formula to be consistent with the  neuralnet() function’s calculation of the training error:  .5 * sum((prediction ? abalone.test$Rings)^2). 
# d. Record the number of steps involved in training the model.

#Basic Model
library(neuralnet)
# Get to know the package:
# https://journal.r-project.org/archive/2010-1/RJournal_2010-1_Guenther+Fritsch.pdf
set.seed(0)
basic = neuralnet(formula,
                  hidden = 1,
                  data = abalone.train) #stepmax
basic 
plot(basic)

basic.prediction = compute(basic, abalone.test[, -8])$net.result

basic$result.matrix
.5*sum((basic.prediction - abalone.test$Rings)^2)
#  10.39232

#Basic Model:
#-1 hidden layer.
#-1 hidden node.
#-Training error of  2.588513.
#-Testing error of  10.39232.
#-1890 steps.

#Moderate Model
set.seed(0)
moderate = neuralnet(formula,
                     hidden = 25,
                     data = abalone.train)

moderate.prediction = compute(moderate, abalone.test[, -8])$net.result

moderate$result.matrix
.5 * sum((moderate.prediction - abalone.test$Rings)^2)
#10.36919

#Moderate Model:
#-1 hidden layer.
#-25 hidden nodes.
#-Training error of  2.306952.
#-Testing error of 10.36919.
#-3686 steps.

#Complex Model
set.seed(0)
complex = neuralnet(formula,
                    hidden = 50,
                    data = abalone.train)

complex.prediction = compute(complex, abalone.test[, -8])$net.result

complex$result.matrix

.5 * sum((complex.prediction - abalone.test$Rings)^2)
#12.04542

#Complex Model:
#-1 hidden layer.
#-50 hidden nodes.
#-Training error of 1.984217.
#-Testing error of 12.04542.
#-75654 steps.

#Multilayer Model #1
set.seed(0)
multi1 = neuralnet(formula,
                   hidden = c(10, 10, 10),
                   data = abalone.train)

plot(multi1)

multi1.prediction = compute(multi1, abalone.test[, -8])$net.result

multi1$result.matrix

.5 * sum((multi1.prediction - abalone.test$Rings)^2)
#11.40576

#Multilayer Model #1:
#-3 hidden layers.
#-10, 10, and 10 hidden nodes.
#-Training error of 1.74269.
#-Testing error of 11.40576.
#- 14255 steps.

#Multilayer Model #2
set.seed(0)
multi2 = neuralnet(formula,
                   hidden = c(10, 5, 2),
                   data = abalone.train)

multi2.prediction = compute(multi2, abalone.test[, -8])$net.result

multi2$result.matrix

.5 * sum((multi2.prediction - abalone.test$Rings)^2)
#11.01431

#Multilayer Model #2:
#-3 hidden layers.
#-10, 5, and 2 hidden nodes.
#-Training error of  1.75592911.
#-Testing error of 11.01431
#-9362 steps.

#Multilayer Model #3
set.seed(0)
multi3 = neuralnet(formula,
                   hidden = c(5, 5, 5),
                   data = abalone.train)

multi3.prediction = compute(multi3, abalone.test[, -8])$net.result

multi3$result.matrix

.5 * sum((multi3.prediction - abalone.test$Rings)^2)
#9.861724

#Multilayer Model #3:
#-3 hidden layers.
#-5, 5, and 5 hidden nodes.
#-Training error of 2.089885.
#-Testing error of  9.861724.
#- 6863 steps.

###########################
#####Model Comparisons#####
###########################

#Of the Basic, Moderate, and Complex model, which would you choose and why?
#We should choose to move forward with the Moderate model because it performs
#better than the Basic and the Complex models. Additionally, the Complex model
#seems to be overfitting to the training data.

#Of the three Multilayer models, which would you choose and why?
#We should choose to move forward with Multilayer Model #3 because it performs
#better than the other models. Additionally, it has a simpler network topology
#and takes a shorter time to train.
