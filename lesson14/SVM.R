# title: SVM
# author: shuo zhang


# The task is to predict the type of a glass on basis of its chemical analysis.
# We start by splitting the data into a train and test set:
library(e1071)
library(rpart)
data(Glass, package="mlbench")
View(Glass)
unique(Glass$Type)
dim(Glass)
str(Glass)
summary(Glass)
 ## split data into a train and test set
set.seed(0)
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/2))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

## svm
# cost: cost of constraints violation (default: 1)—it is the ‘C'
# defalut: kernel ="radial"

# radial kernel
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 0.8)
svm.pred <- predict(svm.model, testset[,-10])
 ## compute svm confusion matrix
table(pred = svm.pred, true = testset[,10])
# accuracy
(20+30+4+2+1+10)/nrow(testset)
# 0.6261682

# linear kernel
svm.model1 <- svm(Type ~ ., data = trainset, kernel='linear', cost = 100)
svm.pred1 <- predict(svm.model1, testset[,-10])
## compute svm confusion matrix
table(pred = svm.pred1, true = testset[,10])
# accuracy
(23+27+2+3+3+13)/nrow(testset)
#  0.6635514

# polynomial kernel
# 2 degree
svm.model2 <- svm(Type ~ ., data = trainset, kernel='polynomial', 
                  cost = 100,  degree=2)
svm.pred2 <- predict(svm.model2, testset[,-10])
## compute svm confusion matrix
table(pred = svm.pred2, true = testset[,10])
# accuracy
(19+19+2+3+3+13)/nrow(testset)
#0.5514019

# linear kernel is best





