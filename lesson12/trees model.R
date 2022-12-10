# class: trees model
# author: shuo zhang group628llc

#Load the  OJdataset from the  ISLRlibrary into your workspace. 
#The data contains 1,070 purchases where the customer either purchased 
#Citrus Hill or Minute Maid orange juice. A number of characteristics of 
# the customer and product are recorded.
library(ISLR)
help(OJ)
dim(OJ)
str(OJ)
# change some factor variable to numeric variable
unique(OJ$StoreID)
OJ$StoreID=as.factor(OJ$StoreID)
unique(OJ$STORE)
OJ$STORE=as.factor(OJ$STORE)
str(OJ)
summary(OJ)

library(corrplot)
# drop factor variables
M <- cor(OJ[-c(1,3,14,18)])
corrplot(M, method="circle")
# multicolinearity exits in the data set
# tree model would work
library(tree) 
library(MASS)
library(randomForest)
library(gbm)

# split data set to training/test set
set.seed(0)
train=sample(1:nrow(OJ), 8*nrow(OJ)/10)
trainOJ=OJ[train,]
testOJ=OJ[-train,]
Purchase.train=OJ[train,]$Purchase
Purchase.test=OJ[-train,]$Purchase
nrow(trainOJ)/nrow(OJ) #0.8
nrow(testOJ)/nrow(OJ) #0.2

# classfication tree: based on Gini coefficient, no choice of entropy
tree.OJ = tree(Purchase ~ ., split = "gini", data = trainOJ)
summary(tree.OJ)
# Number of terminal nodes:  88
# Misclassification error rate: 0.1367
# accuracy of the training set: 1-13.67%=86.33%

# structure of the tree
par(mfrow = c(1, 1))
plot(tree.OJ)
text(tree.OJ, pretty = 0)

# make a prediction on test set
tree.pred1 = predict(tree.OJ, testOJ, type = "class")
table(tree.pred1, Purchase.test)
# accuracy of the test set: (110+52)/214=75.7%

# implement cross-validation and, thus, cost-complexity pruning 
# to determine how far back to prune your tree
set.seed(0)  
cv.OJ = cv.tree(tree.OJ, FUN = prune.misclass)
names(cv.OJ)
cv.OJ$size # number of Terminal Nodes
cv.OJ$dev # Misclassified Observations
cv.OJ$k # alpha
#Visualize your results 
par(mfrow = c(1, 2))
plot(cv.OJ$size, cv.OJ$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "Misclassified Observations")
plot(cv.OJ$k, cv.OJ$dev, type  = "b",
     xlab = "Alpha", ylab = "Misclassified Observations")
#Pruning the overall tree to have 19 terminal nodes; choose the best tree with
#11 terminal nodes based on cost complexity pruning.

prune.OJ = prune.misclass(tree.OJ, best =19)
summary(prune.OJ)
# Number of terminal nodes:  19 
# Misclassification error rate: 0.1636 
# accuracy of the training set:  1-16.36%=83.64%

# structure of the tree
par(mfrow = c(1, 1))
plot(prune.OJ)
text(prune.OJ, pretty = 0)

#make a prediciton
prune.pred1=predict(prune.OJ, testOJ, type='class')
table(prune.pred1, Purchase.test)
# accuracy of the test set: (111+53)/214=76.63%

#The initial tree is likely overfitting to our data, even though it stops splitting
#before placing each observation into its own terminal node. The cost-complexity
#pruning process provides a balance that will ultimately produce a model that
#penalizes complexity to a certain degree in order to stray from the potential
#of overfitting.

# random forest with default: 500 subtrees
# mtry: number of variables considered for each split trees, default: sqrt(p)
set.seed(0)
rf.OJ = randomForest(Purchase ~ ., data = trainOJ, ntree=500,
                     importance = TRUE)

rf.OJ
# No. of variables tried at each split: 4
# OOB estimate of error rate: 19.74%
# accuray of the training set: 1-0.1974=80.26%

# make a prediction
rf.pred1=predict(rf.OJ, testOJ, type='class')
table(rf.pred1, Purchase.test)
# test accuracy: (115+54)/214=78.97%

#feature importance
importance(rf.OJ)
varImpPlot(rf.OJ)
#The loyalty measure to the Citrus Hill brand is driving the largest mean decrease
#in the Gini index, meaning that it is the most important predictor in the current
#random forest.

#tuning parameter mtry: number of variables considered for each split trees
set.seed(0)
oob.err = numeric(17)
for (mtry in 1:17) {
  fit = randomForest(Purchase ~ ., data = trainOJ, mtry = mtry)
  oob.err[mtry] = fit$err.rate[500,1]
  cat("We're performing iteration", mtry, "\n")
}

plot(1:17, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")
min(oob.err)
1-min(oob.err)
# the max training accuracy among random forests: 80.49%
# the best mtry is 6

# bagging model accuracy
1 - oob.err[17]
# the training accuracy of the bagged model is 79.44%

# random forest with mtry= 6 make prediciton
set.seed(0)
rf.2var = randomForest(Purchase ~ ., data =trainOJ,ntree=500, mtry = 6)
rf.pred3=predict(rf.2var, testOJ, type='class')
table(rf.pred3, Purchase.test)
# the accuracy of the best random forest model on test is (116+55)/214=79.91%

## bagged model make prediciton, mtry=p
set.seed(0)
rf.bagged = randomForest(Purchase ~ ., data = trainOJ,ntree=500, mtry = 17)
bg.rf.pred1=predict(rf.bagged, testOJ, type='class')
table(bg.rf.pred1, Purchase.test)
# the accuracy of the bagged model on test is (114+57)/214=79.91%

# gradient boosting model
# In order to boost with classification trees, we need to transform the response variable.
OJ.train.indicator = trainOJ 
OJ.test.indicator = testOJ
OJ.train.indicator$Purchase = as.vector(trainOJ$Purchase, mode = "numeric") - 1
OJ.test.indicator$Purchase = as.vector(testOJ$Purchase, mode = "numeric") - 1

#Construct an initial boosted model on the training set
set.seed(0)
boost.OJ = gbm(Purchase ~ ., data = OJ.train.indicator,
               distribution = "bernoulli",n.trees = 10000,
               interaction.depth = 4, shrinkage = 0.001)
# tuning parameter n.trees: number of trees
n.trees = seq(from = 100, to = 10000, by = 100)
boost.predictions = round(predict(boost.OJ, newdata = OJ.test.indicator, n.trees = n.trees,
                  type = "response"))
# pick the optimal parameter
accuracy.boost = numeric(100)
for (i in 1:100) {
  accuracy.boost[i] = sum(diag(table(OJ.test.indicator$Purchase, boost.predictions[, i]))) / 214  
}
min(which(accuracy.boost == max(accuracy.boost)) * 100)
#the minimum number of trees required to reach the maximum accuracy: 1400

gbmpred=predict(boost.OJ, newdata = OJ.test.indicator,n.trees = 1400,type = "response")
pred.test=ifelse (gbmpred >= 0.5,1,0)
table(pred.test,OJ.test.indicator$Purchase)
# testing accuracy: (118+61)/214＝83.64%

# xgboost: must use data matrix
# clean data: delete factor varaible and change target variable to numeric
# or you can change factor variable to dummy variable 
library(data.table)
library(xgboost)
OJ$StoreID=as.numeric(OJ$StoreID)
OJ$STORE=as.numeric(OJ$STORE)
OJ$Store7=as.numeric(OJ$Store7)
str(OJ)
set.seed(0)
train=sample(1:nrow(OJ), 8*nrow(OJ)/10)
trainOJ=OJ[train,]
testOJ=OJ[-train,]
Purchase.train=OJ[train,]$Purchase
Purchase.test=OJ[-train,]$Purchase
OJ.train.indicator = trainOJ
OJ.test.indicator = testOJ
OJ.train.indicator$Purchase = as.vector(trainOJ$Purchase, mode = "numeric") - 1
OJ.test.indicator$Purchase = as.vector(testOJ$Purchase, mode = "numeric") - 1
train=OJ.train.indicator[, -c(1)]
test=OJ.test.indicator[, -c(1)]
setDT(train) 
setDT(test)
new_tr <- model.matrix(~.+0,data = train)
new_ts <- model.matrix(~.+0,data = test)
labels = as.vector(trainOJ$Purchase, mode = "numeric") - 1
ts_label = as.vector(testOJ$Purchase, mode = "numeric") - 1
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
# build the model
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.001, 
               max_depth=4,  n_estimators=1400)

#model training 
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 1400, 
                     watchlist = list(val=dtest,train=dtrain), 
                     print_every_n = 100, early_stop_round = 100, 
                     maximize = T , eval_metric = "error")
# when nround=400, testing error is smallest
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.001, 
               max_depth=4,  n_estimators=400)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 400, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 100, early_stop_round = 100, 
                   maximize = T , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred >= 0.5,1,0)
table(ts_label,xgbpred)
#testing accuracy:(117+59)/214=82.24%

# importance
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 


