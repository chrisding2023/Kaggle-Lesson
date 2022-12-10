##########################
#####Ridge Regression#####
##########################
# question 1
# please load Hitters data from ISLR library, and remove the observations that have 
# missing value, hint use na.omit() function, and also count how many obs have been removed

# question 2
# The target is Salary, and all other variables are predictors. Please create x and y
#Need x as model matrices for glmnet() function.Model.matrix() automatically conducts
#conversions for factor variables into numeric variables.


# question 3
# use ridge to predict salary by other variables, the lambda range is defined by grid
grid = 10^seq(5, -2, length = 100)

#a. Fitting the ridge regression. Alpha = 0 for ridge regression.

#b. Visualizing the ridge regression shrinkage.

#c. Creating training and test sets. Here we decide to use a 80-20 split with
#approximately 80% of our data in the training set and 20% of our data in the
#test set.

#d. Running 10-fold cross validation to find best lambda for ridge regression.

#e. What is the test MSE associated with this best value of lambda?

#f. Refit the ridge regression on the overall dataset using the best lambda value
#from cross validation; inspect the coefficient estimates.


#g. Let's also inspect the MSE of our final ridge model on all our data.

##########################
#####Lasso Regression#####
##########################
# question 4
#a. Fitting the lasso regression. Alpha = 1 for lasso regression. using the same grid value.

#b. Visualizing the lasso regression shrinkage.

#c. Running 10-fold cross validation to find the best lambda.

#d. What is the test MSE associated with this best value of lambda?

#e. Refit the lasso regression on the overall dataset using the best lambda value
#from cross validation; inspect the coefficient estimates.


#f. Let's also inspect the MSE of our final lasso model on all our data.

# question 5
# compare ridge with lasso for the final models.