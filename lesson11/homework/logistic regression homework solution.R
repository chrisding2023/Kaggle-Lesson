setwd("~/Desktop/kaggle")
GradSchools = read.table("[06] Graduate Schools.txt")
str(GradSchools)
GradSchools$rank = as.factor(GradSchools$rank)
#Fitting the logistic regression with all variables; the family parameter
#specifies the error distribution and link function to be used. For logistic
#regression, this is binomial.
logit.overall = glm(admit ~ gre + gpa + rank,
                    family = "binomial",
                    data = GradSchools)

#Converting the fitted probabilities to binary:
admitted.predicted = round(logit.overall$fitted.values)

#Comparing the true values to the predicted values:
table(truth = GradSchools$admit, prediction = admitted.predicted)

# accuracy: 
#(254+30)/400=71%

summary(logit.overall) #Investigating the overall fit of the model.

#Coefficient interpretations on the log odds scale:
#-Intercept: The log odds of a student getting admitted to a graduate school
#            when they attended a top tier undergraduate school and received a
#            0 on the GRE and a 0 as their GPA is approximately -3.990.
#-GRE: For every additional point a student scores on the GRE, their log odds
#      of being admitted to graduate school increase by approximately 0.002,
#      holding all other variables constant.
#-GPA: For every additional point a student raises their GPA, their log odds of
#      being admitted to graduate school increase by approximately 0.804, holding
#      all other variables constant.
#-Rank: The change in log odds associated with attending an undergraduate school
#       with prestige of rank 2, 3, and 4, as compared to a school with prestige
#       rank 1, is approximately -0.675, -1.340, and -1.552, respectively, holding
#       all other variables constant.

exp(logit.overall$coefficients)

#Coefficient interpretations on the odds scale:
#-Intercept: The odds of a student getting admitted to a graduate school
#            when they attended a top tier undergraduate school and received a
#            0 on the GRE and a 0 as their GPA is approximately 0.019.
#-GRE: For every additional point a student scores on the GRE, their odds
#      of being admitted to graduate school multiply by approximately 1.002,
#      holding all other variables constant.
#-GPA: For every additional point a student raises their GPA, their odds of
#      being admitted to graduate school multiply by approximately 2.235, holding
#      all other variables constant.
#-Rank: The multiplicative change in odds associated with attending an undergraduate school
#       with prestige of rank 2, 3, and 4, as compared to a school with prestige
#       rank 1, is approximately 0.509, 0.262, and 0.212, respectively, holding
#       all other variables constant.


confint(logit.overall) #For logistic regression objects, the confint() function
#defaults to using the log likelihood to generate confidence
#intervals; this is similar to inverting the likelihood
#ratio test.

confint.default(logit.overall) #To generate confidence intervals for logistic
#regression models based on the standard errors
#as we are accustomed to, we can use the
#confint.default() function.

#Generating confidence intervals for the coefficients on the odds scale.
exp(confint(logit.overall))
exp(confint.default(logit.overall))



