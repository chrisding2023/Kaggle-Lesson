###########################
#####One-Sample T-Test#####
###########################
set.seed(0)
heights = rnorm(n = 100, mean = 70, sd = 1) #Randomly generating 100 normally
                                            #distributed observations with a
                                            #mean of 70 and standard deviation
                                            #of 1.

# test whether the mean value of height is 68


t.test(heights, mu = 68, alternative = "two.sided") 

#The p-value for this test is extremely small (<.0005); thus, we reject the null hypothesis
#that the average height of individuals is 68 inches



###########################
#####Two-Sample T-Test#####
###########################
set.seed(0)
SAT.Spring = rnorm(100, 1550, 200) #Randomly generating 100 normally distributed
                                   #observations with a mean of 1550 and a
                                   #standard deviation of 200.
SAT.Fall = rnorm(80, 1500, 210) #Randomly generating 80 normally distributed
                                #observations with a mean of 1500 and a standard
                                #deviation of 210.

# test whether mean values of SAT spring and fall are the same

t.test(SAT.Spring, SAT.Fall, alternative = "two.sided") #Conducting the t-test for two
                                                        #samples, assessing the two-sided
                                                        #alternative hypothesis.

#The p-value is 0.01215, which is less than our threshold of 0.025; thus, we reject
#the null hypothesis that the average score on the spring SAT is the same as the
#fall SAT. We conclude in favor of the alternative that the average score of the
#two test administrations is different at the 95% confidence level.



################
#####F-Test#####
################

# test whether variances of SAT spring and fall are the same

var.test(SAT.Fall, SAT.Spring, alternative = "two.sided") #Conducting the F-test to
                                                          #compare two variances,
                                                          #assessing the two-sided
                                                          #alternative hypothesis.

#The p-value is 0.1161, which is not less than our threshold of 0.05; we do
#not have evidence against the null hypothesis at the 95% confidence level. Thus,
#we retain the null hypothesis that the variance of test scores for the fall and
#spring SAT administrations is the same.



#######################
#####One-Way ANOVA#####
#######################
set.seed(0)
Low.Calorie = rnorm(200, 10, 1) #Randomly generating weight loss measurements
Low.Carb = rnorm(200, 8.5, 1)   #for various diet types.
Low.Fat = rnorm(200, 8, 1)
Control = rnorm(200, 0, 1)

Weight.Loss = c(Low.Calorie, Low.Carb, Low.Fat, Control) #Combining data into
Category = c(rep("Low Calorie", 200),                    #different consolidated
             rep("Low Carb", 200),                       #vectors.
             rep("Low Fat", 200),
             rep("Control", 200))

# test whether the mean values of the weight loss of different categories of diet are the same

summary(aov(Weight.Loss ~ Category)) #Conducting the One-Way ANOVA on the weight
                                     #loss by considering each category of diet.

#The p-value for this test is extremely small (<.0005). Thus, the average weight
#loss for at least one of the diet groups differs from the average weight loss of
#the others. We reject the null hypothesis that the average weight loss is the
#same for each of the types of diet.



##################################
#####X^2 Test of Independence#####
##################################
quiz.data = matrix(c(44, 21, 12, 18), nrow = 2, ncol = 2, byrow = TRUE)
dimnames(quiz.data) = list(Attendance = c("Present", "Absent"),
                           Grade = c("Pass", "Fail"))

# test whether the attendance and grade variables are independent of one another

chisq.test(quiz.data) #Conducting the X^2 test of independence data on the quiz
                      #data.

#The p-value for this test is very small (0.02001). This is an indication that
#under our null hypothesis, it would be extremely unlikely to observe results
#like we have observed in our data. The null hypothesis states that the attendance
#and grade variables are independent of one another; however, the test provides
#evidence against this assertion. Therefore, we conclude in favor of the
#alternative hypothesis that the variables of attendance and grade are not
#independent of one another.



