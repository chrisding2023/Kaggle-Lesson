# PCA
# author: shuo zhang

# Load the  heptathlon dataset from the  HSAUR library into your workspace. 
# A heptathlon is a combined track and field event-based contest for women. 
# This dataset contains scores on each event for the 1988 olympic heptathlon
# competition held in Seoul.

library(HSAUR)
heptathlon
dim(heptathlon)
str(heptathlon)
names(heptathlon)
plot(heptathlon)

# It will help to have all event scores going in the “same direction” 
#(i.e., a higher event score implies a better performance, and a lower 
#event score implies a worse performance). To do so, transform the hurdle
# and running variables by subtracting the original scores 
#for each athlete from the maximum score of each of those variables
heptathlon.new = heptathlon
heptathlon.new$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon.new$run200m = max(heptathlon$run200m) - heptathlon$run200m
heptathlon.new$run800m = max(heptathlon$run800m) - heptathlon$run800m
# We do this because we want to make the data into same direction

heptathlon.new
summary(heptathlon.new)
plot(heptathlon.new)
round(cor(heptathlon.new[,-8]), 2)
#We still see the same relationship as we saw within the original data, except
#now higher numbers are associated with better performance for all events. The
#same underlying structure has been retained.

# Extract the appropriate number of principal components from your dataset 
# that does not include the  score  variable and save this object.
library(psych)
fa.parallel(heptathlon.new[, -8], fa = "pc", n.iter = 100) 
# show the eigenvalues for a principal components using fa = "pc" 
abline(h = 1)

# Kaiser-Harris criterion suggests retaining PCs with eigenvalues > 1; 
#   Thus, we would end up selecting 2 principal components with this method.
# Cattell Scree test visually inspects the elbow graph for diminishing return;
#   retain PCs before a drastic drop-off. Thus, we would end up selecting 2 
# Ultimately, we choose to move forward with 2 principal components.


pc_heptathlon = principal(heptathlon.new[, -8], nfactors = 2, rotate = "none")

pc_heptathlon

#The variance of each of the extracted principal components is 4.46 and 1.19;
#these are also the eigenvalues associated with the principal components.


#The first principal component accounts for approximately 64% of the variability
#in the original dataset, and the second principal component accounts for
#approximately 17% of the variability in the original dataset. Together, they
#account for approximately 81% of the variability in the original dataset.

biplot(pc_heptathlon)
# This shows that the variables longjump, highjump, hurdles, run200m, shot, and run800m  
# are heavily loaded on PC1 and javelin is heavily loaded on PC2 (this can also be seen the loadings above).


factor.plot(pc_heptathlon, labels = colnames(heptathlon.new))

#The first principal component appears to be making a distinction between all
#events in general against the javelin; all events except the javelin are
#highliy positively correlated with the first principal component, whereas the
#javelin event only has a correlation of about .16. The second principal
#component makes a distinction between the hurdles, high jump, long jump, and
#800 meter run against the shot, javelin, and 200 meter run. The former
#group of variables all have negative loadings, whereas the latter group of
#variables all have positive loadings. The second principal component appears
#to define a vector that differentiates between leg strength and arm strength,
#with the exception of the 200 meter run.

# new coordinate system of the data set after pca
plot(pc_heptathlon$scores)

# new data set after pca
pc_heptathlon$scores

# correlation between pc1 and pc2 is almost 0
cor(pc_heptathlon$scores)
