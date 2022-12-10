# k-means clustering
# author: shuozhang


setwd("~/Desktop/kaggle")
protein = read.table("[08] Protein.txt", sep = "\t", header = TRUE)
View(protein)
dim(protein)
str(protein)
summary(protein)
#2a
wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
  print(wss)
  
}

wssplot(protein[,-1])
wss=c(5243.4136, 2476.7487, 1707.0498, 1269.0498, 1034.6793, 866.9146,  734.9707,  636.0665,  541.2653,
      464.1187,  399.4470,  346.4050, 304.0350,  262.5167,223.8967)
wss1=wss[1:14]
wss2=wss[2:15]
(wss1-wss2)/wss1

#There doesn't appear to be a clear drop-off or elbow within the graph. 
# Let's choose K = 3 as the percentage drop-off at this point appears to slow 
# (there seems to be a reduction on the "return on investment" of creating 
# additional clusters at this point).

#Running the K-means procedure 5 different times, but with only one convergence of the
# algorithm each time.
set.seed(0)
km.protein1 = kmeans(protein[,-1], centers = 3) 
km.protein2 = kmeans(protein[,-1], centers = 3) 
km.protein3 = kmeans(protein[,-1], centers = 3) 
km.protein4 = kmeans(protein[,-1], centers = 3) 
km.protein5 = kmeans(protein[,-1], centers = 3)

#Running the K-means procedure 100 times
set.seed(0)
km.proteinsim = kmeans(protein[,-1], centers = 3, nstart = 100)
km.proteinsim
c=km.proteinsim$cluster
table(c)
par(mfrow = c(2, 3))
plot(protein$Cereals, protein$RedMeat, col = km.protein1$cluster,
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Single K-Means Attempt #1\n WCV: ",
                  round(km.protein1$tot.withinss, 4)))
plot(protein$Cereals, protein$RedMeat, col = km.protein2$cluster,
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Single K-Means Attempt #2\n WCV: ",
                  round(km.protein2$tot.withinss, 4)))
plot(protein$Cereals, protein$RedMeat, col = km.protein3$cluster,
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Single K-Means Attempt #3\n WCV: ",
                  round(km.protein3$tot.withinss, 4)))
plot(protein$Cereals, protein$RedMeat, col = km.protein4$cluster,
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Single K-Means Attempt #4\n WCV: ",
                  round(km.protein4$tot.withinss, 4)))
plot(protein$Cereals, protein$RedMeat, col = km.protein5$cluster,
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Single K-Means Attempt #5\n WCV: ",
                  round(km.protein5$tot.withinss, 4)))
plot(protein$Cereals, protein$RedMeat, col = km.proteinsim$cluster,
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Best K-Means Attempt out of 100\n WCV: ",
                  round(km.proteinsim$tot.withinss, 4)))
par(mfrow = c(1, 1))
plot(protein$Cereals, protein$RedMeat, type = "n",
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Best K-Means Attempt out of 100\n WCV: ",
                  round(km.proteinsim$tot.withinss, 4)))
points(km.proteinsim$centers[, 6], km.proteinsim$centers[, 1], pch = 16, col = "blue")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
text(protein$Cereals, protein$RedMeat,
     labels = protein[,1],
     col = km.proteinsim$cluster)

#The best clustering solution manafests in a clear way when inspecting the cereal
#and red meat consumption cross-section. Generally, there appears to be a group
#that consumes cereal at an above average rate, while red meat at a below average
#rate (Albania, USSR, Hungary, Romania, Bulgaria, and
#Yugoslavia, etc). On the flip side, the largest cluster appears to have an above
#average red meat consumption, with a below average cereal consumption (consisting
#of countries like France, UK, Ireland, Switerland, etc.). There is also a smaller
#cluster that falls somewhere closer to the average consumption of both cereal
#and red meat (consisting of countries  Italy, Spain, and Portugal). In
#general, these country groups make sense based on proximity.
#Running the K-means procedure 100 times
set.seed(0)
km.proteinsim = kmeans(protein[,-1], centers = 4, nstart = 100)
km.proteinsim
par(mfrow = c(1, 1))
plot(protein$Cereals, protein$RedMeat, type = "n",
     xlab = "Cereal Consumption", ylab = "Red Meat Consumption",
     main = paste("Best K-Means Attempt out of 100\n WCV: ",
                  round(km.proteinsim$tot.withinss, 4)))
points(km.proteinsim$centers[, 6], km.proteinsim$centers[, 1], pch = 16, col = "blue")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
text(protein$Cereals, protein$RedMeat,
     labels = protein[,1],
     col = km.proteinsim$cluster)
c=km.proteinsim$cluster
table(c)

