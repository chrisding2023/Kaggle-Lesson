#Retrieving the numerical measures of the iris dataset.
iris.meas = iris[, -5]
str(iris.meas)
summary(iris.meas)
sapply(iris.meas, sd)

#A function to help determine the number of clusters 
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
}
#Visualizing the scree plot for the scaled iris data; 3 seems like a plausible
#choice.
wssplot(iris.meas)

#Conducting the K-Means algorithm on the whole dataset.
set.seed(0)
km.iris = kmeans(iris.meas, centers = 3, nstart = 100)

#Inspecting the output of the kmeans() function.
km.iris

#Visualizing the results against the truth.
par(mfrow = c(1, 2))
plot(iris.meas$Petal.Width, iris.meas$Sepal.Width,
     xlab = "Petal Width", ylab = "Sepal Width",
     main = "K-Means Clustering", col = km.iris$cluster)
plot(iris.meas$Petal.Width, iris.meas$Sepal.Width,
     xlab = "Petal Width", ylab = "Sepal Width",
     main = "True Species", col = iris$Species)

#Plotting the cluster centers over the data.
par(mfrow = c(1, 1))
plot(iris.meas$Petal.Width, iris.meas$Sepal.Width,
     xlab = "Petal Width", ylab = "Sepal Width",
     main = "K-Means Clustering", col = km.iris$cluster)
points(km.iris$centers[, 4], km.iris$centers[, 2], pch = 16, col = "blue")


