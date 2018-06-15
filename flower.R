library(tidyverse)

flower <- read.csv("flower.csv", header=FALSE)
str(flower)

# Convert to a vector
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector <- as.vector(flowerMatrix)

# Use clustering to reduce number of colors
distance <- dist(flowerVector, method="euclidean")
clusterIntensity <- hclust(distance, method="ward.D")
plot(clusterIntensity)  # plot to determine best # of clusters.

# Cluster into 3 color values
rect.hclust(clusterIntensity, k=3, border="red")
flowerClusters <- cutree(clusterIntensity, k=3)
tapply(flowerVector, flowerClusters, mean) # apply mean() to each cluster
# 1          2          3 
# 0.08574315 0.50826255 0.93147713 
# cluster 1 is darkest, 3 is the lightest

# Give vector dimensions and plot
dim(flowerClusters) = c(50, 50)
image(flowerClusters, axes=FALSE)
image(flowerMatrix, axes=FALSE, col = grey(seq(0,1, length=256)))

