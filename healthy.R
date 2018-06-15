library(tidyverse)

# Load data as matrix
healthy <- read_csv("healthy.csv", col_names=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

image(healthyMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

healthyVector <- as.vector(healthyMatrix)
# Next line would crash R, requires too much memory. healthyVector is len 365656
#### distance <- dist(healthyVector, method="euclidean")
# str(healthyVector)
# healthyVector is 365636, mem is n*(n-1)/2, 67 GB

k <- 5
set.seed(1)
KMC <- kmeans(healthyVector, centers=k, iter.max=1000)
str(KMC)
# assigns each intensity value to a cluster
healthyClusters <- KMC$cluster
# KMC$centers already has the mean of each cluster
# KMC$size is the size of each cluster
# note that the largest cluster has the lowest intensity (mean)
KMC$centers[2] # mean of cluster 2

# Let's plot
dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=FALSE, col=rainbow(k))
