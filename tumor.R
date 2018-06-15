library(tidyverse)
library(flexclust)
# Requires healthy.R to run first to set environment

# Load data
tumor <- read_csv("tumor.csv", col_names=FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

# Preview
image(tumorMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

# create k-centroids cluster analysis
KMC_kcca <- as.kcca(KMC, healthyVector) # model and training set
tumorClusters <- predict(KMC_kcca, newdata=tumorVector) # new data

# Visualize result
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))
# notes:
# * healthy image shows eyes in front, tumor.csv does not, so likely taken at
# different section
# * tumor intensity is similar to eye intensity


