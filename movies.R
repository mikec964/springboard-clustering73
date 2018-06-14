library(tidyverse)

# I edited movieLens.txt to add column headings
movies <- read_delim("movielens.txt", "|")
movies <- unique(movies)

movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
movies <- unique(movies)  # some movies are no longer unique

#=====
# cluster movies
distances <- dist(movies[2:20], method="euclidean")
clusterMovies <- hclust(distances, method="ward.D")
plot(clusterMovies)
# looks like 10 clusters is a good place to cut

#=====
# analyze clusters
clusterGroups <- cutree(clusterMovies, k=10)
tapply(movies$Action, clusterGroups, mean) 
# about 78% of movies in cluster 2 have action
tapply(movies$Romance, clusterGroups, mean)
# about 10% of the movies in cluster 1 have romance

#=====
# Make recommendations
subset(movies, Title=="Men in Black (1997)") # find which row it's in
clusterGroups[257] # which groups have this movie?
cluster2 <- subset(movies, clusterGroups ==2)
cluster2$Title[1:10]
# recommend moves like Apollo 13 and The Fugitive
