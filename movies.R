library(tidyverse)

# I edited movieLens.txt to add column headings
movies <- read_delim("movielens.txt", "|")
movies <- unique(movies)

