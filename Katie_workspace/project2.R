#Project 2 
#Problem 1

rm(list=ls())
library(igraph)


setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/')
mydata = read.table("actor_movies.txt", header = TRUE, sep = "\t", quote = '"', dec = ".", fill = TRUE, comment.char = "")