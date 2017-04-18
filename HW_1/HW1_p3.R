# HW 1, Problem 3

rm(list = ls())

library(igraph)

# create barabasi graph
nodesNum = 1000
g <- barabasi.game(nodesNum , directed = FALSE)

plot(g, vertex.size=8, vertex.label=NA)

dg_dist = degree(g)
hist(deg_dist, main="Degree Distribution (3a)", xlab="Degree Number", ylab="Probability")

# find communities
fg <- fastgreedy.community(g)
membership(fg)
cmsize <- sizes(fg)

barplot(cmsize,  main="Community Sizes (3b)", xlab="Community Number", ylab="Community Size")
modularity(fg, membership(fg))