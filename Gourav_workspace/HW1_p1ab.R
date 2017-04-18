rm(list=ls())

library(igraph)

# 
p1 <- 0.01
p2 <- 0.05
p3 <- 0.1

# Number of Nodes
NumNodes <- 1000

# Generate graphs for three different probabilities
g1 <- sample_gnp(NumNodes,p1)
g2 <- sample_gnp(NumNodes,p2)
g3 <- sample_gnp(NumNodes,p3)

# Generate Degree Distribution Histograms
hist(degree(g1),main="Degree Distribution (p = 0.01)",xlab="Degree",ylabe="Frequency")
hist(degree(g2),main="Degree Distribution (p = 0.05)",xlab="Degree",ylabe="Frequency")
hist(degree(g3),main="Degree Distribution (p = 0.1)",xlab="Degree",ylabe="Frequency")

# Output connected information and diameter for all three graphs
is.connected(g1)
diameter(g1,directed=FALSE,unconnected=TRUE)

is.connected(g2)
diameter(g2,directed=FALSE,unconnected=TRUE)

is.connected(g3)
diameter(g3,directed=FALSE,unconnected=TRUE)
