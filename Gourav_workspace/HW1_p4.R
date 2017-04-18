# HW1, Problem 4
rm(list=ls())
library(igraph)

# Parameters for Forest Fire Model graph
number_of_nodes <- 1000
fw_prob <- 0.25

# generate graph using Forest Fire Model
g <- sample_forestfire(number_of_nodes, fw.prob=fw_prob)


# Plot Degree Distributions (in and out)
dd1 <- degree_distribution(g, mode="in")
dd2 <- degree_distribution(g, mode="out")
plot(seq(along=dd1)-1, dd1, log="xy", main="Degree Distribution (in and out)", xlab="Degree",ylab="Probability")
points(seq(along=dd2)-1, dd2, col=2, pch=2)

# Find communities
g_clusters <- cluster_infomap(g)

# Find diameter
diameter(g)

# Find modularity
modularity(g_clusters)

# Plot distribution of community sizes
cmsize <- sizes(g_clusters)
barplot(cmsize,main="Number of Communities of Various Sizes",xlab="Size",ylab="Number of Communities")
