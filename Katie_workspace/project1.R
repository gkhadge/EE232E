#Project1 
#Problem #1 

rm(list=ls())
library(igraph)

#setwd('/Users/Yusi/Documents/EE232E/HW_3')
setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project1/')
proj1graph <- "facebook_combined.txt"

# Download the Facebook graph edgelist file facebook_combined.txt
# From readme-Ego.txt file, Edges are undirected for facebook
g <- read_graph(proj1graph,format="ncol",directed=FALSE)

# Is the network connected? 
is.connected(g)

# Measure the diameter of the network 
diameter(g)

# Plot the degree distribution
dg_dist <- degree_distribution(g)
plot(dg_dist, main = "P1 degree distribution of Facebook graph")
# Histogram of degree distribution
g_dg <- degree(g)
g_dg_hist <- hist(g_dg ,plot=FALSE)
plot(g_dg_hist$count, log="xy", type='h', lwd=10, lend=2, main = "P1 Degree Distribution Histogram of Facebook graph", xlab = "Degree", ylab = "Number of Nodes")

# CHECK: Curve Fitting 
x <- seq(0,length(dg_dist),length = length(dg_dist))
fit2 <- lm(dg_dist~poly(x,3,raw=TRUE))
xx <- seq(0,length(dg_dist),length = length(dg_dist))
plot(x,dg_dist)
lines(xx, predict(fit2, data.frame(x=xx)), col="red")
# CHECK: Curve Fitting
lo <- loess(dg_dist~x)
plot(x,dg_dist)
lines(predict(lo), col='red', lwd=2)

# CHECK: What is your curve's total mean squared error? 
mean(fit2$residuals^2)

# What is the average degree? 
avg_dg <- mean(degree(g))


#Problem 2 
# CHECK: Take the node 1 (the node whose ID is 1) => Guessing it is V(g)[1]?
# and find its neighbors
node1_neighbors <- neighbors(g, V(g)[1])

#Create a graph that consists of node 1 and its neighbors and the edges that have both ends within this set of nodes 
node1_personal_nodes <- c(V(g)[1],node1_neighbors)
node1_personal_network <- induced_subgraph(g, node1_personal_nodes)

num_nodes_1 <- length(V(node1_personal_network))
num_edges_1 <- length(E(node1_personal_network))


