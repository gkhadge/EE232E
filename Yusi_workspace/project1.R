#Project1 
#Problem #1 

rm(list=ls())
library(igraph)

setwd('/Users/Yusi/Documents/EE232E/Project1')
#setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project1/')
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
plot(x,dg_dist, xlab = "Degree", ylab = "Probability")
lines(xx, predict(fit2, data.frame(x=xx)), col="red")
# CHECK: Curve Fitting
lo <- loess(dg_dist~x)
plot(x,dg_dist, xlab = "Degree", ylab = "Probability")
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


# Problem 3

dgs <- degree(g)
core_nodes = c()
for (i in seq(1,length(dgs))){
  if (degree(g, v = V(g))[i] > 200){
    core_nodes <- c(core_nodes, V(g)[i])
  }
}

length(core_nodes)
avg_dg_core <- mean(degree(g, v = core_nodes))

print(avg_dg_core)

core_neighbors <- neighbors(g, v=core_nodes[1])

core_personal_nodes <- c(core_nodes[1], core_neighbors)
core_personal_network <- induced_subgraph(g, core_personal_nodes)

plot(core_personal_network)

# Fast Greedy Community Finding Method
core_fg <- fastgreedy.community(core_personal_network)
barplot(sizes(core_fg),  main=c("Community Structure of Core Personal Network (Fast Greedy)", i), xlab="Community Number", ylab="Community Size")

# Edge Betweenness Community Finding Method
core_eb <- cluster_edge_betweenness(core_personal_network, weights = E(core_personal_network)$weight, directed = FALSE)
barplot(sizes(core_eb),  main=c("Community Structure of Core Personal Network (Edge Betweenness)", i), xlab="Community Number", ylab="Community Size")

# Infomap Community Finding Method
core_im <- cluster_infomap(core_personal_network, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
barplot(sizes(core_im),  main=c("Community Structure of Core Personal Network (Infomap)", i), xlab="Community Number", ylab="Community Size")

# Problem 4

# only use neighbors of the core nodes to construct network
core_neighbor_network <- induced_subgraph(g, core_neighbors)
plot(core_neighbor_network)

# Repeat community finding methods

# Fast Greedy Community Finding Method
core_neigh_fg <- fastgreedy.community(core_neighbor_network)
barplot(sizes(core_neigh_fg),  main=c("Community Structure of Core Neighbor Network (Fast Greedy)", i), xlab="Community Number", ylab="Community Size")

# Edge Betweenness Community Finding Method
core_neigh_eb <- cluster_edge_betweenness(core_neighbor_network, weights = E(core_personal_network)$weight, directed = FALSE)
barplot(sizes(core_neigh_eb),  main=c("Community Structure of Core Neighbor Network (Edge Betweenness)", i), xlab="Community Number", ylab="Community Size")

# Infomap Community Finding Method
core_neigh_im <- cluster_infomap(core_neighbor_network, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
barplot(sizes(core_neigh_im),  main=c("Community Structure of Core Neighbor Network (Infomap)", i), xlab="Community Number", ylab="Community Size")

