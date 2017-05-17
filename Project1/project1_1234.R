#Project1 
#Problem #1 

rm(list=ls())
library(igraph)

#setwd('C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Project1')
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

x <- seq(1,length(dg_dist),length = length(dg_dist))
dg_dist_log_raw <-log(dg_dist)
plot(x,dg_dist,log = "xy", main = "Raw Degree Distribution", xlab = "Degree", ylab = "Probability")

dg_dist_log <- dg_dist_log_raw
dg_dist_log[is.infinite(dg_dist_log_raw)] <- NA
dg_dist_log[which(dg_dist_log < -7)] <- NA
dg_dist_log[which(dg_dist_log > -4)] <- NA
# clean up data by taking out -inf values, and values that are constant (would skew fit)

NA_ind <- which(is.na(dg_dist_log))
dg_dist_log <- dg_dist_log[-c(NA_ind)]
x <- x[-c(NA_ind)]
x_log = log(x)
fit <- lm(dg_dist_log ~ x_log)

plot(x_log,dg_dist_log, main = "Fitted Degree Distribution", xlab = "log(Degree)", ylab = "log(Probability)")
dg_dist_predict = fit$coefficients[2]*x_log+fit$coefficients[1]
lines(x_log, dg_dist_predict, col='red')

# CHECK: What is your curve's total mean squared error? 
mean(fit$residuals^2)

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

plot(node1_personal_network, vertex.size = 5, vertex.label = NA, main = c("Personal Network of Node", V(g)[1]))
V(g)[1]$color <- "#FF0000FF"
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

plot(core_personal_network, vertex.label = NA, main = "Core Personal Network")

# Fast Greedy Community Finding Method
core_fg <- fastgreedy.community(core_personal_network)
colors_fg <- rainbow(length(core_fg))
colors_fg[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(core_fg), col=colors_fg, main="Community Structure of Core Personal Network (Fast Greedy)", xlab="Community Number", ylab="Community Size")
plot(core_personal_network,vertex.color=colors_fg[membership(core_fg)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main="Community Structure of Core Personal Network (Fast Greedy)")

# Edge Betweenness Community Finding Method

core_eb <- cluster_edge_betweenness(core_personal_network, weights = E(core_personal_network)$weight, directed = FALSE)
n_large_comm = 8
greys_eb <- replicate(length(core_eb)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey
colors_eb <- c(rainbow(n_large_comm), greys_eb)
colors_eb[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(core_eb), col=colors_eb, main="Community Structure of Core Personal Network (Edge Betweenness) of Node", xlab="Community Number", ylab="Community Size")
plot(core_personal_network,vertex.color=colors_eb[membership(core_eb)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main="Community Structure of Core Personal Network (Edge Betweeness) of Node")

# Infomap Community Finding Method
core_im <- cluster_infomap(core_personal_network, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
greys_im <- replicate(length(core_im)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey
colors_im <- c(rainbow(n_large_comm), greys_im)
colors_im[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(core_im), col=colors_im, main="Community Structure of Core Personal Network (Infomap) of Node", xlab="Community Number", ylab="Community Size")
plot(core_personal_network,vertex.color=colors_im[membership(core_im)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main="Community Structure of Core Personal Network (Infomap) of Node")

# Problem 4

# only use neighbors of the core nodes to construct network
core_neighbor_network <- induced_subgraph(g, core_neighbors)
plot(core_neighbor_network, vertex.label = NA, vertex.size = 3)

# Repeat community finding methods

# Fast Greedy Community Finding Method
core_neigh_fg <- fastgreedy.community(core_neighbor_network)
n_large_comm = 8
greys_neigh_fg <- replicate(length(core_neigh_fg)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey
colors_neigh_fg <- c(rainbow(n_large_comm), greys_neigh_fg)
colors_neigh_fg[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(core_neigh_fg), col=colors_neigh_fg, main="Community Structure of Core Neighbor Network (Fast Greedy) of Node", xlab="Community Number", ylab="Community Size")
plot(core_neighbor_network,vertex.color=colors_neigh_fg[membership(core_neigh_fg)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main="Community Structure of Core Neighbor Network (Fast Greedy)")

# Edge Betweenness Community Finding Method
core_neigh_eb <- cluster_edge_betweenness(core_neighbor_network, weights = E(core_neighbor_network)$weight, directed = FALSE)
greys_neigh_eb <- replicate(length(core_neigh_eb)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey
colors_neigh_eb <- c(rainbow(n_large_comm), greys_neigh_eb)
colors_neigh_eb[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(core_neigh_eb), col=colors_neigh_eb, main="Community Structure of Core Neighbor Network (Edge Betweenness)", xlab="Community Number", ylab="Community Size")
plot(core_neighbor_network,vertex.color=colors_neigh_eb[membership(core_neigh_eb)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main="Community Structure of Core Neighbor Network (Edge Betweenness)")

# Infomap Community Finding Method
core_neigh_im <- cluster_infomap(core_neighbor_network, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
greys_neigh_im <- replicate(length(core_neigh_im)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey
colors_neigh_im <- c(rainbow(n_large_comm), greys_neigh_im)
colors_neigh_im[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(core_neigh_im), col=colors_neigh_im, main="Community Structure of Core Neighbor Network (Infomap)", xlab="Community Number", ylab="Community Size")
plot(core_neighbor_network,vertex.color=colors_neigh_im[membership(core_neigh_im)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main="Community Structure of Core Neighbor Network (Infomap)")

# http://igraph.org/r/doc/cocitation.html

# Problem 5: Plotting High Dispersion Nodes

c_node <- 1
v_disp <- "19"
v_emb <- "56"
# v_disp_emb <- "19"

core_neighbors <- neighbors(g, v=core_nodes[c_node])
core_personal_nodes <- c(core_nodes[c_node], core_neighbors)
core_personal_network <- induced_subgraph(g, core_personal_nodes)

core_fg <- fastgreedy.community(core_personal_network)
colors_fg <- rainbow(core_fg)
# 
# n_large_comm = 8
# greys_fg <- replicate(length(core_fg)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey
# colors_fg <- c(rainbow(n_large_comm), greys_fg)
# colors_fg[3] = "#FFFF00"    # set third color to yellow for better differentiation

# settings for non-special nodes
V(core_personal_network)$frame.color <- "grey"

# set a different frame color for special nodes
V(core_personal_network)[v_disp]$frame.color <- "black"
V(core_personal_network)[v_emb]$frame.color <- "black"
# V(core_personal_network)[v_disp_emb]$frame.color <- "black"

# settings for non-special edges
E(core_personal_network)$lty <- 1
E(core_personal_network)$width <- 1
E(core_personal_network)$color <- "gray"

disp_edges <- incident_edges(core_personal_network, v_disp)
emb_edges <- incident_edges(core_personal_network, v_emb)
# disp_emb_edges <- incident_edges(core_personal_network, v_disp_emb)

E(core_personal_network)$lty <- 3

for (i in 1:length(disp_edges[[1]])){
  # highest dispersion
  E(core_personal_network)[disp_edges[[1]][i]]$color <- "SkyBlue2"
  E(core_personal_network)[disp_edges[[1]][i]]$lty <- 1
  E(core_personal_network)[disp_edges[[1]][i]]$width <- 3
}

for (i in 1:length(emb_edges[[1]])){
  # highest dispersion
  E(core_personal_network)[emb_edges[[1]][i]]$color <- "DarkGreen"
  E(core_personal_network)[emb_edges[[1]][i]]$lty <- 1
  E(core_personal_network)[emb_edges[[1]][i]]$width <- 3
}
# 
# for (i in 1:length(disp_emb_edges[[1]])){
#   # highest dispersion
#   E(core_personal_network)[disp_emb_edges[[1]][i]]$color <- "Red"
#   E(core_personal_network)[disp_emb_edges[[1]][i]]$lty <- 1
#   E(core_personal_network)[disp_emb_edges[[1]][i]]$width <- 3
# }

plot(core_personal_network, vertex.color=colors_fg[membership(core_fg)], vertex.size = 5, vertex.label = NA, 
     layout=layout.fruchterman.reingold,main="Community Structure of Core Neighbor Network (Fast Greedy)")