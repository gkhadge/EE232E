#Project1 
# Gourav, Yusi, Katie

rm(list=ls())
library(igraph)

#setwd('C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Project1')
setwd('/Users/Yusi/Documents/EE232E/Project1')
#setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project1/')
proj1graph <- "facebook_combined.txt"

# Download the Facebook graph edgelist file facebook_combined.txt
# From readme-Ego.txt file, Edges are undirected for facebook
g <- read_graph(proj1graph,format="ncol",directed=FALSE)


#######################################
# Problem 5
#######################################

# Store data for embeddedness and dispersion to RData files for each core node

for (core_node in 1:length(core_nodes))
{
  print(core_node)
  core_neighbors <- neighbors(g, v=core_nodes[core_node])
  core_personal_nodes <- c(core_nodes[core_node], core_neighbors)
  core_personal_network <- induced_subgraph(g, core_personal_nodes)


  embedded_vec <- rep(NA_integer_, length(core_neighbors))
  dispersion_vec <- rep(NA_integer_, length(core_neighbors))

  cocitation_vec <- cocitation(core_personal_network, v=V(g)[core_nodes[core_node]]$name)

  #embedded_vec <- c(embedded_vec, cocitation(core_personal_network, v=V(g)[core_nodes[core_node]]$name))

  core_personal_network_without_core <- delete_vertices(core_personal_network, v=V(g)[core_nodes[core_node]]$name)

  default_path_length <- vcount(core_personal_network_without_core)

  for (disp_node in 1:length(core_neighbors))
  {

    sub_g <- delete_vertices(core_personal_network_without_core, core_neighbors[disp_node]$name)

    disp_node_neighbors <- neighbors(core_personal_network_without_core, core_neighbors[disp_node]$name)

    node_disp <- 0
    if (length(disp_node_neighbors) > 1)
    {
      a <- distances(sub_g, disp_node_neighbors$name, disp_node_neighbors$name)
      a[is.infinite(a)] <- default_path_length
      node_disp <- sum(a)
    }
    # dispersion_vec <- c(dispersion_vec,node_disp)
    # embedded_vec <- c(embedded_vec,cocitation_vec[core_neighbors[disp_node]$name])
    dispersion_vec[disp_node] <- node_disp
    embedded_vec[disp_node] <- cocitation_vec[V(g)[core_nodes[core_node]]$name,core_neighbors[disp_node]$name]

  }

  dispersion_vec <- dispersion_vec/2

  filename <- paste("D_E_data_",core_node,".RData",sep="")
  save(embedded_vec,dispersion_vec,file=filename)
}


# Accumulate data from all runs and plot histogram
total_embedded_vec <- c()
total_dispersion_vec <- c()
for (core_node in 1:length(core_nodes))
{
  filename <- paste("D_E_data_",core_node,".RData",sep="")
  load(filename)
  
  total_embedded_vec <- c(total_embedded_vec,embedded_vec)
  total_dispersion_vec <- c(total_dispersion_vec,dispersion_vec)
}


hist(total_embedded_vec, main = "Distribution of Embeddedness", xlab = "Embeddedness", ylab = "Number of Instances")
hist(log(total_dispersion_vec), main = "Distribution of Dispersion", xlab = "Log(Dispersion)", ylab = "Number of Instances")
hist(log(total_dispersion_vec/total_embedded_vec), main = "Distribution of D/E", xlab = "Log(Dispersion/Embeddedness)", ylab = "Number of Instances")


# Extract Community properties one node at a time
# Used Core Nodes: 1, 3, 5
core_node <- 5

# Create subgraphs for analysis
core_neighbors <- neighbors(g, v=core_nodes[core_node])
core_personal_nodes <- c(core_nodes[core_node], core_neighbors)
core_personal_network <- induced_subgraph(g, core_personal_nodes)
core_personal_network_without_core <- delete_vertices(core_personal_network, v=V(g)[core_nodes[core_node]]$name)

# Extract communities using Fast Greedy Algorithm
core_fg <- fastgreedy.community(core_personal_network)
num_comms <- length(core_fg)

# Initialize vectors to store data
community_dispersion <- vector(mode="list", length=num_comms)
community_embeddedness <- vector(mode="list", length=num_comms)

# Read from Stored Datafile. 
# Load: dispersion_vec, embedded_vec
filename <- paste("D_E_data_",core_node,".RData",sep="")
load(filename)

for (disp_node in 1:length(core_neighbors))
{
  a <- membership(core_fg)[core_neighbors[disp_node]$name]
  community_dispersion[[a]] = c(community_dispersion[[a]],dispersion_vec[disp_node])
  community_embeddedness[[a]] = c(community_embeddedness[[a]],embedded_vec[disp_node])
}

# Initialize data stats vectors
mean_dispersions <- rep(NA_integer_, num_comms) 
mean_embeddedness <- rep(NA_integer_, num_comms)
mean_d_e <- rep(NA_integer_, num_comms) 
median_dispersions <- rep(NA_integer_, num_comms)
median_embeddedness <- rep(NA_integer_, num_comms) 
median_d_e <- rep(NA_integer_, num_comms) 
median_norm_d <- rep(NA_integer_, num_comms) 
median_norm_e <- rep(NA_integer_, num_comms) 
comm_edge_density <- rep(NA_integer_, num_comms) 

# Extract Data Stats
for (comm in 1:num_comms)
{
  mean_dispersions[comm] <- mean(community_dispersion[[comm]],na.rm=TRUE)
  mean_embeddedness[comm] <- mean(community_embeddedness[[comm]],na.rm=TRUE)
  mean_d_e[comm] <- mean(community_dispersion[[comm]]/community_embeddedness[[comm]],na.rm=TRUE)
  
  median_dispersions[comm] <- median(community_dispersion[[comm]],na.rm=TRUE)
  median_embeddedness[comm] <- median(community_embeddedness[[comm]],na.rm=TRUE)
  median_d_e[comm] <- median(community_dispersion[[comm]]/community_embeddedness[[comm]],na.rm=TRUE)
  median_norm_d[comm] <- median(community_dispersion[[comm]]/length(core_fg[[comm]]),na.rm=TRUE)
  median_norm_e[comm] <- median(community_embeddedness[[comm]]/length(core_fg[[comm]]),na.rm=TRUE)
  
  community_network <- induced_subgraph(g, core_fg[[comm]])
  comm_edge_density[comm] <- edge_density(community_network)*length(core_fg[[comm]])
}

print(core_neighbors[which.max(dispersion_vec)])
print(core_neighbors[which.max(embedded_vec)])
print(core_neighbors[which.max(dispersion_vec/embedded_vec)])
print(num_comms)

print(mean_d_e)
print(mean_dispersions)
print(mean_embeddedness)
print(comm_edge_density)

#######################################
# Problem 6
#######################################

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
