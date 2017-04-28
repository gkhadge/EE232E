rm(list=ls())
library(igraph)

setwd('C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Gourav_workspace')
hw3graph <- "sorted_directed_net.txt"

# Read in graph from file
g <- read_graph(hw3graph,format="ncol",directed=TRUE)

# Extract GCC
cl <- clusters(g)
gccIndex <- which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)

# Plot Outgoing Degree Distribution
gcc_dg_in <- degree(gcc, mode = "out")
gcc_dg_in_hist <- hist(gcc_dg_in,plot=FALSE)
plot(gcc_dg_in_hist$count, log="y", type='h', lwd=10, lend=2, main = "Outgoing Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")

# Plot Incoming Degree Distribution
gcc_dg_out <- degree(gcc, mode = "in")
gcc_dg_out_hist <- hist(gcc_dg_out,plot=FALSE)
plot(gcc_dg_out_hist$count, log="y", type='h', lwd=10, lend=2, main = "Incoming Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")

# Convert to undirected graph (Option 1)
gcc_ud1 <- as.undirected(gcc, mode = "each")

# Convert to undirected graph (Option 2)
gcc_ud2 <- as.undirected(gcc,mode="collapse",edge.attr.comb = "prod")
E(gcc_ud2)$weight <- sqrt(E(gcc_ud2)$weight)

# Find Communities (Option 1)
gcc_ud1_comm <- label.propagation.community(gcc_ud1)

# Find Communities (Option 2)
gcc_ud2_comm_FG <- fastgreedy.community(gcc_ud2)
gcc_ud2_comm_LP <- label.propagation.community(gcc_ud2) 

# Note LP is awful, use FG when possible
