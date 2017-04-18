rm(list=ls())

library(igraph)


# This section shows that if p is pc=0.00725,
# Then, the 50% of graphs generated will be fully connected
NumNodes <- 1000
pc <- 0.00725

# Number of trials to iterate over
NumTrials <- 10000

# Counts number of fully connected graphs over trials
numConnected <- 0 

# Monte Carlo Loop, counts number of fully connected ER graphs over NumTrials
for (i in 1:NumTrials)
{
# Generate random Erdos-Renyi Graph according to parameters
g1 <- sample_gnp(NumNodes,pc)

# Add them to count if the graph is fully connected
if (is.connected(g1)) numConnected <- numConnected + 1;
}
# Should be roughly 0.5000
numConnected/NumTrials