rm(list=ls())
library(igraph)

setwd('/Users/Yusi/Documents/EE232E/HW_3/')
hw3graph <- "sorted_directed_net.txt"

# Read in graph from file
g <- read_graph(hw3graph,format="ncol",directed=TRUE)

#Problem1
is.connected(g)

# Extract GCC
cl <- clusters(g)
gccIndex <- which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)

# Plot Outgoing Degree Distribution
gcc_dg_out <- degree(gcc, mode = "out")
gcc_dg_out_hist <- hist(gcc_dg_out, plot=FALSE)
plot(gcc_dg_out_hist$count, log="y", type='h', lwd=10, lend=2, main = "Outgoing Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")
#plot(degree.distribution(gcc, mode = "out"), main="Outgoing Degree Distribution")

# Plot Incoming Degree Distribution
gcc_dg_in <- degree(gcc, mode = "in")
gcc_dg_in_hist <- hist(gcc_dg_in,plot=FALSE)
plot(gcc_dg_in_hist$count, log="y", type='h', lwd=10, lend=2, main = "Incoming Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")
#plot(degree.distribution(gcc, mode = "in"), main="Incoming Degree Distribution")

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

#find the size of each community
sizes(gcc_ud1_comm)
sizes(gcc_ud2_comm_FG)
sizes(gcc_ud2_comm_LP)

#find the modularity of community structure
modularity(gcc_ud1_comm)
modularity(gcc_ud2_comm_FG)
modularity(gcc_ud2_comm_LP)

#Problem 4 

# TODO: PLEASE VERIFY CODE
# Extract Largest Community form UD2_FG from 
largest_comm_index <- which.max(sizes(gcc_ud2_comm_FG))

nonLCNodes <- (1:vcount(gcc))[gcc_ud2_comm_FG$membership != largest_comm_index]
gcc_LC <- delete.vertices(gcc, nonLCNodes) #GCC Largest Community
# plot(gcc_LC, vertex.size=5, vertex.label.cex=0.4,edge.arrow.size=0.2)

# Convert gcc_LC to undirected graph (Option 2)
gcc_LC_ud <- as.undirected(gcc_LC,mode="collapse",edge.attr.comb = "prod")
E(gcc_LC_ud)$weight <- sqrt(E(gcc_LC_ud)$weight)

# Find Communities of subnet using FG
gcc_LC_ud_FG <- fastgreedy.community(gcc_LC_ud)

#Using induced.subgraph function
sub_gcc_LC <- induced.subgraph(gcc_ud2, which(membership(gcc_ud2_comm_FG)) == largest_comm_index)
sub_gcc_LC_fg <- fastgreedy.community(sub_gcc_LC)

#Problem 5 
#find the index of community size greater than 100
large_comm_index <- which(sizes(gcc_ud2_comm_FG)>100)
#find the subcommunity of each index 
for (i in large_comm_index){
  sub_gcc <- induced.subgraph(gcc_ud2, which(membership(gcc_ud2_comm_FG) == i))
  sub_gcc_fg <- fastgreedy.community(sub_gcc)
  
  #TODO: print information we need
}


#Problem 6 
g_6 <- gcc_ud2
comm_struct <- gcc_ud2_comm_FG

# Random Walk and Teleport Params
currStep <- 0
minSteps <- 10000 # 1000000
damping_factor <- 85 # in %
# Initialize path. 
path = c()

#choose random start node i 
startNode<- sample(vcount(g),1)

# Run until you get enough steps in the path
while (currStep < minSteps)
{
  # Calculate length of segment before teleportation occurs
  numStepsSeg <- 1 # Number of steps before teleportation occurs
  
  # Calculate whether teleportation occurs
  smp <- sample(1:100,1)
  while(smp<=damping_factor)
  {
    # If no teleportation, continue incrementing numStepsSeg
    numStepsSeg <- numStepsSeg + 1
    smp <- sample(1:100,1)
  }
  
  # while currStep < minSteps
  # set start node
  # P = (1-(largest weight))/total weight
  
  # Single random walk segment, start node chosen by pagerank, run for numStepsSeg
  rw_seg <- random_walk(g_6, start = startNode, steps = numStepsSeg)
  # Append segment to path
  path = c(path,rw_seg)
  
  # Increment number of steps
  currStep <- currStep + numStepsSeg
  print(currStep/minSteps)
}

# Extract table of node Instances
q <- table(path)
# Calculate probability of being at each node
pr <- q/currStep
#Visiting Probability of all nodes in the network
visiting_prob = pr 

#sort visiting probability 
sorted_visiting_prob <- sort(visiting_prob,decreasing = TRUE, index.return=TRUE)

#initialize M_i and m_j 
M_i = rep(0,length(comm_struct))
m_j = rep(0,length(comm_struct)) #n dimensional vector wiht only one element being 1

for (j in 1:30){
  #get top ith visiting probability 
  v_j <- sorted_visiting_prob[j]
  #get top ith visiting probability's node index 
  index_jth <- attributes(v_j)$name
  
  #get top ith node index's membership
  j_membership <-comm_struct$membership[index_jth]
  
  #build m_j (all 0s and 1 at j_membership)
  m_j[j_membership]<-1
  
  #calculate M_i
  M_i <- M_i + (v_j*m_j)
  
}







