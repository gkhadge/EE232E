rm(list=ls())
library(igraph)

#setwd('/Users/Yusi/Documents/EE232E/HW_3')
setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/HW_3')
hw3graph <- "sorted_directed_net.txt"

# Read in graph from file
g <- read_graph(hw3graph,format="ncol",directed=TRUE)

# Problem1
is.connected(g)

# Extract GCC
cl <- clusters(g)
gccIndex <- which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)

# Problem 2
# Plot Outgoing Degree Distribution
gcc_dg_out <- degree(gcc, mode = "out")
gcc_dg_out_hist <- hist(gcc_dg_out,plot=FALSE)
plot(gcc_dg_out_hist$count, log="xy", type='h', lwd=10, lend=2, main = "Outgoing Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")
#plot(degree.distribution(gcc, mode = "out"), main="Outgoing Degree Distribution")

# Plot Incoming Degree Distribution
gcc_dg_in <- degree(gcc, mode = "in")
gcc_dg_in_hist <- hist(gcc_dg_in,plot=FALSE)
plot(gcc_dg_in_hist$count, log="xy", type='h', lwd=10, lend=2, main = "Incoming Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")
#plot(degree.distribution(gcc, mode = "in"), main="Incoming Degree Distribution")

# Problem 3
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

barplot(sizes(gcc_ud1_comm),  main="Community Structure ('each' Edge LP Method)", xlab="Community Number", ylab="Community Size")
barplot(sizes(gcc_ud2_comm_FG),  main="Community Structure ('mean' Edge FG Method)", xlab="Community Number", ylab="Community Size")
barplot(sizes(gcc_ud2_comm_LP),  main="Community Structure ('mean' Edge LP Method)", xlab="Community Number", ylab="Community Size")

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

barplot(sizes(gcc_LC_ud_FG),  main="Community Structure of Largest Subcommunity", xlab="Community Number", ylab="Community Size")

#Using induced.subgraph function
sub_gcc_LC <- induced.subgraph(gcc_ud2, which(membership(gcc_ud2_comm_FG) == largest_comm_index))
sub_gcc_LC_fg <- fastgreedy.community(sub_gcc_LC)

#Problem 5 
#find the index of community size greater than 100
large_comm_index <- which(sizes(gcc_ud2_comm_FG)>100)
#find the subcommunity of each index 
for (i in large_comm_index){
  sub_gcc <- induced.subgraph(gcc_ud2, which(membership(gcc_ud2_comm_FG) == i))
  sub_gcc_fg <- fastgreedy.community(sub_gcc)
  
  barplot(sizes(sub_gcc_fg),  main=c("Community Structure of Subcommunity", i), xlab="Community Number", ylab="Community Size")
  
  #TODO: print information we need
}


#Problem 6 

#choose which optino to try Problem 6 
option <- 1

if(option == 2){
  g_6 <- gcc_ud2
  comm_struct <- gcc_ud2_comm_FG
}else {
  print("option1")
  g_6 <- gcc_ud1
  comm_struct <- gcc_ud1_comm
}
maxexample <- 4 
example_count <- 0

while(example_count<maxexample){
  # Random Walk and Teleport Params
  currStep <- 0
  maxSteps <- 35 # must be at least 31
  damping_factor <- 85 # in %
  # Initialize path. 
  path = c()
  
  #choose random start node i 
  startNode<- sample(length(V(g_6)),1)

  #PLEASE CHECK: Do we add startNode to the path?
  #path <- c(path,as.numeric(attributes(startNode)$name))
  
  # Run until you get enough steps in the path
  while (currStep < maxSteps)
  {
    # Calculate length of segment before teleportation occurs
    numStepsSeg <- 1 # Initialize number of steps before teleportation occurs
    
    # Calculate whether teleportation occurs
    smp <- sample(1:100,1)
    while(smp<=damping_factor)
    {
      # If no teleportation, continue incrementing numStepsSeg
      numStepsSeg <- numStepsSeg + 1
      smp <- sample(1:100,1)
    }
      
    print(numStepsSeg)
    if ((currStep + numStepsSeg) > maxSteps) {
      numStepsSeg = maxSteps - currStep
    }
    
    currentNode<- startNode
    
    for(t in 1:numStepsSeg){
      nodeNeighbors <- neighbors(g_6,V(g_6)[currentNode])
      #print("neighborNodes")
      #print(nodeNeighbors)
      numNeighbors <- length(nodeNeighbors)
      
      if(numNeighbors == 1){
        nextNode <- nodeNeighbors
      }else{
        #calculate edge weight 
        edge_weight <- rep(0,numNeighbors)
        for (k in 1:numNeighbors){
          edge_weight[k] <- E(g_6,P=c(V(g_6)[currentNode],V(g_6)[nodeNeighbors[k]]))$weight
        }
        #calculate probability 
        edge_weight_prob <-  rep(0,numNeighbors)
        sum_weight <- sum(edge_weight)
        for (l in 1:numNeighbors){
          edge_weight_prob[l] <- (1-edge_weight[l]/sum_weight)/(numNeighbors-1)
        }
        
       # print("edge_weight_prob")
       # print(edge_weight_prob)
        # sample nextNode
        nextNode <-  sample(nodeNeighbors, 1, replace = TRUE, prob = edge_weight_prob)
        # Append segment to path
        print("nextNode")
        print(nextNode)
      }
      path = c(path,as.numeric(V(g_6)[nextNode]))
      currentNode <- nextNode
    }
      
    # Increment number of steps
    currStep <- currStep + numStepsSeg
    print(currStep/maxSteps)
  }
  
  # Extract table of node Instances
  q <- table(path)
  # Calculate probability of being at each node, including the starting node due to teleportation
  pr <- q/length(path)

  #Visiting Probability of all nodes in the network
  visiting_prob = pr 
  
  #sort visiting probability 
  sorted_visiting_prob <- sort(visiting_prob,decreasing = TRUE, index.return=TRUE)
  
  #initialize M_i and m_j 
  M_i = rep(0,length(comm_struct))
  if(option == 1){
    print("option1")
    v_j_length <- as.numeric(length(pr))
  }else{
    v_j_length <- as.numeric(30)
  }
  #calculate M_i with nodes j of top 30 visiting probability
  for (j in 1:v_j_length){
    #get top ith visiting probability 
    m_j = rep(0,length(comm_struct))  # n dimensional vector wiht only one element being 1
                                      # denotes the community that v_j belongs to
    v_j <- sorted_visiting_prob[j]
    
    #get top ith visiting probability's node index 
    index_jth <- attributes(v_j)$name
    
    #get top ith node index's membership
    j_membership <- comm_struct$membership[as.numeric(index_jth)]
    print(j_membership)
    #build m_j (all 0s and 1 at j_membership)
    m_j[j_membership]<-1
    print("v_j and m_j")
    print(index_jth)
    print(m_j)
    
    #calculate M_i
    M_i <- M_i + (v_j*m_j)
    #print("M_i")
    #print(M_i)
  
  }
  print("M_i")
  print(M_i)
  
  #find out whether M_i includes multiple values greater than the threshold
  numComm <- 0 
  for(k in 1:length(M_i)){
    if(M_i[k]>0.1){
      numComm <- numComm + 1
    }
  }
  
  #if there are multiple values passing the threshold from prvious for loop, add count to example_count
  if(numComm >1){
    print("startNode with multiple communities")
    print("F: startnode is")
    print(V(g_6)[startNode])
    print("M_i")
    print(M_i)
    example_count <- example_count + 1
  }
}  








  
#PLEASE CHECK: Visualize the result
#startNode should be the same as the startNode from above to compare the results
#startNode <-1090

#Initialize subgraph's verticies
sub_v <- c()
#add startNode to the sub-graph's verticies array
sub_v <- c(sub_v,startNode)
#Get neighbors of the startNode
nodesNeighbors_start <- neighbors(g_6,startNode)
print(nodesNeighbors_start)

#for each neighbor of the startNode, add its neighbors to sub_v array
for (r in 1:length(nodesNeighbors_start)){
  sub_v <- c(sub_v,neighbors(g_6,nodesNeighbors_start[r]))
}

#remove the duplicates in sub_v array
unique_sub_v <- unique(sub_v)
#create subgraph with startNode, its neighbors, and their neighbors
sub_g_6 <- induced_subgraph(g_6, unique_sub_v)
plot(sub_g_6,  layout=layout_with_fr, vertex.size = 10, vertex.label.dist = 1,edge.arrow.size=0.5)



#sanity check for numNeighbors == 1
g_6 <- gcc_ud2
comm_struct <- gcc_ud2_comm_FG
count <- 0 
currentNode <- 1 


while(count <1){

  nodeNeighbors <- neighbors(g_6,currentNode)
  numNeighbors <- length(nodeNeighbors)
  
  if(numNeighbors == 1){
    print("currentNode")
    print(currentNode)
    print(nodeNeighbors)
    print(numNeighbors)
    count <-1 
  }else{
    print("no")
    print(currentNode)
    currentNode <- currentNode +1
  }
  
}

  
  
