library(matrixStats)
library(igraph)

#problem 4 (a)
# Random Walk and Teleport Params
currStep <- 0 
minSteps <- 20000
damping_factor <- 85 # in %

# Graph params
numNodes <- 1000
p <- 0.01

# regenerate graph until it is connected
g_4a <- sample_gnp(numNodes, p, directed = TRUE)
while (!is.connected(g_4a))
{
  g_4a <- sample_gnp(numNodes, p, directed = TRUE)
  print("Not Connected, Regenerating")
}

# Initialize path. 
path1 = c()

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
  # Single random walk segment, start node chosen randomly, run for numStepsSeg
  rw_seg <- random_walk(g_4a, start = sample(1:numNodes,1), steps = numStepsSeg)
  # Append segment to path
  path1 = c(path1,rw_seg)
  
  # Increment number of steps
  currStep <- currStep + numStepsSeg
  print(currStep/minSteps)
  
}

# Extract table of node Instances
q <- table(path1)
# Calculate probability of being at each node
pr <- q/currStep
page_rank_val1 = pr




#problem 4 (b)
currStep <- 0 
actual_pagerank <- page_rank(g_4a)$vector
# Initialize path. 
path2 = c()

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
  
  
  #Choose according to distribution in actual_pagerank
  startNode <- sample(1:1000, 1, replace = TRUE, prob = actual_pagerank)
  
  # Single random walk segment, start node chosen by pagerank, run for numStepsSeg
  rw_seg <- random_walk(g_4a, start = startNode, steps = numStepsSeg)
  # Append segment to path
  path2 = c(path2,rw_seg)
  
  # Increment number of steps
  currStep <- currStep + numStepsSeg
  print(currStep/minSteps)
}

# Extract table of node Instances
q <- table(path2)
# Calculate probability of being at each node
pr <- q/currStep
page_rank_val2 = pr

x_plot = 1:(length(page_rank_val))

degree_4a <- degree(g_4a)

print(mean(degree(g_4a, path1)))
print(median(degree(g_4a, path1)))

print(mean(degree(g_4a, path2)))
print(median(degree(g_4a, path2)))

#plot against built-in function
plot(degree_4a, page_rank_val1, xlim=range(0,max(degree_4a)),ylim=range(0,page_rank_val1),  main="PageRank of 4(a) and 4(b) vs. degree", xlab="degree",ylab="Probability")
points(degree_4a,page_rank_val2, col=2, pch=2)

#get maximum degree value
m <- max(degree_4a)
#initialize arrays with the length of the maxium degree
page_rank1_per_degree <- c(1:m)*0
rank_count1 <- c(1:m)*0 
page_rank2_per_degree <- c(1:m)*0 
rank_count2 <- c(1:m)*0 

#iterate through each nodes and calculate the sum of probability at each degree
#and count how many nodes have each degree. 
for (i in 1:numNodes){
  #n: degree at node i
  n <- degree_4a[i]
  #stores probability at each degree for Part a's pageRank
  page_rank1_per_degree[n] <- page_rank1_per_degree[n] + page_rank_val1[i]
  #stores the count of how many nodes have each degree in Part a 
  rank_count1[n] <- rank_count1[n] + 1
  #stores probability at each degree for Part b's pageRank
  page_rank2_per_degree[n] <- page_rank2_per_degree[n] + page_rank_val2[i]
  #stores the count of how many nodes have each degree in Part a 
  rank_count2[n] <- rank_count2[n] + 1
  print(i)
  print(page_rank2_per_degree)
}

#taking the average over each degree
for (t in 1: m){
  if(rank_count1[t]<= 0.0){
    page_rank1[t] <- 0.0
  }else{
    page_rank1[t] <- page_rank1_per_degree[t]/rank_count1[t]
  }
  
  if(rank_count2[t]<= 0.0){
    page_rank2[t] <- 0.0
  }else{
    page_rank2[t] <- page_rank2_per_degree[t]/rank_count2[t]
  }
}

#plot part a against part b 
x_plot = 1:m
plot(x_plot, page_rank1, xlim=range(1,length(x_plot)), ylim=range(0,max(page_rank1)),  main="Normal PageRank and Personalized PageRank vs. Degree", xlab="Degree",ylab="Probability")
points(x_plot,page_rank2, col=2, pch=2)
