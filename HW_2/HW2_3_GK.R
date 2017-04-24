library(matrixStats)
library(igraph)

#problem 3 (a)
# Parameters
numNodes <- 1000
p <- 0.01
numSteps <- 1000000

# regenerate graph until it is connected
g_3a <- sample_gnp(numNodes, p, directed = FALSE)
while (!is.connected(g_3b))
{
  g_3a <- sample_gnp(numNodes, p, directed = FALSE)
  print("Not Connected, Regenerating")
}

# Single random walk, start node arbitrarily chosen to be 1
w <- random_walk(g_3a, start = 1, steps = numSteps)
# Extract table of node Instances
q <- table(w)
# Calculate probability of being at each node
pr <- q/numSteps
# Find degree of each node
deg_g_3a <- degree(g_3a)
# Plot against each other
plot(deg_g_3a,pr,xlim=range(0,max(deg_g_3a)),ylim=range(0,max(pr)), main="3(a) Probability of visit vs. Degree of each node", ylab="Probability of visit", xlab="Degree of node")


#Problem 3 (b)
# Parameters
numNodes <- 1000
p <- 0.01
numSteps <- 1000000

# regenerate graph until it is connected
g_3b <- sample_gnp(numNodes, p, directed = TRUE)
while (!is.connected(g_3b))
{
  g_3b <- sample_gnp(numNodes, p, directed = TRUE)
  print("Not Connected, Regenerating")
}

# Single random walk, start node arbitrarily chosen to be 1
w <- random_walk(g_3b, start = 1, steps = numSteps)
# Extract table of node Instances
q <- table(w)
# Calculate probability of being at each node
pr <- q/numSteps
# Find degree of each node
deg_g_3b_out <- degree(g_3b,mode = "out")
deg_g_3b_in <- degree(g_3b,mode = "in")
# Plot against each other
plot(deg_g_3b_out,pr[1:length(deg_g_3b_out)],xlim=range(0,max(deg_g_3b_out)),ylim=range(0,max(pr)), main="3(b) Probability of visit vs. Outgoing Degree of each node", ylab="Probability of visit", xlab="Outgoing Degree of node")
plot(deg_g_3b_in,pr[1:length(deg_g_3b_in)],xlim=range(0,max(deg_g_3b_in)),ylim=range(0,max(pr)), main="3(b) Probability of visit vs. Incoming Degree of each node", ylab="Probability of visit", xlab="Incoming Degree of node")

#problem 3 (c)
# Random Walk and Teleport Params
currStep <- 0
minSteps <- 100000
damping_factor <- 85 # in %

# Graph params
numNodes <- 1000
p <- 0.01

# regenerate graph until it is connected
g_3c <- sample_gnp(numNodes, p, directed = FALSE)
while (!is.connected(g_3b))
{
  g_3c <- sample_gnp(numNodes, p, directed = FALSE)
  print("Not Connected, Regenerating")
}

# Initialize path. 
path = c()

# Run until you get enough steps in the path
while (currStep < minSteps)
{
  # Calculate length of segment before teleportation occurs
  numStepsSeg <- 1 # Number of steps before teleportation occurs
  
  # Calculate whether teleportation occurs
  smp <- sample(1:100,1)
  while(smp<damping_factor)
  {
    # If no teleportation, continue incrementing numStepsSeg
    numStepsSeg <- numStepsSeg + 1
    smp <- sample(1:100,1)
  }
  # Single random walk segment, start node chosen randomly, run for numStepsSeg
  rw_seg <- random_walk(g_3c, start = sample(1:numNodes,1), steps = numStepsSeg)
  # Append segment to path
  path = c(path,rw_seg)
  
  # Increment number of steps
  currStep <- currStep + numStepsSeg
}

# Extract table of node Instances
q <- table(path)
# Calculate probability of being at each node
pr <- q/currStep
# Find degree of each node
deg_g_3c <- degree(g_3c)
# Plot against each other
plot(deg_g_3c,pr,xlim=range(0,max(deg_g_3c)),ylim=range(0,max(pr)), main="3(c) Probability of visit vs. Degree of each node", ylab="Probability of visit", xlab="Degree of node")
