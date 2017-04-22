library(matrixStats)
library(igraph)


# Part A: Create Undirected Random Network

useBarabasi = TRUE

p = 0.01
numNodes = 1000

if (useBarabasi)
{
  g_1a <- barabasi.game(numNodes, directed = FALSE)
} else
{
  g_1a <- sample_gnp(numNodes, p)
  while (!is.connected(g_1a))
  {
    g_1a <- sample_gnp(numNodes, p)
    print("Not Connected, Regenerating")
  }
  
}

#plot(g_1a, vertex.label = NA)
diameter(g_1a)

# Part B: Random Walk on Random Network

# set parameters
minSteps = 0
maxSteps = 15#numNodes/10
stepInt = 1 #numNodes/100
numIter = numNodes
stepVec = seq(minSteps, maxSteps, stepInt)

pathLenVec <- matrix(, nrow = maxSteps, ncol = numIter)
# pathLenVec = c()
pathMeans = c()
pathSds = c()


  for (j in 1:numIter) {
    startNode <- sample(1:numNodes, 1)
    # iterate through different numbers of steps between set values with interval stepInt
    rw <- random_walk(g_1a, startNode, maxSteps, mode = "out")
    # perform random walk and store vertices in rw
    
    for (t in 1:maxSteps) {
      pathLenVec[t,j] <- distances(g_1a, v=rw[1], to=rw[t], mode = "out", weights = NULL)
      #       
      # get distance between starting point and ending point
    }
    print(j)
  }
#pathMeans <- pathMeans/(numIter*numNodes)
pathMeans <- rowMeans(pathLenVec)
pathSds <- rowSds(pathLenVec)

steps <- 1:maxSteps - 1
plot(steps, pathMeans, main="Mean Path Length vs. Step Length", xlab="Number of Steps", ylab="Mean Path Length")
points(steps, sqrt(steps), col=2, pch=2)
#plot(1:maxSteps, pathSds, main="Path Length Standard Deviation vs. Step Length", xlab="Number of Steps", ylab="Path Length Standard Deviation")

