library(matrixStats)
library(igraph)

# Part A: Create Undirected Random Network

numNodes = 100
degreePow = -3

g_2a <- barabasi.game(numNodes, degreePow, directed = FALSE)

plot(g_2a)
diameter(g_2a)

# Part B: Random Walk on Random Network

# set parameters
minSteps = 0
maxSteps = numNodes/2
stepInt = numNodes/10
numIter = 10
stepVec = seq(minSteps, maxSteps, stepInt)

pathLenVec <- matrix(, nrow = numNodes, ncol = numIter)
pathMeans = c()
pathSds = c()
startNodeVec <- sample(1:numNodes, numNodes, replace=FALSE)

for (i in 1:length(stepVec)) {
  for (k in 1:numNodes) {
    startNode <- startNodeVec[k]  # select a random start node, must iterate through all possible start nodes
    for (j in 1:numIter) {
      # iterate through different numbers of steps between set values with interval stepInt
      rw <- random_walk(g_2a, startNode, i, mode = "out")
      # perform random walk and store vertices in rw
      pathLenVec[k,j] <- distances(g_2a, v=rw[1], to=rw[length(rw)], mode = "out", weights = NULL)
      # get distance between starting point and ending point
    }
  }
  startNodeMeans <- rowMeans(pathLenVec)
  startNodeSds <- rowSds(pathLenVec)
  
  pathMeans[i] <- mean(startNodeMeans)
  pathSds[i] <- mean(startNodeSds)
}

plot(stepVec, pathMeans, main="Mean Path Length vs. Step Length", xlab="Number of Steps", ylab="Mean Path Length")
plot(stepVec, pathSds, main="Path Length Standard Deviation vs. Step Length", xlab="Number of Steps", ylab="Path Length Standard Deviation")

