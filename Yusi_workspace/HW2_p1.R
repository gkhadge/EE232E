library(matrixStats)
library(igraph)


# Part A: Create Undirected Random Network

p = 0.01
numNodes = 1000
g_1a <- sample_gnp(numNodes, p)

plot(g_1a, vertex.label = NA)
diameter(g_1a)

# Part B: Random Walk on Random Network

# set parameters
minSteps = 0
maxSteps = numNodes/2
stepInt = numNodes/100
numIter = 10
stepVec = seq(minSteps, maxSteps, stepInt)

pathLenVec <- matrix(, nrow = numNodes, ncol = numIter)
startNodeVec <- sample(1:numNodes, numNodes, replace=FALSE)

for (i in 1:length(stepVec)) {
  for (k in 1:numNodes) {
    startNode <- startNodeVec[k]  # select a random start node, must iterate through all possible start nodes
    for (j in 1:numIter) {
      # iterate through different numbers of steps between set values with interval stepInt
      rw <- random_walk(g_1a, startNode, i, mode = "out")
      # perform random walk and store vertices in rw
      pathLenVec[k,j] <- distances(g_1a, v=rw[1], to=rw[length(rw)], mode = "out", weights = NULL)
      # get distance between starting point and ending point
    }
  }
  pathMeans[i] <- mean(rowMeans(pathLenVec))
  pathSts[i] <- mean(rowSds(pathLenVec))
}

plot(stepVec, pathMeans, main="Mean Path Length vs. Step Length", xlab="Number of Steps", ylab="Mean Path Length")
plot(stepVec, pathSts, main="Path Length Standard Deviation vs. Step Length", xlab="Number of Steps", ylab="Path Length Standard Deviation")

