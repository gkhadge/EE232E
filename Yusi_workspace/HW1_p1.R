library(matrixStats)
library(igraph)

p = 0.01
numNodes = 1000
minSteps = 0
maxSteps = numNodes/2
stepInt = 10
numIter = 100

stepVec = seq(minSteps, maxSteps, stepInt)

g_1 <- sample_gnp(numNodes, p)

plot(g_1)
diameter(g_1)

pathLenVec = matrix(, nrow = length(stepVec), ncol = numIter)
  
for(i in 1:length(stepVec)) {
    for (j in 1:numIter) {
    # iterate through different numbers of steps between set values with interval stepInt
    startNode <- sample(1:numNodes, 1, replace=TRUE)  # select a random start node
    rw <- random_walk(g_1, startNode, i, mode = "out")
    # perform random walk and store vertices in rw
    pathLenVec[i,j] <- distances(g_1, v=rw[1], to=rw[length(rw)], mode = "out", weights = NULL)
    # get distance between starting point and ending point
    }
}

pathMeans <- rowMeans(pathLenVec)
pathSts <- rowSds(pathLenVec)

plot(stepVec, pathMeans, main="Mean Path Length vs. Step Length", xlab="Number of Steps", ylab="Mean Path Length")

plot(stepVec, pathSts, main="Path Length Standard Deviation vs. Step Length", xlab="Number of Steps", ylab="Path Length Standard Deviation")