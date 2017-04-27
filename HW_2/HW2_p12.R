library(matrixStats)
library(igraph)

# Part A: Create Undirected Random Network and Barabasi Network

useBarabasi = TRUE

p = 0.01
numNodes = 10000

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

diameter(g_1a)

# Part B: Random Walk on Random Network and Barabasi Network

# Set parameters for random walk

minSteps = 0
maxSteps = 40000      # maximum number of steps for random walker to take
stepInt = 1000        # step interval 
numIter = numNodes
stepVec = seq(minSteps, maxSteps, stepInt)
stepVec[1] = 1

pathLenVec <- matrix(, nrow = maxSteps/stepInt+1, ncol = numIter)
degreeVec <- c()

pathMeans = c()
pathSds = c()

for (j in 1:numIter) {
  startNode <- sample(1:numNodes, 1)
  # iterate through different numbers of steps between set values with interval stepInt
  rw <- random_walk(g_1a, startNode, maxSteps, mode = "out")
  rw_d <- random_walk(g_1a, startNode, numNodes, mode = "out")
  
  # perform random walk and store vertices in rw
  for (t in 1:(maxSteps/stepInt+1)) {
    rw_current = stepVec[t]
    pathLenVec[t,j] <- distances(g_1a, v=rw[1], to=rw[rw_current], mode = "out", weights = NULL)
    # get distance between starting point and ending point
    # find degree of ending node
  }
  degreeVec[j] <- degree(g_1a, v=rw_d[numNodes], mode = "out")
  # calculate degree of last node in random walk
  print(j)
}

# Average across iterations to get mean path length and standard deviation
pathMeans <- rowMeans(pathLenVec)
pathSds <- rowSds(pathLenVec)

# Plot mean path length results
plot(stepVec, pathMeans, main="Mean Path Length vs. Step Length", xlab="Number of Steps", ylab="Mean Path Length")
points(seq(1,100,10), sqrt(seq(1,100,10)), col=2, pch=2)

# Plot path length standard deviation results
plot(stepVec, pathSds, main="Path Length Standard Deviation vs. Step Length", xlab="Number of Steps", ylab="Standard Deviation")

# Part C: Explained in report
# Part D: Re-run by changing numNodes to 100 and 10000

# Part E: Obtain degree distribution results

degreeHist_rw <- hist(degreeVec, plot = "FALSE")$density
x_plot_rw <- hist(degreeVec, plot = "FALSE")$mids       
# Use histogram density and matching degree values for random walk
degreeHist <- degree_distribution(g_1a)
x_plot <- 1:length(degreeHist)

# Plot degree distribution results
# Use log-xy axes if Barabasi, linear axes if random
# Plot the degree distribtution for graph as black circles, random walk as red triangles

if (useBarabasi)
{
  plot(x_plot, degreeHist, log = "xy", main="Degree Distribution For Barabasi Network", xlab="Degree", ylab="Probability")
  points(x_plot_rw, degreeHist_rw, col=2, pch=2)
  # Plot Barabasi results on log-xy axes to see linearize degree distribution for easier characterization
} else
{
  plot(x_plot, degreeHist, main="Degree Distribution For Random Network", xlab="Degree", ylab="Probability")
  points(x_plot_rw, degreeHist_rw, col=2, pch=2)
}

graph_dgMean <- mean(degree(g_1a))
rw_dgMean <- mean(degreeVec)

graph_dgMed <- median(degree(g_1a))
rw_dgMed <- median(degreeVec)