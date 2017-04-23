library(matrixStats)
library(igraph)

#problem 3 (a)
p = 0.01
numNodes = 1000

g_3a <- sample_gnp(numNodes, p)
while (!is.connected(g_3a))
{
  g_3a <- sample_gnp(numNodes, p)
  print("Not Connected, Regenerating")
}

# set parameters
minSteps = 0
maxSteps = 15#numNodes/10
stepInt = 1 #numNodes/100
numIter = numNodes

walkerVisit <- c(1:numNodes)*0

for (j in 1:numIter) {
  
  startNode <- sample(1:numNodes, 1)
  # iterate through different numbers of steps between set values with interval stepInt
  rw <- random_walk(g_3a, startNode, maxSteps, mode = "out")
  # perform random walk and store vertices in rw
  
  for (t in 1:maxSteps) {
    #get number the walker visits each node
    walkerVisit[rw[t]] <- walkerVisit[rw[t]]+1
  }
  print(j)
}

totalVisit <- sum(walkerVisit)  #Calculate total number of visits of the walker
walkerVisitProb <- walkerVisit/totalVisit   #Calculate the probability that the walker visits each node
nodesDegree <- degree(g_3a)    #Calculate degree of each node

cor(walkerVisitProb, nodesDegree)
plot(walkerVisitProb, nodesDegree, main="3(a) Probability of visit vs. Degree of each node", xlab="Probability of visit", ylab="Degree of node")


#Problem 3 (b)
g_3b <- sample_gnp(numNodes, p, directed = TRUE)
while (!is.connected(g_3b))
{
  g_3b <- sample_gnp(numNodes, p, directed = TRUE)
  print("Not Connected, Regenerating")
}


walkerVisit_3b <- c(1:numNodes)*0

for (j in 1:numIter) {
  
  startNode <- sample(1:numNodes, 1)
  # iterate through different numbers of steps between set values with interval stepInt
  rw_3b <- random_walk(g_3b, startNode, maxSteps, mode = "out")
  # perform random walk and store vertices in rw
  
  for (t in 1:maxSteps) {
    #get number the walker visits each node
    walkerVisit_3b[rw_3b[t]] <- walkerVisit_3b[rw_3b[t]]+1
  }
  print(j)
}

totalVisit_3b <- sum(walkerVisit_3b)  #Calculate total number of visits of the walker
walkerVisitProb_3b <- walkerVisit_3b/totalVisit_3b   #Calculate the probability that the walker visits each node
nodesDegree_3b <- degree(g_3b)    #Calculate degree of each node

cor(walkerVisitProb_3b, nodesDegree_3b)
plot(walkerVisitProb_3b, nodesDegree_3b, main="3(b) Probability of visit vs. Degree of each node", xlab="Probability of visit", ylab="Degree of node")


#problem 3 (c)

g_3c <- sample_gnp(numNodes, p)
while (!is.connected(g_3c))
{
  g_3c <- sample_gnp(numNodes, p)
  print("Not Connected, Regenerating")
}
#plot(g_3c)

walkerVisit <- c(1:numNodes)*0
startNode <- sample(1:numNodes,1)

for (j in 1:numIter){
  rw <- c()
  continue = TRUE
  for (t in 1:maxSteps){
    if(continue == TRUE){
      rw[t]<-startNode
      walkerVisit[rw[t]] <- walkerVisit[rw[t]]+1
      #walkerVisit[startNode] <- walkerVisit[startNode]+1
      neighborNodes <- neighbors(g_3c,startNode)
      if(length(neighborNodes)==2){
        nextNode <- neighborNodes[1]
      }else {
        nextNode <- sample(neighborNodes,1)
        while(nextNode == prevNode){
          nextNode <- sample(neighborNodes,1)
        }
      }
      
      prevNode <- startNode
      startNode <- nextNode
      smp <- sample(1:100,1)
      if(smp>85){
        continue = FALSE; 
      }
    }
  }
  print(j)
}

totalVisit <- sum(walkerVisit)  #Calculate total number of visits of the walker
walkerVisitProb <- walkerVisit/totalVisit   #Calculate the probability that the walker visits each node
nodesDegree <- degree(g_3c)    #Calculate degree of each node

cor(walkerVisitProb, nodesDegree)
plot(walkerVisitProb, nodesDegree, main="3(c) Probability of visit vs. Degree of each node", xlab="Probability of visit", ylab="Degree of node")


