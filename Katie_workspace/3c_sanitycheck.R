library(matrixStats)
library(igraph)

#sanity check for 3c
#problem 3 (c)
p = 0.01
numNodes = 1000

g_3c <- sample_gnp(numNodes, p)
while (!is.connected(g_3c))
{
  g_3c <- sample_gnp(numNodes, p)
  print("Not Connected, Regenerating")
}
plot(g_3c)

# set parameters
maxSteps = 15#numNodes/10
stepInt = 1 #numNodes/100
numIter = numNodes


walkerVisit <- c(1:numNodes)*0
startNode <- sample(1:numNodes,1)

for (j in 1:numIter){
  rw <- c()
  #continue = TRUE
  for (t in 1:maxSteps){
    #if(continue == TRUE){
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
      #smp <- sample(1:100,1)
      #if(smp>85){
        #continue = FALSE; 
      #}
    #}
  }
  print(j)
}

totalVisit <- sum(walkerVisit)  #Calculate total number of visits of the walker
walkerVisitProb <- walkerVisit/totalVisit   #Calculate the probability that the walker visits each node
nodesDegree <- degree(g_3c)    #Calculate degree of each node

cor(walkerVisitProb, nodesDegree)
plot(walkerVisitProb, nodesDegree, main="3(c) Probability of visit vs. Degree of each node", xlab="Probability of visit", ylab="Degree of node")

