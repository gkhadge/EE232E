rm(list=ls())

library(igraph)

#g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 

x <- 1000
p_sum <- 0
for (i in 1:x)  
  
{

g1 <- sample_gnp(1000,0.00723)

#deg_dist <- degree_distribution(g1)
#plot(0:(length(deg_dist)-1),deg_dist)
#plot(g1) # A simple plot of the network - we'll talk more about plots later

#h <- diameter(g1,directed=FALSE,unconnected=TRUE)

if (is.connected(g1)) p_sum <- p_sum + 1;
}
p_sum/x