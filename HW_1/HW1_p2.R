##Download and install the package 
#install.packages("igraph")

##Load package
library(igraph)

rm(list=ls())
nodesNum = 1000

#2 
#Create a network with a fat-tailed degree distribution
#2-a

degreePro = -3

g_2a = barabasi.game(nodesNum, degreePro, directed = FALSE)
deg_dist_2a <- degree.distribution(g_2a)
x_plot_2a = 0:(length(deg_dist_2a)-1)
plot(deg_dist_2a, log = "xy", main="Degree Distribution (2a)", xlab="Degree Number", ylab="Probability")
dia_2a <- diameter(g_2a, unconnected = TRUE)


#2-b
is.connected(g_2a)
cl_2a <- clusters(g_2a)
gccIndex_2a <- which.max(cl_2a$size)
nonGccNodes_2b <- (1:vcount(g_2a))[cl_2a$membership != gccIndex_2a]
gcc_2b <- delete.vertices(g_2a, nonGccNodes_2b)

#community structure
community_str_2b <- fastgreedy.community(gcc_2b, merges=TRUE, modularity=TRUE,membership=TRUE, weights=NULL)
modularity(community_str_2b)
sizes(community_str_2b)
barplot(sizes(community_str_2b),  main="Community Sizes (2b)", xlab="Community Number", ylab="Community Size")

#2-c

nodesNum2 = 10000

g_2c = barabasi.game(nodesNum2, degreePro, directed = FALSE)
deg_dist_2c <- degree.distribution(g_2c)
x_plot_2c = 0:(length(deg_dist_2c)-1)
plot(deg_dist_2c, log = "xy", main="Degree Distribution (2c)", xlab="Degree Number", ylab="Probability")
dia_2c <- diameter(g_2c, unconnected = TRUE)
is.connected(g_2c)

#GCC calculation

cl_2c <- clusters(g_2c)
gccIndex_2c <- which.max(cl_2c$size)
nonGccNodes_2c <- (1:vcount(g_2c))[cl_2c$membership != gccIndex_2c]
gcc_2c <- delete.vertices(g_2c, nonGccNodes_2c)

#community structure
community_str_2c <- fastgreedy.community(gcc_2c, merges=TRUE, modularity=TRUE,membership=TRUE, weights=NULL)
modularity(community_str_2c)
sizes(community_str_2c)
barplot(sizes(community_str_2c),  main="Community Sizes (2c)", xlab="Community Number", ylab="Community Size")

#2-d
chosennodes <- 10000
degree_array <- vector()
smpl <- sample(1:nodesNum2, chosennodes, replace=TRUE)
for (i in 1:chosennodes){
  vtx <- V(gcc_2c)[smpl[i]]
  #dg <- degree(gcc_2c, vtx)
  n_vtx <- neighbors(gcc_2c, vtx)
  
  smpl2 <- sample(1:length(n_vtx),1,replace=TRUE)
  neighbor_degree <- degree(gcc_2c, n_vtx[smpl2])
  degree_array <- c(degree_array, neighbor_degree)
}

degree_prob <- hist(degree_array)$density
x_plot = 0:(length(degree_prob)-1)

plot(x_plot, degree_prob,  log = "xy", main = "Neighbor degree plot", xlab = "Degree", ylab = "Probability")


dd1 <- degree_prob
dd2 <- deg_dist_2a
plot(x_plot, dd1, log="y", main="Degree Distribution (2a and 2d)", xlab="Degree",ylab="Probability")
points(x_plot_2a, dd2, col=2, pch=2)


