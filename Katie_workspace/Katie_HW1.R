##Download and install the package 
#install.packages("igraph")

##Load package
library(igraph)

nodesNum = 1000

#2 
#Create a network with a fat-tailed degree distribution
#2-a

degreePro = -3

g_2a = barabasi.game(nodesNum, degreePro, directed = FALSE)
deg_dist_2a <- degree(g_2a)
hist(deg_dist_2a, main="Degree Distribution (2a)", xlab="Degree Number", ylab="Probability")
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
deg_dist_2c <- degree(g_2c)
hist(deg_dist_2c, main="Degree Distribution (2c)", xlab="Degree Number", ylab="Probability")
dia_2c <- diameter(g_2c, unconnected = TRUE)

#GCC calculation

cl_2c <- clusters(g_2c)
gccIndex_2c <- which.max(cl_2c$size)
nonGccNodes_2c <- (1:vcount(g))[cl_2c$membership != gccIndex_2c]
gcc_2c <- delete.vertices(g_2c, nonGccNodes_2c)

#community structure
community_str_2c <- fastgreedy.community(gcc_2c, merges=TRUE, modularity=TRUE,membership=TRUE, weights=NULL)
modularity(community_str_2c)
sizes(community_str_2c)
barplot(sizes(community_str_2c),  main="Community Sizes (2c)", xlab="Community Number", ylab="Community Size")

#2-d
smpl <- igraph.sample(1, 1000, 1000)

