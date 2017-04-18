##Download and install the package 
#install.packages("igraph")

##Load package
library(igraph)

p1 = 0.01
g <- sample_gnp(1000, p1, directed = FALSE, loops = FALSE)
plot(g, vertex.size = 2)
deg_dist <- degree_distribution(g)
plot(0: (length(deg_dist)-1), deg_dist)
is.connected(g)
dia <- diameter(g, unconnected = TRUE)

#2 
#Create a network with a fat-tailed degree distribution
#2-a
g2a = barabasi.game(1000, -3, directed = FALSE)
deg_dist2a <- degree_distribution(g2a)
plot(deg_dist2a)
dia2a <- diameter(g2a, unconnected = TRUE)

#2-b
is.connected(g2a)
cl <- clusters(g2a)
gcc <- which.max(size)

#community structure
community_str <- fastgreedy.community(graph, merges=TRUE, modularity=TRUE,membership=TRUE, weights=NULL)
modularity = modularity(community_str)
size = sizes(community_str)


#2-c
g2c = barabasi.game(10000, -3, directed = FALSE)
deg_dist2c <- degree_distribution(g2c)
plot(deg_dist2c)
dia2c <- diameter(g2c, unconnected = TRUE)

#GCC calculation

cl <- clusters(g2c)
gcc <- which.max(size)

#community structure
community_str <- fastgreedy.community(graph, merges=TRUE, modularity=TRUE,membership=TRUE, weights=NULL)
modularity = modularity(community_str)
size = sizes(community_str)


#2-d
smpl <- igraph.sample(1, 1000, 1000)

