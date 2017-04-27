library(igraph)

file <- "/Users/Yusi/Documents/EE232E/Yusi_workspace/sorted_directed_net.txt"

g<-read.graph(file, format="ncol", directed = TRUE)
E(g)$weight
is.directed(g)

is.connected(g)
cl <- clusters(g)
gccIndex <- which.max(cl$size)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)

gcc_dg <- degree_distribution(g, mode = "out")
plot()