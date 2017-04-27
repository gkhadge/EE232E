library(igraph)

file <- "/Users/Yusi/Documents/EE232E/Yusi_workspace/sorted_directed_net.txt"

g<-read.graph(file, format="ncol", directed = TRUE)
E(g)$weight
is.directed(g)

is.connected(g)
cl <- clusters(g)
gccIndex <- which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)
length(V(gcc))

gcc_dg_in <- degree(gcc, mode = "out")
gcc_hist_in <- hist(gcc_dg_in, plot = "FALSE")
plot(gcc_hist_in$count, log = "y", type = "h", lwd = 10, lend = 2, main = "Outgoing Degree Distribution", xlab = "Degree", ylab = "Number of Nodes")

gcc_dg_out <- degree(gcc, mode = "in")
gcc_hist_out <- hist(gcc_dg_out, plot = "FALSE")
plot(gcc_hist_out$count, log = "y", type = "h", lwd = 10, lend = 2, main = "Incoming Degree Distribution", , xlab = "Degree", ylab = "Number of Nodes")

gcc_ud <- as.undirected(gcc, mode = "each")
simplify(gcc_ud, remove.multiple = TRUE, remove.loops = TRUE)
