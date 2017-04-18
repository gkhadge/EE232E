# opt-cmd-R to run all code
# shft-cmd-C to comment

rm(list = ls())

library(igraph)

p0 = 0.01
p1 = 0.05
p2 = 0.1

nodes = 1000

g0 = sample_gnp(nodes, p0, directed = FALSE, loops = FALSE)
g1 = sample_gnp(nodes, p1, directed = FALSE, loops = FALSE)
g2 = sample_gnp(nodes, p2, directed = FALSE, loops = FALSE)

plot(g0, vertex.size=2, vertex.label=NA)  
plot(g1, vertex.size=2, vertex.label=NA)
plot(g2, vertex.size=2, vertex.label=NA)

# if average degree of each node is 2, then the graph is likely connected

dg_dist0 = degree_distribution(g0)
a0 = 0:(length(dg_dist0)-1)

dg_dist1 = degree_distribution(g1)
a1 = 0:(length(dg_dist1)-1)

dg_dist2 = degree_distribution(g2)
a2 = 0:(length(dg_dist2)-1)

plot(a0, dg_dist0)
plot(a1, dg_dist1)
plot(a2, dg_dist2)

barplot(degree.distribution(g0))


con0 = is.connected(g0)
con1 = is.connected(g1)
con2 = is.connected(g2)

dia0 = diameter(g0, directed = FALSE, unconnected = TRUE, weights = NULL)
dia1 = diameter(g1, directed = FALSE, unconnected = TRUE, weights = NULL)
dia2 = diameter(g2, directed = FALSE, unconnected = TRUE, weights = NULL)

#####

# directed = connected nodes in specified order
cl <- clusters(g)

gccIndex = which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]

# giant connected component
# barabasi network -- preferential attachment network

g = barabasi.game(nodes, directed = FALSE)
plot(g, vertex.size=3, vertex.label=NA)

fg = fastgreedy.community(g)
cmsize = sizes(fg)
