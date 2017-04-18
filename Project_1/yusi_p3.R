# opt-cmd-R to run all code
# shft-cmd-C to comment

rm(list = ls())

library(igraph)

# create barabasi graph
nodesNum = 1000
g <- barabasi.game(nodesNum , directed = FALSE)

plot(g, vertex.size=8, vertex.label=NA)

dg_dist = degree_distribution(g)
barplot(dg_dist)

# find communities
fg <- fastgreedy.community(g)
membership(fg)
cmsize <- sizes(fg)

modularity(fg, membership(fg))
