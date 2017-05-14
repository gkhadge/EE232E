#Project 1 
#Problem 7

rm(list=ls())
library(igraph)

#setwd('/Users/Yusi/Documents/EE232E/HW_3')
setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project1/')
edges_filenames <- list.files("gplus", pattern="*.edges", full.names = TRUE)
circles_filenames <- list.files("gplus", pattern="*.circles", full.names = TRUE)
nodes_id <- sub("^([^.]*).*", "\\1", list.files("gplus", pattern="*.edges", full.names = FALSE))
#^: beginning of the string
#http://www.endmemo.com/program/R/sub.php



circles <- list()
for (i in 1:length(circles_filenames)){
#why it only takes in [[i]] not i? 
  circles[[i]]<- strsplit(readLines(circles_filenames[i]), "\t")
}

#Create personal network for users who have more than 2 circles
for (i in 1:10){ #length(nodes_id)
  if(length(circles[[i]])>2){

    g <- read_graph(edges_filenames[[i]],format="ncol",directed=TRUE)
   # print(g)
    
    #add core node to the graph 
    g_personal <- g + vertex(nodes_id[[i]])
    
    #add edges from core node to all other nodes
    for(l in 1:(length(V(g_personal))-1)){
      g_personal <- add_edges(g_personal, c(V(g_personal)[length(V(g_personal))], V(g_personal)[l]))
    }
    
    #Extract the community structure using Walktrap 
    #http://igraph.org/r/doc/cluster_walktrap.html
    g_personal_wt <- cluster_walktrap(g_personal)
    #, weights = E(graph)$weight, steps = 4, merges =  TRUE, modularity = FALSE, labels = TRUE)
    
    #Extract the community structure using Infomap , membership(g_personal_if), communities(g_personal_if)
    #http://igraph.org/r/doc/cluster_infomap.html
    g_personal_if <- cluster_infomap(g_personal)
    
    #show how communities overlap with the users' circles 
    #how do these overlap vary across users?
    #how does this relate to a user's habit on tagging relatioships with circles? 
    
    
  }
}