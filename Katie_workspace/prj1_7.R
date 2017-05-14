#Project 1 
#Problem 7

rm(list=ls())
library(igraph)

#setwd('/Users/Yusi/Documents/EE232E/HW_3')
setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project1/')
edges_filenames <- list.files("gplus", pattern="*.edges", full.names = TRUE)
circles_filenames <- list.files("gplus", pattern="*.circles", full.names = TRUE)
node_list <- list.files("gplus", pattern="*.edges", full.names = FALSE)
nodes_id <- sub(".edges", "\\1", node_list)
#http://www.endmemo.com/program/R/sub.php

circles <- list()
for (i in 1:length(circles_filenames)){
#why it only takes in [[i]] not i? 
  circles[[i]]<- strsplit(readLines(circles_filenames[i]), "\t")
}

#Create personal network for users who have more than 2 circles
for (i in 1:10){ #length(nodes_id)
  if(length(circles[[i]])>2){
    print("i")
    print(i)
    g <- read_graph(edges_filenames[[i]], format="ncol", directed=TRUE)
    #print(g)
    
    #add core node to the graph 
    g_personal <- g + vertex(nodes_id[[i]])
    
    #add edges from core node to all other nodes
    for(l in 1:(length(V(g_personal))-1)){
#CHECK: should this be directed?
      g_personal <- add_edges(g_personal, c(V(g_personal)[length(V(g_personal))], V(g_personal)[l]))
    }
    
    #Extract the community structure using Walktrap 
    #http://igraph.org/r/doc/cluster_walktrap.html
    g_personal_wt <- cluster_walktrap(g_personal)
    #, weights = E(graph)$weight, steps = 4, merges =  TRUE, modularity = FALSE, labels = TRUE)
    
    #Extract the community structure using Infomap , membership(g_personal_if), communities(g_personal_if)
    #http://igraph.org/r/doc/cluster_infomap.html
    g_personal_if <- cluster_infomap(g_personal)
    

    #how do these overlap vary across users?
    #how does this relate to a user's habit on tagging relatioships with circles? 
    print("i")
    print(i)
    
  }
}







#show how communities overlap with the users' circles 
n_large_comm = 8
greys_eb <- replicate(length(g_personal_wt)-n_large_comm, "#D3D3D3")   # make everything other than largest 8 communities grey


colors_eb <- c(rainbow(8))
colors_eb[3] = "#FFFF00"    # set third color to yellow for better differentiation

barplot(sizes(g_personal_wt), col=colors_eb, main=c("Community Structure of Core Personal Network (Edge Betweenness) of Node", i), xlab="Community Number", ylab="Community Size")
plot(g,vertex.color=colors_eb[membership(g_personal_wt)], vertex.label = NA,
     layout=layout.fruchterman.reingold,main=c("Community Structure of Core Neighbor Network (Edge Betweeness) of Node", i))





circle_membership <- c()

countfortrue <- 0
countforfalse <- 0
count <- 0 
for (t in 1:length(V(g))){
  mem <- 0 
  for (i in 1:length(circles[[7]])){
    for(k in 1:length(circles[[7]][[i]])){
      if(V(g)[t]$name == circles[[7]][[i]][[k]]){
        print("t")
        print(t)
        print("true")
        count <- count + 1
        mem <- i
      }
    }
  }
  
  if(count > 0){
    countfortrue <- countfortrue + 1
  }else 
    countfortrue <- countforfalse + 1
  
  circle_membership[t] <- mem
  count <- 0 
}

plot(g,vertex.color=colors_eb[circle_membership], vertex.label = NA,layout=layout.fruchterman.reingold,main=c("Color by circle", i))


for(t in 1:length(circle_membership2)){
  if(circle_membership[t]==0){
    print("true")
    circle_membership[t]<- 5
  }
}


