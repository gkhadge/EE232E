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


circles <- list()
for (i in 1:length(circles_filenames)){
#why it only takes in [[i]] not i? 
  circles[[i]]<- strsplit(readLines(circles_filenames[i]), "\t")
}

#Create personal network for users who have more than 2 circles
for (i in 1:length(nodes_id)){ #length(nodes_id)
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
    
    g_personal <- as.directed(g_personal, "mutual")
    
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
    
    
    
    #show how communities overlap with the users' circles 
    colors_eb <- c(rainbow(8))
    colors_eb[3] = "#FFFF00"    # set third color to yellow for better differentiation
    
    barplot(sizes(g_personal_wt), col=colors_eb, main=c("Community structure of Walktrap i=7"), xlab="Community Number", ylab="Community Size")
    #plot without core node 
    
    barplot(sizes(g_personal_if), col=colors_eb, main=c("Community structure of Infomap, i = 7"), xlab="Community Number", ylab="Community Size")
    #plot without core node 
    
    
    wt_len <- length(V(g_personal))-1
    
    plot(g,vertex.color=colors_eb[membership(g_personal_wt)[1:wt_len]], vertex.label = NA,
         layout=layout.fruchterman.reingold,main=c("Community Structure of Walktrap, i = 12"),  vertex.size=5,
         vertex.label.cex=0.5, edge.arrow.size=0.2)
    
    if_len <- length(V(g_personal))-1
    
    plot(g,vertex.color=colors_eb[membership(g_personal_if)[1:if_len]], vertex.label = NA,
         layout=layout.fruchterman.reingold,main=c("Community Structure of Infomap, i = 12"),  vertex.size=5,
         vertex.label.cex=0.5, edge.arrow.size=0.2)
    
    g_sub_1 <- induced.subgraph(g_personal, which(membership(g_personal_if)==1))
    g_sub_2 <- induced.subgraph(g_personal, which(membership(g_personal_if)==2))
    g_sub_3 <- induced.subgraph(g_personal, which(membership(g_personal_if)==3))
    g_sub_4 <- induced.subgraph(g_personal, which(membership(g_personal_if)==4))
    
    
    #g_sub_1 <- induced.subgraph(g_personal, which(membership(g_personal_wt)==1))
    #g_sub_2 <- induced.subgraph(g_personal, which(membership(g_personal_wt)==2))
    #g_sub_3 <- induced.subgraph(g_personal, which(membership(g_personal_wt)==3))
    #g_sub_4 <- induced.subgraph(g_personal, which(membership(g_personal_wt)==4))
    
    
    circle_membership_1_3 <- c()
    
    countfortrue <- 0
    countforfalse <- 0
    count <- 0 
    for (t in 1:length(V(g_sub_1))){
      mem <- 0 
      for (j in 1:length(circles[[12]])){
        for(k in 1:length(circles[[12]][[j]])){
          if(V(g_sub_1)[t]$name == circles[[12]][[j]][[k]]){
            print("t")
            print(t)
            print("true")
            count <- count + 1
            mem <- j
          }
        }
      }
      
      if(count > 0){
        countfortrue <- countfortrue + 1
      }else 
        countfortrue <- countforfalse + 1
      
      circle_membership_1_3[t] <- mem
      count <- 0 
    }
    
    colors_sub <- c(rainbow(8))
    colors_sub[1] = "#FFFF00"    # set first color to yellow for better differentiation
    colors_sub[2] = "#FF0000FF"
    colors_sub[3] = "#FFBF00FF"
    
    plot(g_sub_1,vertex.color=colors_sub[circle_membership_1_3 + 1], vertex.label = NA,
         layout=layout.fruchterman.reingold,main=c("Overlap of Infomap Community 1 & circle 1, i = 12 "),  vertex.size=5,
         vertex.label.cex=0.5, edge.arrow.size=0.2)
    
    
  }
}


