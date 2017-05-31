# Project 2 
# Problem 6

rm(list=ls())
library(igraph)

load("/Users/Yusi/Downloads/movie_fg.RData")
load("/Users/Yusi/Documents/EE232E/Yusi_workspace/movie_actor_graphs.RData")
#movie_list = "/Users/Yusi/Documents/EE232E/Project2/MovieNetwork.txt"
#actor_list = "/Users/Yusi/Documents/EE232E/Project2/ActorNetwork.txt"

#g_movie <- read_graph(movie_list, format =  "ncol", directed = FALSE)
#g_actor <- read_graph(actor_list, format =  "ncol", directed = TRUE)

BvS <- "BatmanvSuperman:DawnofJustice(2016)"
MI <- "Mission:Impossible-RogueNation(2015)"
M <- "Minions(2015)"
BvS_neigh <- neighbors(g_movie, v = BvS)
MI_neigh <- neighbors(g_movie, v = MI)
M_neigh <- neighbors(g_movie, v = M)


#### BvS Top 5 Neighbors #####

BvS_edgelist <- c()

for (i in 1:length(BvS_neigh)){
  BvS_edgelist <- c(BvS_edgelist, BvS, BvS_neigh[i]$name)
}

BvS_weights <- E(g_movie, BvS_edgelist)$weight
#tail(sort(BvS_weights), 5)

BvS_ind <- c()
BvS_weights_temp <- BvS_weights

for (i in 1:5){
  BvS_ind <- c(BvS_ind, which.max(BvS_weights_temp))
  BvS_weights_temp[which.max(BvS_weights_temp)]=0
}

BvS_top5 <- BvS_neigh[BvS_ind]

BvS_comm_top5 <- c(membership(g_movie_fg)[BvS])

for (i in 1:length(BvS_top5)){
  BvS_comm_top5 <- c(BvS_comm_top5, membership(g_movie_fg)[BvS_top5[i]])
}

#### MI Top 5 Neighbors #####

MI_edgelist <- c()

for (i in 1:length(MI_neigh)){
  MI_edgelist <- c(MI_edgelist, MI, MI_neigh[i]$name)
}

MI_weights <- E(g_movie, MI_edgelist)$weight
#tail(sort(BvS_weights), 5)

MI_ind <- c()
MI_weights_temp <- MI_weights

for (i in 1:5){
  MI_ind <- c(MI_ind, which.max(MI_weights_temp))
  MI_weights_temp[which.max(MI_weights_temp)]=0
}

MI_top5 <- MI_neigh[MI_ind]

MI_comm_top5 <- c(membership(g_movie_fg)[MI])

for (i in 1:length(MI_top5)){
  MI_comm_top5 <- c(MI_comm_top5, membership(g_movie_fg)[MI_top5[i]])
}

#### M Top 5 Neighbors #####
M_edgelist <- c()

for (i in 1:length(M_neigh)){
  M_edgelist <- c(M_edgelist, M, M_neigh[i]$name)
}

M_weights <- E(g_movie, M_edgelist)$weight
#tail(sort(BvS_weights), 5)

M_ind <- c()
M_weights_temp <- M_weights

for (i in 1:5){
  M_ind <- c(M_ind, which.max(M_weights_temp))
  M_weights_temp[which.max(M_weights_temp)]=0
}

M_top5 <- M_neigh[M_ind]

M_comm_top5 <- c(membership(g_movie_fg)[M])

for (i in 1:length(MI_top5)){
  M_comm_top5 <- c(M_comm_top5, membership(g_movie_fg)[M_top5[i]])
}

BvS_comm_top5
MI_comm_top5
M_comm_top5

# Read movie rating data
rating_data <- list()
rating_con <- file("/Users/Yusi/Documents/EE232E/Project2/project_2_data/movie_rating.txt", "r")
rating_data <- readLines(rating_con)
close(rating_con)


