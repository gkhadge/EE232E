# Project 2 
# Problem 6

rm(list=ls())
library(igraph)

load("/Users/Yusi/Downloads/movie_fg.RData")
#load("/Users/Yusi/Documents/EE232E/Yusi_workspace/movie_actor_graphs.RData")
#movie_list = "/Users/Yusi/Documents/EE232E/Project2/MovieNetwork.txt"
#actor_list = "/Users/Yusi/Documents/EE232E/Project2/ActorNetwork.txt"
load("/Users/Yusi/Documents/EE232E/Yusi_workspace/workspace_0530.RData")
load("/Users/Yusi/Documents/EE232E/Yusi_workspace/rating_data_list.RData")
#g_movie <- read_graph(movie_list, format =  "ncol", directed = FALSE)
#g_actor <- read_graph(actor_list, format =  "ncol", directed = TRUE)

BvS <- "BatmanvSuperman:DawnofJustice(2016)"
MI <- "Mission:Impossible-RogueNation(2015)"
M <- "Minions(2015)"
BvS_neigh <- neighbors(g_movie, v = BvS)
MI_neigh <- neighbors(g_movie, v = MI)
M_neigh <- neighbors(g_movie, v = M)

##### Problem 7: Ratings

# Read movie rating data
rating_data <- list()
rating_con <- file("/Users/Yusi/Documents/EE232E/Project2/project_2_data/movie_rating.txt", "r")
rating_data_raw <- readLines(rating_con)
close(rating_con)

rating_data <- strsplit(rating_data_raw,"\t\t", perl = TRUE)
rating_data_names <- c()
rating_data_vals <- c()

for (i in 1:length(rating_data)){
  rating_data_names[i] <- gsub(" ", "", rating_data[[i]][1], fixed = TRUE)
  rating_data_vals[i] <- rating_data[[i]][2]
}
rating_data_vals <- as.numeric(rating_data_vals)
rating_data_list <- setNames(object = rating_data_vals, rating_data_names)
save(rating_data_list,file="/Users/Yusi/Documents/EE232E/Project2/rating_data_list.RData")

#### BvS Top Neighbors #####
n_neigh <- 20

BvS_edgelist <- c()

for (i in 1:length(BvS_neigh)){
  BvS_edgelist <- c(BvS_edgelist, BvS, BvS_neigh[i]$name)
}

BvS_weights <- E(g_movie, BvS_edgelist)$weight
#tail(sort(BvS_weights), 5)

BvS_ind <- c()
BvS_weights_temp <- BvS_weights

for (i in 1:n_neigh){
  BvS_ind <- c(BvS_ind, which.max(BvS_weights_temp))
  BvS_weights_temp[which.max(BvS_weights_temp)]=0
}

BvS_top <- BvS_neigh[BvS_ind]

#BvS_comm_top <- c(membership(g_movie_fg)[BvS])
BvS_comm_top <- c()

for (i in 1:length(BvS_top)){
  BvS_comm_top <- c(BvS_comm_top, membership(g_movie_fg)[BvS_top[i]])
}

#### MI Top Neighbors #####

MI_edgelist <- c()

for (i in 1:length(MI_neigh)){
  MI_edgelist <- c(MI_edgelist, MI, MI_neigh[i]$name)
}

MI_weights <- E(g_movie, MI_edgelist)$weight
#tail(sort(BvS_weights), 5)

MI_ind <- c()
MI_weights_temp <- MI_weights

for (i in 1:n_neigh){
  MI_ind <- c(MI_ind, which.max(MI_weights_temp))
  MI_weights_temp[which.max(MI_weights_temp)]=0
}

MI_top <- MI_neigh[MI_ind]

#MI_comm_top <- c(membership(g_movie_fg)[MI])
MI_comm_top <- c()

for (i in 1:length(MI_top)){
  MI_comm_top <- c(MI_comm_top, membership(g_movie_fg)[MI_top[i]])
}

#### M Top Neighbors #####
M_edgelist <- c()

for (i in 1:length(M_neigh)){
  M_edgelist <- c(M_edgelist, M, M_neigh[i]$name)
}

M_weights <- E(g_movie, M_edgelist)$weight
#tail(sort(BvS_weights), 5)

M_ind <- c()
M_weights_temp <- M_weights

for (i in 1:n_neigh){
  M_ind <- c(M_ind, which.max(M_weights_temp))
  M_weights_temp[which.max(M_weights_temp)]=0
}

M_top <- M_neigh[M_ind]

#M_comm_top <- c(membership(g_movie_fg)[M])
M_comm_top <- c()

for (i in 1:length(MI_top)){
  M_comm_top <- c(M_comm_top, membership(g_movie_fg)[M_top[i]])
}

BvS_comm_top
MI_comm_top
M_comm_top


######## Problem 7: Ratings


BvS_top_rating <- c()
for (i in 1:length(BvS_ind)){
  BvS_top_rating <- c(BvS_top_rating, rating_data_list[BvS_top[i]$name])
}

MI_top_rating <- c()
for (i in 1:length(M_ind)){
  MI_top_rating <- c(MI_top_rating, rating_data_list[MI_top[i]$name])
}

M_top_rating <- c()
for (i in 1:length(M_ind)){
  M_top_rating <- c(M_top_rating, rating_data_list[M_top[i]$name])
}

BvS_top
BvS_top_rating

MI_top
MI_top_rating

M_top
M_top_rating

# save(BvS_top_rating,file="/Users/Yusi/Documents/EE232E/Project2/BvS_top_rating.RData")
# save(MI_top_rating,file="/Users/Yusi/Documents/EE232E/Project2/MI_top_rating.RData")
# save(M_top_rating,file="/Users/Yusi/Documents/EE232E/Project2/M_top_rating.RData")

# Problem 7


# BvS Rating: 6.7
# MI Rating: 7.4
# Minions: 6.4

# Unweighted Means

BvS_mean <- mean(BvS_top_rating, na.rm=TRUE)
MI_mean <- mean(MI_top_rating, na.rm=TRUE)
M_mean <- mean(M_top_rating, na.rm=TRUE)

# Weighted Means

BvS_weights_n <- BvS_weights[BvS_ind]/sum(BvS_weights[BvS_ind])
MI_weights_n <- MI_weights[MI_ind]/sum(MI_weights[MI_ind])
M_weights_n <- M_weights[M_ind]/sum(M_weights[M_ind])

BvS_mean_w <- weighted.mean(BvS_top_rating, BvS_weights_n, na.rm = TRUE)
MI_mean_w <- weighted.mean(MI_top_rating, MI_weights_n, na.rm = TRUE)
M_mean_w <- weighted.mean(M_top_rating, M_weights_n, na.rm = TRUE)

