# Project 2 
# Problem 1

rm(list=ls())
library(igraph)

# Read actor data
actor_data <- list()
actor_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/actor_movies.txt", "r")
actor_data <- readLines(actor_con)
close(actor_con)

# Read actress data
actress_data <- list()
actress_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/actress_movies.txt", "r")
actress_data <- readLines(actress_con)
close(actress_con)

# Combine actor and actress data
combined_data <- list()
combined_data <- c(actor_data, actress_data)
# split movies by separator = "\t\t"
combined_split_data <- strsplit(combined_data,"\t\t", perl = TRUE)

# Remove all actor/actress with less than 5 movies
filtered_data <- list()
for (i in 1:length(combined_split_data)){
  if(length(combined_split_data[[i]])>5){
    filtered_data <- c(filtered_data,combined_split_data[i])
  }
}


setwd("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2 data/")
load("filtered_actor_data.RData")

# Problem 2 
# Loop through all actor/actress
net_file <- c()
n_actors <- length(filtered_data)

for (a in 1: n_actors){ #n_actors){
  print(a)
  # For all the movies each actor/actress has acted
  for (b in 1: n_actors){
    if(a != b){
       n_a <- filtered_data[[a]][2:length(filtered_data[[a]])]
       n_intersect <- intersect(n_a,filtered_data[[b]][2:length(filtered_data[[b]])] )
       len_n_inter <- length(n_intersect)
       if(len_n_inter>0){
         E_weight <- len_n_inter/length(n_a)
         net_file <- c(net_file, a, b, E_weight)
       }
    }
  }
}

net_mat <- matrix(net_file, ncol=3, byrow = TRUE)
net_con<-file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2 data//p2_network.txt")
write.table(net_mat, net_con, col.names = FALSE, row.names = FALSE, append = FALSE, quote = FALSE, sep = "\t\t", eol = "\n", na = "NA")

g <- read_graph(net_con,format="ncol",directed=TRUE)

# Problem 3 
page_rank(g, directed = TRUE, weights = NULL)

# Problem 4

# Read movie genre data
genre_data <- list()
genre_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/movie_genre.txt", "r")
genre_data <- readLines(genre_con)
close(genre_con)

genre_split <- strsplit(genre_data,"\t\t")



# Read movie rating data
rating_data <- list()
rating_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/movie_rating.txt", "r")
rating_data <- readLines(rating_con)
close(rating_con)




