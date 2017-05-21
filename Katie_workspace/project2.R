#Project 2 
#Problem 1

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
combined_split_data <- strsplit(combined_data,"\t\t")

# Remove all actor/actress with less than 5 movies
filtered_data <- list()
for (i in 1:length(combined_split_data)){
  if(length(combined_split_data[[i]])>5){
    filtered_data <- c(filtered_data,combined_split_data[i])
  }
}
