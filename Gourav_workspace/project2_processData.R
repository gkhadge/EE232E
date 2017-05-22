#Project 2 
#Problem 1
# Extracts data, outputs filtered_actor_data to "filtered_actor_data.txt"

rm(list=ls())
library(igraph)

setwd('C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Project2')
#setwd('/Users/Yusi/Documents/EE232E/Project2')
#setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2')

# Read actor data
actor_data <- list()
actor_con <- file("project_2_data/actor_movies.txt", "r")
actor_data <- readLines(actor_con)
close(actor_con)

# Read actress data
actress_data <- list()
actress_con <- file("project_2_data/actress_movies.txt", "r")
actress_data <- readLines(actress_con)
close(actress_con)

# Combine actor and actress data
combined_data <- list()
combined_data <- c(actor_data, actress_data)
# split movies by separator = "\t\t"
combined_split_data <- strsplit(combined_data,"\t\t")

# Regex for extracting movie title
# Extracts from start until it sees a year such as "(1993)", "(????)" or "(2014/II)"
# Discards all characters after that, assuming that it's not relevant to the title.
movie_title <- regex("^(.+?)\\(([:digit:][:digit:][:digit:][:digit:]|\\?\\?\\?\\?)(.*?)\\)")

# Remove all actor/actress with less than 5 movies  
filtered_actor_data <- list()
for (i in 1:length(combined_split_data)){
  print(i/length(combined_split_data))
  # Filter out actors with less than 5 movies 
  if(length(combined_split_data[[i]])>5){
    # Extract movie title without any flair
    for (j in 2:length(combined_split_data[[i]])) {
      #skip first element since it's the actor's name, not a movie title
      combined_split_data[[i]][j] = str_extract(combined_split_data[[i]][j], movie_title)
    }
    # Add actor to filtered dataset with pruned movie titles
    filtered_actor_data <- c(filtered_actor_data,combined_split_data[i])
  }
}
filename <- "filtered_actor_data.txt"
save(filtered_actor_data,file=filename)