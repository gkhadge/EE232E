rm(list=ls())
library(igraph)

#problem2
setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/')
actor_edgelist <- "ActorNetwork.txt"
# Read in graph from file
g_actor <- read_graph(actor_edgelist,format="ncol",directed=TRUE)

g_actor_pr <- page_rank(g_actor, directed = TRUE, damping = 0.85, personalized = NULL, weights = NULL,
          options = NULL)$vector

## Export to RData file
filename_3 <- "g_actor_pr.RData"
save(g_actor_pr,file=filename_3)


top_10_actors <- c()
top_10_values <- tail(sort(g_actor_pr),10)
for(i in 1:10){
  top_10_actors<- c(top_10_actors, which(g_actor_pr==top_10_values[i]))
}

# Pagerank of well-known actors 
wellknown10actors <- c("Bale,Christian", "Cruise,Tom",  "Pitt,Brad", "Damon,Matt", "Hanks,Tom", "Chaplin,Charles", "Willis,Bruce","Jolie,Angelina","Johansson,Scarlett", "Monroe,Marilyn")
wellknown10actors_pr <- c()
for (i in 1:length(g_actor_pr)){
  for (t in 1:length(wellknown10actors)){
    if(names(g_actor_pr[i]) == wellknown10actors[t]){
      print(names(g_actor_pr[i]))
      wellknown10actors_pr <- c(wellknown10actors_pr , g_actor_pr[i])
    }
  }
}


movie_edgelist <- "MovieNetwork.txt"
# Read in graph from file
g_movie <- read_graph(movie_edgelist,format="ncol",directed=FALSE)

g_movie_fg <- cluster_fast_greedy(g_movie)

# Check: argument & weight fast_greedy not working
m_g_movie <- as.undirected(g_movie,mode = "collapse")


g_movie_simp <- simplify(g_movie, remove.multiple=TRUE)
g_movie_fg <- fastgreedy.community(g_movie_simp)
g_movie_fg_n <- g_movie_fg

## Export to RData file
filename <- "movie_fg.RData"
save(g_movie_fg,file=filename)

load("movie_fg.RData")


#Problem 8 
# Read movie data
movieDict <- list()
movieDict_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/MovieDict.txt", "r")
movieDict <- readLines(movieDict_con)
close(movieDict_con)

# Featuer1: top 5 pageranks
movieDict_data <- movieDict[movieDict != ""]
movieDict_data <- strsplit(movieDict_data,"\t", perl = TRUE)

save(movieDict_data, file ="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/movieDict_data.RData" )

test_data <- movieDict_data
new_data <- list()
feature_1_movie_pgrank <- list()
for (i in 1:length(test_data)){
  print(i)
  new_data[[i]]<-test_data[[i]][1]
  for (a in 2:length(test_data[[i]])){
    new_data[[i]][a] <- g_actor_pr[test_data[[i]][a]]
  }
}

## Export to RData file
filename_8 <- "new_data.RData"
save(new_data,file=filename_8)

filename_8 <- "/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/new_data.RData"
load(filename_8)


top5pageranks <- list()
for (m in 1:length(new_data)){
  print(m)
  top5pageranks[[m]] <- c(new_data[[m]][1], sort(new_data[[m]][2:length(new_data[[m]])], decreasing = TRUE)[1:5])
}



## Export to RData file
filename_8_1 <- "top5pageranks.RData"
save(top5pageranks,file=filename_8_1)
load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/top5pageranks.RData")

top5pgr <- list()
for(t in 1:length(top5pageranks)){
  print(t)
  top5pgr[[top5pageranks[[t]][1]]] <- top5pageranks[[t]][2:6]
}

filename_81 <- "/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top5pgr.RData"
save(top5pgr,file=filename_81)



# Read movie rating data
rating_data <- list()
rating_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/movie_rating.txt", "r")
rating_data <- readLines(rating_con)
close(rating_con)

ratingDict_data <- strsplit(rating_data,"\t", perl = TRUE)
ratingDict <- list()
for (g in 1: length(ratingDict_data)){
  print(g)
  ratingDict[[g]] <- c(gsub(" ","",ratingDict_data[[g]])[1], ratingDict_data[[g]][3])
}

## Export to RData file
filename_8_2 <- "ratingDict.RData"
save(ratingDict,file=filename_8_2)

load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/ratingDict.RData")

# Find top 100 movies from ratingDict
count = 0
top100_movies <- c()
for (n in 1:200){
  rate = (101-n)/10
  print(rate)
  for(r in 1:length(ratingDict)){
    if(rate == as.numeric(ratingDict[[r]][2])){
      count = count +1
      print(count)
      print("r:")
      print(r)
      top100_movies <-c(top100_movies, ratingDict[[r]][1])
    }
    
    if(count == 200){
      break
    }
  }
  if(count == 200){
    break
  }
}



save(top100_movies,file="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top100_movies.RData")

# Read top 100 movies
#top100movies <- list()
#top100movies_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top100movie.txt", "r")
#top100movies <- readLines(top100movies_con)
#close(top100movies_con)

# List of top 100 movies data
#top100_movies <- gsub(" ","",top100movies)

###################################################################################################################
# TRIED TO CREATE TOP 100 DIRECTORS LIST AND DOES NOT MAKE SENSE ##################################################
###################################################################################################################

# Make top100movies_director list 
top100m_dir <- list()
for(m in 1:200){#1:length(top100_movies)){
  print(m)
  top100m_dir[top100_movies[[m]][1]]<- directors_namedList[top100_movies[[m]][1]][1]
}

save(top100m_dir,file="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top100m_dir.RData")
load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top100m_dir.RData")

top100_directors <- list()
for (j in 1:length(top100m_dir)){
  if(!is.null(top100m_dir[[j]])){
    top100_directors[names(top100m_dir[j])] <- top100m_dir[[j]]
  }
}

###################################################################################################################
# TRIED TO CREATE TOP 100 DIRECTORS LIST AND DOES NOT MAKE SENSE ##################################################
###################################################################################################################
# Read top director list
directors_data <- list()
directors_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/DirectorLookUpDict.txt", "r")
directors_data <- readLines(directors_con)
close(directors_con)

directors_data <- strsplit(directors_data,"\t", perl = TRUE)

# Create name list of director dictionary 
directors_namedList <- list()
n.obs <- sapply(directors_data, length)

for (u in 1:length(directors_data)){
  print(u)
  directors_namedList[[directors_data[[u]][1]]]<- directors_data[[u]][2:n.obs[u]]
}

#directors_namedList <- setNames(lapply(directors_data, `[`, 2), sapply(directors_data, `[`, 1))

save(directors_namedList,file="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/directors_namedList.RData")
load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/directors_namedList.RData")


# Read top 100 directors list of IMDb250
top100directors_list <- list()
top100directors_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top100_directorsfromimdb250.txt", "r")
top100directors_list <- readLines(top100directors_con)
close(top100directors_con)

save(top100directors_list, file = '/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/top100directors_list.Rdata')

#top100directors_list <- strsplit(top100directors_list,"\t", perl = TRUE)

startbool <-  rep(0, 101)
startbool[101] <- 1
director_101 <- list()

for(v in 1:length(movieDict_data)){
  print(v)
  boolean101 <- startbool
  inter <- match(directors_namedList[movieDict_data[[v]][1]], top100directors_list)
  inter <- inter[!is.na(inter)]
  k = length(inter)
  while(k){
    print("k")
    boolean101[inter[k]]<- 1
    boolean101[101]<-0
    k = k-1
  }
  director_101[[movieDict_data[[v]][1]]] <- boolean101
}



save(director_101,file="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/director_101.RData")
load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/director_101.RData")


# FEATURE 3

# Read movie rating data
genre_data <- list()
genre_con <- file("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/project_2_data/movie_genre.txt", "r")
genre_data <- readLines(genre_con)
close(genre_con)

genreDict_data <- strsplit(genre_data,"\t", perl = TRUE)

genreDict <- list()
for (g in 1: length(genreDict_data)){
  print(g)
  genreDict[[g]] <- c(gsub(" ","",genreDict_data[[g]])[1], genreDict_data[[g]][3])
}


## Export to RData file
filename_83 <- "genreDict.RData"
save(genreDict,file=filename_83)

filename_83 <- "/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/genreDict.RData"
load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/genreDict.RData")

save(genreDict, file="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/genreDict.RData")

genre_namedList <- list()
genre_namedList <- setNames(lapply(genreDict, `[`, 2), sapply(genreDict, `[`, 1))

save(genre_namedList, file="/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/genre_namedList.RData")



load("/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/ratingDict.RData")

rating_namedList <- setNames(lapply(ratingDict, `[`, 2), sapply(ratingDict, `[`, 1))

# Features are 
# top5pagerank 
# directors 101 
# genre 

setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/')


#Get movielist in the order of movieDict_data
movieNames <- c()
for(i in 1:length(movieDict_data)){
  print(i)
  movieNames <- c(movieNames,movieDict_data[[i]][1])
}

save(movieNames, file= 'movieNames.Rdata')
load('movieNames.Rdata')

#create genre data in the order of movie list 

genre_FT <- c()
for (i in 1:length(movieNames)){
  print(i)
  genre <- genre_namedList[[movieNames[i]]]
  if(!is.null(genre)){
    genre_FT[i] <- genre
  }
}

save(genre_FT, file= 'genre_FT.Rdata')
load('genre_FT.Rdata')

# create top5pagerank data in the order of movie list 

top5pgr_FT <- list()
for (i in 1:length(top5pgr)){
  print(i)
  top5pgr_FT[[i]] <- top5pgr[[i]][!is.na(top5pgr[[i]])]
}

save(top5pgr_FT, file = 'top5pgr_FT.Rdata')
load('top5pgr_FT.Rdata')
load('top5pgr.Rdata')

# create director101_FT data in the order of movie list 
#Have not run it yet

director101_FT <- list()
for (i in 1:5){#length(movieNames)){
  print(i)
  director101_FT[[i]] <- director_101[[i]][!is.na(director_101[[i]])]
}

save(director101_FT, file = 'director101_FT.Rdata')
load('director_101.Rdata')

# create ratings data in the order of movie list. 
rating_output <- c()
for (i in 1:length(movieNames)){
  print(i)
  rating_val <- rating_namedList[[movieNames[i]]]
  if(!is.null(rating_val)){
    rating_output[i] <- rating_val
  }
}
save(rating_output, file = 'rating_output.Rdata')
load('rating_output.Rdata')

rating_output_aslist <- list()
for (i in 1:length(movieNames)){
  print(i)
  rating_output_aslist[[movieNames[[i]]]] <- rating_output[i]
}
save(rating_output_aslist, file = 'rating_output_aslist.Rdata')
load('rating_output_aslist.Rdata')

# TEST 
t = 1
l = length(top5pgr)
test_top5 <- top5pgr[t:l]
test_genre <- genre_FT[t:l]
test_director <- director_101[t:l]
test_output <- rating_output[t:l]
test_matrix_top5pgr <- matrix(unlist(test_top5), ncol = 5, byrow = TRUE)
test_matrix_director101 <- matrix(unlist(test_director), ncol = 101, byrow = TRUE)

test_Z1 = as.data.frame(cbind(test_output, test_genre))
test_Z2 = as.data.frame(cbind(test_Z1,test_matrix_top5pgr))
test_Z3 = as.data.frame(cbind(test_Z2,test_matrix_director101))


testdelete<- test_Z3[!is.na(test_Z3$test_output),]

testdelete$test_output <- as.double(as.character(testdelete$test_output))
testdelete$test_genre<- as.character(testdelete$test_genre)
testdelete$'1'<- as.double(as.character(testdelete$'1'))
testdelete$`1`[is.na(testdelete$`1`)]<- mean(testdelete$'1', na.rm = TRUE)
testdelete$'2'<- as.double(as.character(testdelete$'2'))
testdelete$`2`[is.na(testdelete$`2`)]<- mean(testdelete$'2', na.rm = TRUE)
testdelete$'3'<- as.double(as.character(testdelete$'3'))
testdelete$`3`[is.na(testdelete$`3`)]<- mean(testdelete$'3', na.rm = TRUE)
testdelete$'4'<- as.double(as.character(testdelete$'4'))
testdelete$`4`[is.na(testdelete$`4`)]<- mean(testdelete$'4', na.rm = TRUE)
testdelete$'5'<- as.double(as.character(testdelete$'5'))
testdelete$`5`[is.na(testdelete$`5`)]<- mean(testdelete$'5', na.rm = TRUE)

colnames(testdelete) <- c("rating","genre","pgr1","pgr2","pgr3","pgr4","pgr5",
                  "bol1","bol2","bol3","bol4","bol5","bol6","bol7","bol8","bol9","bol10",
                  "bol11","bol12","bol13","bol14","bol15","bol16","bol17","bol18","bol19","bol20",
                  "bol21","bol22","bol23","bol24","bol25","bol26","bol27","bol28","bol29","bol30",
                  "bol31","bol32","bol33","bol34","bol35","bol36","bol37","bol38","bol39","bol40",
                  "bol41","bol42","bol43","bol44","bol45","bol46","bol47","bol48","bol49","bol50",
                  "bol51","bol52","bol53","bol54","bol55","bol56","bol57","bol58","bol59","bol60",
                  "bol61","bol62","bol63","bol64","bol65","bol66","bol67","bol68","bol69","bol70",
                  "bol71","bol72","bol73","bol74","bol75","bol76","bol77","bol78","bol79","bol80",
                  "bol81","bol82","bol83","bol84","bol85","bol86","bol87","bol88","bol89","bol90",
                  "bol91","bol92","bol93","bol94","bol95","bol96","bol97","bol98","bol99","bol100",
                  "bol101")



#avgrating_animation <- mean(testdelete$test_output[which(testdelete$test_genre == "Animation")])
#avgrating_animation <- mean(testdelete$test_output[which(testdelete$test_genre == "Mystery")])


plot (rating~testdelete$'bol91', testdelete)

test_model1 = lm(rating~., data = testdelete)
#abline(test_model1,col="red")

#plot(test_model1)
summary(test_model1)








#Data we predict 
threemovies_names <- c("Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)", "Minions (2015)")
threemovies_directors <- list()
threemovies_directors[["Batman v Superman: Dawn of Justice (2016)"]] <- "Snyder, Zack"
threemovies_directors[["Mission: Impossible - Rogue Nation (2015)"]] <- "McQuarrie, Christopher"
threemovies_directors[["Minions (2015)"]] <- c("Balda, Kyle", "Coffin, Pierre")

threemovies_actors <- list()
threemovies_actors[["Batman v Superman: Dawn of Justice (2016)"]] <- c("Affleck,Ben", "Cavill,Henry", "Adams,Amy(III)", "Eisenberg,Jesse", "Lane,Diane(I)", 
                                                                       "Fishburne,Laurence", "Irons,Jeremy", "Hunter,Holly", "Gadot,Gal", "McNairy,Scoot",
                                                                       "Mulvey,Callan")
threemovies_actors[["Mission: Impossible - Rogue Nation (2015)"]] <- c("Cruise,Tom", "Ferguson,Rebecca(I)", "Renner,Jeremy", 
                                                                       "Pegg,Simon", "Rhames,Ving", "Harris,Sean(I)","McBurney,Simon","Zhang,Jingchu",
                                                                       "Hollander,Tom")
threemovies_actors[["Minions (2015)"]] <- c("Bullock,Sandra", "Hamm,Jon", "Keaton,Michael", "Janney,Allison", "Coogan,Steve", "Saunders,Jennifer(I)",
                                            "Rush,Geoffrey", "Carell,Steve")

threemovies_actorspr <- list()

threemovies_genre<- c("Action", "Action", "Animation")

load('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/g_actor_pr.Rdata')
load('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2/top5pageranks.Rdata')

#threemovies_Z1 = as.data.frame(cbind(threemovies_genre, threemovies_directors))

threemovies_actorspr <- list()
for (i in 1:length(g_actor_pr)){
  for (t in 1:length(threemovies_actors[["Batman v Superman: Dawn of Justice (2016)"]])){
    if(names(g_actor_pr[i]) == threemovies_actors[["Batman v Superman: Dawn of Justice (2016)"]][t]){
      print(names(g_actor_pr[i]))
      threemovies_actorspr[["Batman v Superman: Dawn of Justice (2016)"]] <- c(threemovies_actorspr[["Batman v Superman: Dawn of Justice (2016)"]], g_actor_pr[i])
    }
  }
}

for (i in 1:length(g_actor_pr)){
  for (t in 1:length(threemovies_actors[["Mission: Impossible - Rogue Nation (2015)"]])){
    if(names(g_actor_pr[i]) == threemovies_actors[["Mission: Impossible - Rogue Nation (2015)"]][t]){
      print(names(g_actor_pr[i]))
      threemovies_actorspr[["Mission: Impossible - Rogue Nation (2015)"]] <- c(threemovies_actorspr[["Mission: Impossible - Rogue Nation (2015)"]], g_actor_pr[i])
    }
  }
}


for (i in 1:length(g_actor_pr)){
  for (t in 1:length(threemovies_actors[["Minions (2015)"]] )){
    if(names(g_actor_pr[i]) == threemovies_actors[["Minions (2015)"]] [t]){
      print(names(g_actor_pr[i]))
      threemovies_actorspr[["Minions (2015)"]]  <- c(threemovies_actorspr[["Minions (2015)"]] , g_actor_pr[i])
    }
  }
}

threemovies_actorspr[["Batman v Superman: Dawn of Justice (2016)"]]<- sort(threemovies_actorspr[["Batman v Superman: Dawn of Justice (2016)"]], decreasing = TRUE)[1:5]
threemovies_actorspr[["Mission: Impossible - Rogue Nation (2015)"]]<- sort(threemovies_actorspr[["Mission: Impossible - Rogue Nation (2015)"]], decreasing = TRUE)[1:5]
threemovies_actorspr[["Minions (2015)"]]<- sort(threemovies_actorspr[["Minions (2015)"]], decreasing = TRUE)[1:5]


startbool <-  rep(0, 101)
startbool[101] <- 1


#for(v in 1:length(top100directors_list)){
#  if(top100directors_list[v]=="Coffin, Pierre"){
#    print("yes")
#  }
#}

threemovies_director_101 <- list()
threemovies_director_101[[1]] <- startbool
threemovies_director_101[[2]] <- startbool
threemovies_director_101[[3]] <- startbool

predict_matrix_top5pgr <- matrix(unlist(threemovies_actorspr), ncol = 5, byrow = TRUE)
predict_matrix_director101 <- matrix(unlist(threemovies_director_101), ncol = 101, byrow = TRUE)

predict_Z1 = as.data.frame(cbind(threemovies_genre, predict_matrix_top5pgr))
predict_Z2 = as.data.frame(cbind(predict_Z1, predict_matrix_director101))

colnames(predict_Z2) <- c("genre","pgr1","pgr2","pgr3","pgr4","pgr5",
                          "bol1","bol2","bol3","bol4","bol5","bol6","bol7","bol8","bol9","bol10",
                          "bol11","bol12","bol13","bol14","bol15","bol16","bol17","bol18","bol19","bol20",
                          "bol21","bol22","bol23","bol24","bol25","bol26","bol27","bol28","bol29","bol30",
                          "bol31","bol32","bol33","bol34","bol35","bol36","bol37","bol38","bol39","bol40",
                          "bol41","bol42","bol43","bol44","bol45","bol46","bol47","bol48","bol49","bol50",
                          "bol51","bol52","bol53","bol54","bol55","bol56","bol57","bol58","bol59","bol60",
                          "bol61","bol62","bol63","bol64","bol65","bol66","bol67","bol68","bol69","bol70",
                          "bol71","bol72","bol73","bol74","bol75","bol76","bol77","bol78","bol79","bol80",
                          "bol81","bol82","bol83","bol84","bol85","bol86","bol87","bol88","bol89","bol90",
                          "bol91","bol92","bol93","bol94","bol95","bol96","bol97","bol98","bol99","bol100",
                          "bol101")


predict_Z2$genre<- as.character(predict_Z2$genre)
predict_Z2$pgr1<- as.double(as.character(predict_Z2$pgr1))
predict_Z2$pgr1[is.na(predict_Z2$pgr1)]<- mean(predict_Z2$pgr1, na.rm = TRUE)

predict_Z2$pgr2<- as.double(as.character(predict_Z2$pgr2))
predict_Z2$pgr2[is.na(predict_Z2$pgr2)]<- mean(predict_Z2$pgr2, na.rm = TRUE)

predict_Z2$pgr3<- as.double(as.character(predict_Z2$pgr3))
predict_Z2$pgr3[is.na(predict_Z2$pgr3)]<- mean(predict_Z2$pgr3, na.rm = TRUE)

predict_Z2$pgr4<- as.double(as.character(predict_Z2$pgr4))
predict_Z2$pgr4[is.na(predict_Z2$pgr4)]<- mean(predict_Z2$pgr4, na.rm = TRUE)

predict_Z2$pgr5<- as.double(as.character(predict_Z2$pgr5))
predict_Z2$pgr5[is.na(predict_Z2$pgr5)]<- mean(predict_Z2$pgr5, na.rm = TRUE)


predict(test_model1, predict_Z2)




#Problem 9 Bonus Question 

setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E_project2_data/')
bipartite_edgelist <- "ActorMovieBipartiteNetwork.txt"
# Read in graph from file
g_bipartite <- read_graph(bipartite_edgelist,format="ncol",directed=FALSE)

#Getting neighbors of three movies
neighbors_list_batman <- list()
for(i in 1:length(threemovies_actors[[1]])){
  actor <- threemovies_actors[[1]][i]
  neighbors_list_batman[[actor]] <- c(neighbors(g_bipartite, which(names(V(g_bipartite))==actor)))
}

neighbors_list_mission <- list()
for(i in 1:length(threemovies_actors[[2]])){
  actor <- threemovies_actors[[2]][i]
  neighbors_list_mission[[actor]] <- c(neighbors(g_bipartite, which(names(V(g_bipartite))==actor)))
}

neighbors_list_minion <- list()
for(i in 1:length(threemovies_actors[[3]])){
  actor <- threemovies_actors[[3]][i]
  neighbors_list_minion[[actor]] <- c(neighbors(g_bipartite, which(names(V(g_bipartite))==actor)))
}

rating_list_batman <- list()

for(i in 1:length(neighbors_list_batman)){
  for (t in 1:length(neighbors_list_batman[[i]])){
    movie <- names(neighbors_list_batman[[i]][t])
    ratingval <- rating_namedList[[movie]]
    if(!is.null(ratingval)){
      rating_list_batman[[names(neighbors_list_batman[i])]] <- c(rating_list_batman[[names(neighbors_list_batman[i])]],ratingval)
    }
  }
}

rating_list_mission <- list()
for(i in 1:length(neighbors_list_mission)){
  for (t in 1:length(neighbors_list_mission[[i]])){
    movie <- names(neighbors_list_mission[[i]][t])
    ratingval <- rating_namedList[[movie]]
    if(!is.null(ratingval)){
      rating_list_mission[[names(neighbors_list_mission[i])]] <- c(rating_list_mission[[names(neighbors_list_mission[i])]],ratingval)
    }
  }
}


rating_list_minion <- list()
for(i in 1:length(neighbors_list_minion)){
  for (t in 1:length(neighbors_list_minion[[i]])){
    movie <- names(neighbors_list_minion[[i]][t])
    ratingval <- rating_namedList[[movie]]
    if(!is.null(ratingval)){
      rating_list_minion[[names(neighbors_list_minion[i])]] <- c(rating_list_minion[[names(neighbors_list_minion[i])]],ratingval)
    }
  }
}


rateofactor_batman <- c()
for(i in 1:length(rating_list_batman)){
  rateofactor_batman <- c(rateofactor_batman, mean(as.double(rating_list_batman[[i]])))
  print(rateofactor_batman)
  print(i)
}

rateofactor_mission <- c()
for(i in 1:length(rating_list_mission)){
  rateofactor_mission <- c(rateofactor_mission, mean(as.double(rating_list_mission[[i]])))
  print(rateofactor_mission)
  print(i)
}

rateofactor_minion <- c()
for(i in 1:length(rating_list_minion)){
  rateofactor_minion <- c(rateofactor_minion, mean(as.double(rating_list_minion[[i]])))
  print(rateofactor_minion)
  print(i)
}


avg_batman <- mean(rateofactor_batman)
avg_mission<- mean(rateofactor_mission)
avg_minion <- mean(rateofactor_minion)










