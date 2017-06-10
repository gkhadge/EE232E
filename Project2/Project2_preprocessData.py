#!/usr/bin/env python
from __future__ import division
import os
from collections import defaultdict
import pickle
import re
import csv

## Gourav Khadge, Yusi Ou, Katie Lee
## EE232E Project 2
## This file creates ActorNetwork.txt and MovieNetwork.txt for problems 2 and 4
## It also creates FilteredActorDict.pickle which stores the
## dictionary of Actors starring in 5 of more movies.

###############################################
## Set Path 
###############################################

## Set path for your computer to Project2 folder
path = "C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Project2"
##path = "/Users/Yusi/Documents/EE232E/Project2"
##path = "/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2"

os.chdir(path)

###############################################
## Create FilteredActorDict and store in pickle
###############################################
## Only need to run this section if FilteredActorDict.pickle has not been created yet
## Otherwise, comment this section_movies.txt"

## regex to extract movie titles
movie_title_regex = r'^.+\(([0-9][0-9][0-9][0-9]|\?\?\?\?).*?\)'
movie_title_search = re.compile(movie_title_regex)

# File locations of unfiltered actor movie files
UnfilteredActorFile = "project_2_data/actor_movies.txt"
UnfilteredActressFile = "project_2_data/actress_movies.txt"

print "Starting Actor File"

ActorDict = defaultdict(list)
##ActorDict = {}

with open(UnfilteredActorFile) as f:
    for line in f:
       tokens = line.split("\t\t")
       tokens = filter(str.strip, tokens) #Remove titles that are only whitespace
       
       if len(tokens) > 5:
           # Only include Actors with more than 5 movies
           for movie in tokens[1:]:
               try:
                   # Try to Extract Movie title that ends with (DDDD) year
                   title = movie_title_search.search(movie).group(0)
                   # Append it to the dictionary
                   ActorDict[tokens[0]].append(title.replace(' ', ''))
               except AttributeError:
                   # Movie Title not found with (DDDD) year at the end

                   # Make do with the original title, with whitespace stripped
                   makeshiftTitle = movie.strip()
                   # Add it to the dictionary
                   ActorDict[tokens[0]].append(makeshiftTitle.replace(' ', ''))

print "Starting Actress File"
                       
with open(UnfilteredActressFile) as f:
    for line in f:
       tokens = line.split("\t\t")
       tokens = filter(str.strip, tokens)#Remove titles that are only whitespace
              
       if len(tokens) > 5:
           # Only include Actors with more than 5 movies
           for movie in tokens[1:]:
               try:
                   # Try to Extract Movie title that ends with (DDDD) year
                   title = movie_title_search.search(movie).group(0)
                   # Append it to the dictionary (without whitespace in movies)
                   ActorDict[tokens[0]].append(title.replace(' ', ''))
               except AttributeError:
                   # Movie Title not found with (DDDD) year at the end

                   # Make do with the original title, with whitespace stripped
                   makeshiftTile = movie.strip()
                   # Add it to the dictionary
                   ActorDict[tokens[0]].append(makeshiftTile.replace(' ', ''))


# Movies have all whitespace removed
# Actors still have whitespace
with open('FilteredActorDict.pickle', 'wb') as handle:
    pickle.dump(ActorDict, handle, protocol=pickle.HIGHEST_PROTOCOL)

print "Created Filter Actor Dictionary pickle"

#################################################
#### Create DirectorLookUpDict.txt
#################################################
# Optional
##UnfilteredDirectorFile = "project_2_data/director_movies.txt"
##
print "Starting Director File"

DirectorDict = defaultdict(list)
with open(UnfilteredDirectorFile) as f:
   for line in f:
      tokens = line.split("\t\t")
      for movie in tokens[1:]:
         try:
             # Try to Extract Movie title that ends with (DDDD) year
             title = movie_title_search.search(movie).group(0)
             # Append it to the dictionary
             DirectorDict[tokens[0]].append(title.replace(' ', ''))
         except AttributeError:
             # Movie Title not found with (DDDD) year at the end

             # Make do with the original title, with whitespace stripped
             makeshiftTile = movie.strip()
             # Add it to the dictionary if it's not the empty string
             if makeshiftTile:
                 DirectorDict[tokens[0]].append(makeshiftTile.replace(' ', ''))

# Fill DirectorLookUpDict from DirectorDict
DirectorLookUpDict = defaultdict(list)
for director in DirectorDict:
    for movie in DirectorDict[director]:
        DirectorLookUpDict[movie].append(director)

f = open('DirectorLookUpDict.txt', 'wb') 
writer = csv.writer(f, delimiter = '\t')
for movie in DirectorLookUpDict:
   writer.writerow([movie] + DirectorLookUpDict[movie])
f.close()

###########################################
## Load FilteredActorDict from pickle
###########################################
## You can start here if FilteredActorDict.pickle is already available
## Always run this section

with open('FilteredActorDict.pickle', 'rb') as handle:
    ActorDict = pickle.load(handle)

# Fill MovieDict from ActorDict
MovieDict = defaultdict(list)
for actor in ActorDict:
    for movie in ActorDict[actor]:
        MovieDict[movie].append(actor.replace(' ', ''))

##f = open('MovieDict.txt', 'wb') 
##writer = csv.writer(f, delimiter = '\t')
##for movie in MovieDict:
##   writer.writerow([movie] + MovieDict[movie])
##f.close()

###############################################
#### Create ActorNetwork (Directed Edge List)
###############################################
#### Section is optional
        
ActorNetworkFile = "ActorNetwork.txt"
f = open(ActorNetworkFile, 'w')

# Progress Counter variables
numActors = len(ActorDict.keys())
count = 0

print "Creating Actor Network"
# Loop over all Actors in Dictionary
for actor_i in ActorDict.keys():
    # Number of movies each neighbor actor_j shares with actor_i
    ActorNeighborDict = defaultdict(list)

    # Progress Counter
    count += 1
    if (count % int(numActors/1000) == 0):
        print count/numActors

    # Loop over all Movies from actor_i
    for movie in ActorDict[actor_i]:
        # Loop over all actors costarring in this movie with actor_i
        for actor_j in MovieDict[movie]:
            if actor_j != actor_i: # Ignore actor_i costarring with him/herself
                if actor_j in ActorNeighborDict:
                    # Increment dict entry (+1 movies they've been in together)
                    ActorNeighborDict[actor_j] += 1
                else:
                    # If we've never never seen actor_j before, initialize dict entry
                    ActorNeighborDict[actor_j] = 1

    actor_i_noWS = actor_i.replace(' ', '')
    for actor_k in ActorNeighborDict:
        weight = ActorNeighborDict[actor_k]/len(ActorDict[actor_i])
        f.write(actor_i_noWS+"\t"+actor_k.replace(' ', '')+"\t"+("%.15f" % weight)+"\n")
        # python will convert \n to os.linesep
f.close()  # you can omit in most cases as the destructor will call it

print "Finished Creating Actor Network"

#######################################################
#### Create MovieNetwork (Undirected Edge List)
#######################################################
#### Section is optional

## Filter MovieDict and ActorDict of movies with less than 5 actors
movies = MovieDict.keys();
for movie in movies:
    if len(MovieDict[movie]) < 5:
        for actor in MovieDict[movie]:
            # Remove movie from each actor's list
            ActorDict[actor].remove(movie)
        # delete movie from MovieDict
        del MovieDict[movie]
        
MovieNetworkFile = "MovieNetwork.txt"
f = open(MovieNetworkFile, 'w')

# Progress Counter variables
numMovies = len(MovieDict.keys())
count = 0

# Sort list of Movies in alphabetical order
Movies = MovieDict.keys()
Movies_sorted = sorted(Movies);

print "Creating Movie Network"
# Loop through sorted movie list
for movie_i in Movies_sorted:
    # Number of actors each neighbor movie_j shares with movie_i
    MovieNeighborDict = {}

    # Progress Counter
    count += 1
    if (count % int(numMovies/1000) == 0):
        print count/numMovies

    # Loop over all actors in movie_i
    for actor in MovieDict[movie_i]:
        # Loop over all neighbor movies that this actor has also starred in
        for movie_j in ActorDict[actor]:
            # Ignore movie self loops and previously processed movies
            # Assume movie_i is input in alphabetically sorted order
            
            # Equivalent to: if movie_i -- movie_j not already handled before
            if movie_j > movie_i:
                if movie_j in MovieNeighborDict:
                    MovieNeighborDict[movie_j] += 1
                else:
                    MovieNeighborDict[movie_j] = 1

    #movie_i_noWS = movie_i.replace(' ', '')
    for movie_k in MovieNeighborDict:
        weight = MovieNeighborDict[movie_k]/len(set(MovieDict[movie_i]).union(set(MovieDict[movie_k])))
        f.write(movie_i+"\t"+movie_k+"\t"+("%.15f" % weight)+"\n")
        # python will convert \n to os.linesep
f.close()  # you can omit in most cases as the destructor will call it

print "Finished Creating Movie Network"

###############################################
#### Create ActorMovieBipartiteNetwork (Undirected Edge List)
###############################################
#### Section is optional
        
ActorMovieNetworkFile = "ActorMovieBipartiteNetwork.txt"
f = open(ActorMovieNetworkFile, 'w')

# Progress Counter variables
numActors = len(ActorDict.keys())
count = 0

print "Creating Actor Movie Bipartite Network"
# Loop over all Actors in Dictionary
for actor_i in ActorDict.keys():
    # Number of movies each neighbor actor_j shares with actor_i
    ActorNeighborDict = defaultdict(list)

    # Progress Counter
    count += 1
    if (count % int(numActors/1000) == 0):
        print count/numActors


    actor_i_noWS = actor_i.replace(' ', '')

    # Loop over all Movies from actor_i
    for movie in ActorDict[actor_i]:
        f.write(actor_i_noWS+"\t"+movie+"\n")
        # python will convert \n to os.linesep
f.close()  # you can omit in most cases as the destructor will call it

print "Finished Creating Actor Movie Bipartite Network"
