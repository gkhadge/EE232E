#!/usr/bin/env python
from __future__ import division
import os
from collections import defaultdict
import pickle
import re
import operator
import csv

## Gourav Khadge, Yusi Ou, Katie Lee
## EE232E Project 2
## This file finds genre labels for each community (>20% content)
## and saves it into CommGenre.csv 

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
## Otherwise, comment this section

# File locations of Movie Genre file
MovieGenreFile = "project_2_data/movie_genre.txt"

print "Starting Genre File"

GenreDict = {}
with open(MovieGenreFile) as f:
    for line in f:
       tokens = line.split("\t\t")
       GenreDict[tokens[0].replace(' ', '')] = tokens[1].replace(' ', '').strip()
       
# Movies have all whitespace removed
# Actors still have whitespace
with open('GenreDict.pickle', 'wb') as handle:
    pickle.dump(GenreDict, handle, protocol=pickle.HIGHEST_PROTOCOL)

print "Created Genre Dictionary pickle"

###########################################
## Load FilteredActorDict from pickle
###########################################
## You can start here if FilteredActorDict.pickle is already available
## Always run this section

with open('GenreDict.pickle', 'rb') as handle:
    GenreDict = pickle.load(handle)

# File locations of community file
MovieCommFile = "FGNetworkCommunity.txt"

print "Starting Community File"

CommunityList = []
with open(MovieCommFile) as f:
    for line in f:
       tokens = line.split()
       CommunityList.append(tokens)

#############################################
## Perform Community Genre Analysis
#############################################

numComms = len(CommunityList)
GenreAppearances = [defaultdict(list) for _ in range(numComms)]

CommGenre = [[] for i in range(numComms)]

for comm in range(numComms):
   for movie in CommunityList[comm]:
      if movie in GenreDict:
         movie_genre = GenreDict[movie]
         if movie_genre in GenreAppearances[comm]:
            # Increment dict entry (+1 movies in genre)
            GenreAppearances[comm][movie_genre] += 1
         else:
            # If we've never never seen movie_genre before, initialize dict entry
            GenreAppearances[comm][movie_genre] = 1
   for movie_genre in GenreAppearances[comm]:
      GenreAppearances[comm][movie_genre] = GenreAppearances[comm][movie_genre]/len(CommunityList[comm])
      if GenreAppearances[comm][movie_genre] > 0.2:
         CommGenre[comm].append([movie_genre,GenreAppearances[comm][movie_genre]])

#############################################
## Save to CSV
#############################################
with open('CommGenre.csv', 'wb') as csvfile:
   writer = csv.writer(csvfile)
   ind = 0
   for CommGenreInst in CommGenre:
      writer.writerow([len(CommunityList[ind])] + CommGenreInst)
      ind = ind + 1
                       
