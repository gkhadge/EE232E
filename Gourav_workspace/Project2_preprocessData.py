import os
from collections import defaultdict

#setwd('C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Project2')
#setwd('/Users/Yusi/Documents/EE232E/Project2')
#setwd('/Users/eunsunlee/Documents/UCLA_Spring_2017/EE232E/Project2')
path = "C:\\Users\\gkhadge\\Documents\\UCLA\\EE232E\\Project2"

os.chdir(path)
ActorFile = "filtered_actor_data.txt"

# Fill ActorDict
ActorDict = {}
with open(ActorFile) as f:
    for line in f:
       tokens = line.split("\t\t")
       tokens[-1] = tokens[-1][:-1] # Hack to remove last newlines
       ActorDict[tokens[0]] = tokens[1:]

# Fill MovieDict
MovieDict = defaultdict(list)
for actor in ActorDict:
    for movie in ActorDict[actor]:
        MovieDict[movie].append(actor)

