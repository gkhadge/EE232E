# Homework 4

rm(list=ls())
library(igraph)

# Load the stock edgelist
stock_file<-file("/Users/eunsunlee/Documents/EE232_HW4/StockNetworkFile.txt")
sector_file <- read.csv("/Users/eunsunlee/Documents/UCLA_Spring_2017/HW4/finance_data/Name_sector.csv", header = TRUE, sep = ",", quote = "\"")

# create stock graph
g_stocks <- read_graph(stock_file,format="ncol",directed=FALSE)

n_stocks <- length(V(g_stocks))
n_sectors <- 11

g_mst <- mst(g_stocks)
plot(g_mst, vertex.label = NA, vertex.size = 2)


sector_list <- setNames(object = sector_file$Sector, sector_file$Symbol)

sector_color <- rep(0, n_stocks)

sector_color[which(sector_list == "Health Care")] <- colors[1]
sector_color[which(sector_list == "Industrials")] <- colors[2]
sector_color[which(sector_list == "Consumer Discretionary")] <- colors[3]
sector_color[which(sector_list == "Information Technology")] <- colors[4]
sector_color[which(sector_list == "Consumer Staples")] <- colors[5]
sector_color[which(sector_list == "Utilities")] <- colors[6]
sector_color[which(sector_list == "Financials")] <- colors[7]
sector_color[which(sector_list == "Materials")] <- colors[8]
sector_color[which(sector_list == "Energy")] <- colors[9]
sector_color[which(sector_list == "Telecommunication Services")] <- colors[10]
sector_color[which(sector_list == "Real Estate")] <- colors[11]

sector_color_list <- setNames(object = sector_color, sector_file$Symbol)

sector_sorted_names <- rep(0, n_stocks)
sector_sorted_colors <- rep(0, n_stocks)

for (i in 1:n_stocks){
  sector_sorted_names[i] <- V(g_mst)[i]$name
  sector_sorted_colors[i] <- sector_color_list[V(g_mst)[i]$name]
}

plot(g_mst, vertex.label = NA, vertex.color=sector_sorted_colors, vertex.size = 5, main = "Daily MST")



# Problem 4 
numV = length(V(g_mst))

# For each node, calculate alpha
alpha_value <- c()
sector_value <- c()
for(i in 1:numV){
  # Node name
  nodeName = V(g_mst)[i]
  sectorName = sector_list[[nodeName]]
  # print(sectorName)
  neighborNodes = neighbors(g_mst, V(g_mst)[i])
  # print("neighborNodes:")
  # print(neighborNodes)
  samesector_list <- which(sector_list == sectorName)
  samesector_namelist <- V(g_mst)[samesector_list]
  # print("samesector_namelist")
  # print(samesector_namelist)
  numIntersection <- length(intersect(names(neighborNodes), names(samesector_namelist)))
  #print(numIntersection)
  alpha_value <- c(alpha_value, (numIntersection/length(neighbors)))
  sector_value <- c(sector_value, (length(samesector_namelist))/numV)
}


alpha_val <- (1/numV)*sum(alpha_value)
sector_val <- (1/numV)*sum(sector_value)
