# Homework 4

rm(list=ls())
library(igraph)

##################################################################################################
# Problem 3
##################################################################################################

# Load the stock edgelist
stock_file <- file("/Users/Yusi/Documents/EE232E/HW_4/StockNetworkFile.txt")
sector_file <- read.csv("/Users/Yusi/Documents/EE232E/HW_4/finance_data/Name_sector.csv", header = TRUE, sep = ",", quote = "\"")

# create stock graph
g_stocks <- read_graph(stock_file,format="ncol",directed=FALSE)

n_stocks <- length(V(g_stocks))
n_sectors <- 11

g_mst <- mst(g_stocks)
#write_graph(g_mst, 'StockNetwork_MST.txt', format = "ncol")
plot(g_mst, vertex.label = NA, vertex.size = 5)

sector_list <- setNames(object = sector_file$Sector, sector_file$Symbol)

sector_color <- rep(0, n_stocks)
colors_s <- rainbow(n_sectors)
colors_s[3] = "#FFFF00"

sector_color[which(sector_list == "Health Care")] <- colors_s[1]
sector_color[which(sector_list == "Industrials")] <- colors_s[2]
sector_color[which(sector_list == "Consumer Discretionary")] <- colors_s[3]
sector_color[which(sector_list == "Information Technology")] <- colors_s[4]
sector_color[which(sector_list == "Consumer Staples")] <- colors_s[5]
sector_color[which(sector_list == "Utilities")] <- colors_s[6]
sector_color[which(sector_list == "Financials")] <- colors_s[7]
sector_color[which(sector_list == "Materials")] <- colors_s[8]
sector_color[which(sector_list == "Energy")] <- colors_s[9]
sector_color[which(sector_list == "Telecommunication Services")] <- colors_s[10]
sector_color[which(sector_list == "Real Estate")] <- colors_s[11]

sector_color_list <- setNames(object = sector_color, sector_file$Symbol)

sector_sorted_names <- rep(0, n_stocks)
sector_sorted_colors <- rep(0, n_stocks)

for (i in 1:n_stocks){
  sector_sorted_names[i] <- V(g_mst)[i]$name
  sector_sorted_colors[i] <- sector_color_list[V(g_mst)[i]$name]
}

plot(g_mst, vertex.label = NA, vertex.color=sector_sorted_colors, vertex.size = 5, main = "Daily MST")

barplot(rep(1,11),col=colors_s, main = "Colors Used for Vertices")

##################################################################################################
# Problem 7: Modifying Correlation
##################################################################################################

# Load the stock edgelist
stock_file_mod <- file("/Users/Yusi/Documents/EE232E/HW_4/StockNetworkFileMod.txt")

# create stock graph
g_stocks_mod <- read_graph(stock_file_mod,format="ncol",directed=FALSE)

g_mst_mod <- mst(g_stocks_mod)
plot(g_mst_mod, vertex.label = NA, vertex.size = 5)

sector_sorted_names_mod <- rep(0, n_stocks)
sector_sorted_colors_mod <- rep(0, n_stocks)

for (i in 1:n_stocks){
  sector_sorted_names_mod[i] <- V(g_mst_mod)[i]$name
  sector_sorted_colors_mod[i] <- sector_color_list[V(g_mst_mod)[i]$name]
}

plot(g_mst_mod, vertex.label = NA, vertex.color=sector_sorted_colors_mod, vertex.size = 5, main = "Daily MST (Modified)")
