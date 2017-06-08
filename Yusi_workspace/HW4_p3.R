# Homework 4

rm(list=ls())
library(igraph)

# Load the stock edgelist
stock_file<-file("/Users/Yusi/Documents/EE232E/HW_4/StockNetworkFile.txt")
sector_file <- read.csv("/Users/Yusi/Documents/EE232E/HW_4/finance_data/Name_sector.csv", header = TRUE, sep = ",", quote = "\"")

# create stock graph
g_stocks <- read_graph(stock_file,format="ncol",directed=FALSE)

n_stocks <- length(V(g_stocks))
n_sectors <- 11

g_mst <- mst(g_stocks, weights = NULL, algorithm = 'prim')
plot(g_mst, vertex.label = NA, vertex.size = 2)

sector_list <- setNames(object = sector_file$Sector, sector_file$Symbol)
sector_list_ind <- sector_list

for (i in 1:n_stocks){
  if (sector_list[i] == 'Health Care'){
    sector_list_ind[i] = 1
  }
  if (sector_list[i] == 'Industrials'){
    sector_list_ind[i] = 2
  }
  if (sector_list[i] == 'Consumer Discretionary'){
    sector_list_ind[i] = 3
  }
  if (sector_list[i] == 'Information Technology'){
    sector_list_ind[i] = 4
  }
  if (sector_list[i] == 'Consumer Staples'){
    sector_list_ind[i] = 5
  }
  if (sector_list[i] == 'Utilities'){
    sector_list_ind[i] = 6
  }
  if (sector_list[i] == 'Financials'){
    sector_list_ind[i] = 7
  }
  if (sector_list[i] == 'Real Estate'){
    sector_list_ind[i] = 8
  }
  if (sector_list[i] == 'Materials'){
    sector_list_ind[i] = 9
  }
  if (sector_list[i] == 'Energy'){
    sector_list_ind[i] = 10
  }
  if (sector_list[i] == 'Telecommunication Services'){
    sector_list_ind[i] = 11
  }
}