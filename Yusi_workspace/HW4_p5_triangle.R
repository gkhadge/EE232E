##################################################################################################
# Problem 5
##################################################################################################

# Load the stock edgelist
stock_file <- file("/Users/Yusi/Documents/EE232E/HW_4/StockNetworkFile.txt")
sector_file <- read.csv("/Users/Yusi/Documents/EE232E/HW_4/finance_data/Name_sector.csv", header = TRUE, sep = ",", quote = "\"")

# create stock graph
g_stocks <- read_graph(stock_file,format="ncol",directed=FALSE)

g_tri <- triangles(g_stocks)

violation_count <- 0

for (i in seq(1,length(g_tri)-3,3)) {
  l1 <- distances(g_stocks, g_tri[i], g_tri[i+1])
  l2 <- distances(g_stocks, g_tri[i+1], g_tri[i+2])
  l3 <- distances(g_stocks, g_tri[i], g_tri[i+2])
  if ((l1+l2 < l3) | (l1+l3 < l2) | (l2+l3 < l1)){
    violation_count = violation_count+1
  }
  print(violation_count)
}