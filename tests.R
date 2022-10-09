vgsales <- read.csv("./data/vgsales.csv")

get_top_publishers <- function(vgsales) {
  vgsales_table <- data.table(vgsales)
  
  P_NA_Sales <- vgsales_table[, sum(NA_Sales), by = Publisher][order(-V1),]
  TOP10_NA <- P_NA_Sales[1:10]
  
  P_EU_Sales <- vgsales_table[, sum(EU_Sales), by = Publisher][order(-V1),]
  TOP10_EU <- P_EU_Sales[1:10]
  
  P_JP_Sales <- vgsales_table[, sum(JP_Sales), by = Publisher][order(-V1),]
  TOP10_JP <- P_JP_Sales[1:10]
  
  P_Other_Sales <- vgsales_table[, sum(Other_Sales), by = Publisher][order(-V1),]
  TOP10_Other <- P_Other_Sales[1:10]
  
  P_Global_Sales <- vgsales_table[, sum(Global_Sales), by = Publisher][order(-V1),]
  TOP10_Global <- P_Global_Sales[1:10]
  
  
  Publisher_Sales <- merge(merge(merge(P_NA_Sales,P_EU_Sales, 'Publisher'), P_JP_Sales, 'Publisher'), P_Other_Sales, 'Publisher')
  
  
  colnames(Publisher_Sales)[2] ="North America"
  colnames(Publisher_Sales)[3] ="Europe"
  colnames(Publisher_Sales)[4] ="Japan"
  colnames(Publisher_Sales)[5] ="Other"
  
  Publisher_Sales <- merge(Publisher_Sales, P_Global_Sales, 'Publisher')[order(-V1),]
  colnames(Publisher_Sales)[6] = "Global"
  
  top_10_publishers <- Publisher_Sales[1:10]
  # transposed_top_10 <- t(top_10_publishers)
  # 
  # colnames(transposed_top_10) <- transposed_top_10[1,]
  # top_10_publishers <- transposed_top_10[-1, ]
  
  return(top_10_publishers)
  
}
top_10_publishers <- get_top_publishers(vgsales)