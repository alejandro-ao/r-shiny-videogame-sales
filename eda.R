library(ggplot2)

vgsales <- read.csv("./data/vgsales.csv")

Platform_ordered <- with(vgsales,
                   reorder(Platform,
                           NA_Sales,
                           sum))

ggplot(Platform_ordered, aes(x=Platform, y=Global_sales)) + geom_bar()

# reordered_vgsales <- vgsales
# reordered_vgsales$Platform <- factor(reordered_vgsales$Platform,
#                                      levels = levels(Platform_ordered))

# boxplot all platforms
ggplot(vgsales,
       aes(
         x= reorder(Platform,
                    NA_Sales,
                    median),
         y=NA_Sales)) +
  geom_boxplot() +
  xlab("Platform") + 
  ylab("Sales (in millions)") +
  ggtitle("Games sold by platform in North America") +
  # ylim(0,0.1) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))

# boxplot only DS
ds_sales <- vgsales[vgsales$Platform == "DS",]
ggplot(ds_sales,
       aes(
         x= Platform,
         y=NA_Sales)) +
  geom_boxplot() +
  xlab("Platform") + 
  ylab("Sales (in millions)") +
  ggtitle("Nintendo DS Games sold in North America") +
  ylim(0,0.1) +
  # scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))

# boxplot only PS4
ps4_sales <- vgsales[vgsales$Platform == "PS4",]
ggplot(ps4_sales,
       aes(
         x= Platform,
         y=NA_Sales)) +
  geom_boxplot() +
  xlab("Platform") + 
  ylab("Sales (in millions)") +
  ggtitle("PS4 Games sold in North America") +
  ylim(0,0.1) +
  # scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))

# table of all platforms by median sales in NA_Sales


### 5. Create dataframe of the sales of top 10 publishers around the world

get_top_publishers <- function() {
  vgsales <- read.csv("./data/vgsales.csv")
  library(data.table)
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
  
  return(Publisher_Sales[1:10])
}
top10 <- get_top_publishers()
View(top10)

ggplot(TOP_10_Publisher_Global_Sales, 
  aes(x=reorder(Publisher,Global,max), y=Global, fill=Other)) +
  geom_bar(width = 1, stat = "identity")

