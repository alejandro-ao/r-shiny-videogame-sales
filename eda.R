library(ggplot2)

vgsales <- read.csv("./data/vgsales.csv")

wii_sales <- vgsales[vgsales$Platform == "Wii",]
pc_sales <- vgsales[vgsales$Platform == "PC",]
ng_sales <- vgsales[vgsales$Platform == "NG",]
nes_sales <- vgsales[vgsales$Platform == "NES",]
psp_sales <- vgsales[vgsales$Platform == "PSP",]


Platform_ordered <- with(vgsales,
                   reorder(Platform,
                           NA_Sales,
                           median))

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
library(dplyr)
grouped_platforms <- group_by(vgsales, Platform)
summarise(grouped_platforms, median_sales = median(NA_Sales))



# data.frame("Platform"=unique(vgsales$Platform), "Sales in NA"=

