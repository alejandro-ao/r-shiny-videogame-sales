# Returns a dataframe with the total sales of the top 10 publishers
get_top_publishers <- function(vgsales) {
    vgsales_table <- data.table(vgsales)

    P_NA_Sales <- vgsales_table[, sum(NA_Sales), by = Publisher][order(-V1), ]
    TOP10_NA <- P_NA_Sales[1:10]

    P_EU_Sales <- vgsales_table[, sum(EU_Sales), by = Publisher][order(-V1), ]
    TOP10_EU <- P_EU_Sales[1:10]

    P_JP_Sales <- vgsales_table[, sum(JP_Sales), by = Publisher][order(-V1), ]
    TOP10_JP <- P_JP_Sales[1:10]

    P_Other_Sales <- vgsales_table[, sum(Other_Sales), by = Publisher][order(-V1), ]
    TOP10_Other <- P_Other_Sales[1:10]

    P_Global_Sales <- vgsales_table[, sum(Global_Sales), by = Publisher][order(-V1), ]
    TOP10_Global <- P_Global_Sales[1:10]


    Publisher_Sales <- merge(merge(merge(P_NA_Sales, P_EU_Sales, "Publisher"), P_JP_Sales, "Publisher"), P_Other_Sales, "Publisher")


    colnames(Publisher_Sales)[2] <- "North.America"
    colnames(Publisher_Sales)[3] <- "Europe"
    colnames(Publisher_Sales)[4] <- "Japan"
    colnames(Publisher_Sales)[5] <- "Other"

    Publisher_Sales <- merge(Publisher_Sales, P_Global_Sales, "Publisher")[order(-V1), ]
    colnames(Publisher_Sales)[6] <- "Global"

    top_10_publishers <- Publisher_Sales[1:10]
    # transposed_top_10 <- t(top_10_publishers)
    #
    # colnames(transposed_top_10) <- transposed_top_10[1,]
    # top_10_publishers <- transposed_top_10[-1, ]

    return(data.frame(top_10_publishers))
}

get_top_genres <- function(vgsales) {
  vgsales_table <- data.table(vgsales)
  
  G_NA_Sales <- vgsales_table[, sum(NA_Sales), by = Genre]
  G_EU_Sales <- vgsales_table[, sum(EU_Sales), by = Genre]
  G_JP_Sales <- vgsales_table[, sum(JP_Sales), by = Genre]
  G_Other_Sales <- vgsales_table[, sum(Other_Sales), by = Genre]
  G_Global_Sales <- vgsales_table[, sum(Global_Sales), by = Genre]
  
  Genre_Sales <- merge(merge(merge(G_NA_Sales,G_EU_Sales, 'Genre'), 
                             G_JP_Sales, 'Genre'), G_Other_Sales, 'Genre')
  colnames(Genre_Sales)[2] ="North.America"
  colnames(Genre_Sales)[3] ="Europe"
  colnames(Genre_Sales)[4] ="Japan"
  colnames(Genre_Sales)[5] ="Other"
  
  Genre_Sales <- merge(Genre_Sales, G_Global_Sales, 'Genre')[order(-V1),]
  colnames(Genre_Sales)[6] = "Global"
  
  return(Genre_Sales)
}