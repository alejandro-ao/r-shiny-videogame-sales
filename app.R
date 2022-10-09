library(shiny)
library(ggplot2)
library(data.table)

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
  
  
  colnames(Publisher_Sales)[2] ="North.America"
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
  
  return(data.frame(top_10_publishers))
  
}
top_10_publishers <- get_top_publishers(vgsales)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Videogame sales visualisation dashboard"),
  
  # PANEL 1
  sidebarLayout(
    sidebarPanel(
      helpText("Here you can visualize the sales of the highest grossing videogames released in the world. Every dot in the plot represents one videogame."),
      helpText("Please select the data that you would like to visualize."),
      selectInput('x_axis',label="Sales by", c(
        "Platform",
        "Genre"
      )),
      selectInput('y_axis',label="Select a region", c(
        "Global",
        "North America",
        "Europe",
        "Japan",
        "Rest of the world"
      )),
      checkboxInput("enable_log_scale", label="See log scale", value=TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("vgsalesPlot")
    )
  ),
  
  # PANEL 2
  sidebarLayout(
    sidebarPanel(
      helpText("Here you can visualize the total sales by the 10 best performing platforms by region."),
      helpText("Please select the region that you want to visualize"),
      selectInput('plot2_region',label="Select a publisher", c(
        "Global",
        "North America",
        "Europe",
        "Japan",
        "Other"
      )),
    ),
    mainPanel(
      plotOutput("platformSalesPlot")
    )
  )
)

#Define server logic
server<-function (input,output){
  output$vgsalesPlot <- renderPlot({

    x_axis <- switch(input$x_axis,
                     "Platform" = vgsales$Platform,
                     "Genre" = vgsales$Genre,
                     )
    y_axis <- switch(input$y_axis,
                     "Global" = vgsales$Global_Sales,
                     "North America" = vgsales$NA_Sales,
                     "Europe" = vgsales$EU_Sales,
                     "Japan" = vgsales$JP_Sales,
                     "Rest of the world" = vgsales$Other_Sales
                     )
    plot_title <- paste(input$y_axis, "Videogame Sales by", input$x_axis)
    
    p <- ggplot(vgsales,
           aes(
             x= reorder(x_axis,
                        y_axis,
                        median),
             y=y_axis)) +
      geom_boxplot(color="#47B5FF", fill="white", alpha=0.4) +
      # labs(title=plot_title) +
      xlab(input$x_axis) + 
      ylab(paste(input$y_axis, "sales (in millions)")) +
      theme(axis.text.x = element_text(angle = 90))
    
    if (input$enable_log_scale) {
      p + scale_y_log10()
    } else {
      p
    }
    
  })
  output$platformSalesPlot <- renderPlot({
    
    y_axis <- switch (input$plot2_region,
                      "North America" = top_10_publishers$North.America,
                      "Europe" = top_10_publishers$Europe,
                      "Japan" = top_10_publishers$Japan,
                      "Other" = top_10_publishers$Other,
                      "Global" = top_10_publishers$Global
                      )
    
    ggplot(top_10_publishers, 
           aes(x=reorder(Publisher, y_axis, max), y=y_axis)) +
      geom_bar(width = 1, stat = "identity", color="white", fill="#47B5FF", alpha=0.5) +
      xlab("Publisher") +
      ylab(paste("Total sales in", input$plot2_region,"(in Millions)")) +
      scale_x_discrete(guide = guide_axis(n.dodge=3))
  })
}

#run the app
shinyApp(ui=ui, server=server)
