library(shiny)
library(ggplot2)

vgsales <- read.csv("./data/vgsales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Videogame Sales"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Please select the data that you would like to visualize."),
      selectInput('x_axis',label="Sales by", c(
        "Platform",
        # "Publishers (top 20)",
        "Genre"
      )),
      selectInput('y_axis',label="Select a region", c(
        "Global",
        "North America",
        "Europe",
        "Japan",
        "Rest of the world"
      )),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

#Define server logic
server<-function (input,output){
  
  output$distPlot <- renderPlot({


    x_axis <- switch(input$x_axis,
                     "Platform" = vgsales$Platform,
                     "Genre" = vgsales$Genre,
                     # "Publishers (top 20)" = vgsales$Publisher
                     )
    y_axis <- switch(input$y_axis,
                     "Global" = vgsales$Global_Sales,
                     "North America" = vgsales$NA_Sales,
                     "Europe" = vgsales$EU_Sales,
                     "Japan" = vgsales$JP_Sales,
                     "Rest of the world" = vgsales$Other_Sales
                     )
    plot_title <- paste(input$y_axis, "Sales by", input$x_axis)
    
    ggplot(vgsales,
           aes(
             x= reorder(x_axis,
                        y_axis,
                        median),
             y=y_axis)) +
      geom_boxplot() +
      labs(title=plot_title) +
      xlab(input$x_axis) + 
      ylab(paste(input$y_axis, "sales (in millions)")) +
      scale_y_log10() +
      theme(axis.text.x = element_text(angle = 90))
  })
}

#run the app
shinyApp(ui=ui, server=server)
