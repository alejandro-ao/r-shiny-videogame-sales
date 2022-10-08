#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
      #selectInput('x',"Genre",vgsales$Genre),
      selectInput('x_axis',label="X Axis", c(
        "Platform",
        "Publishers (top 20)",
        "Genre"
      )),
      selectInput('y_axis',label="Y axis", c(
        "Global Sales",
        "NA Sales",
        "Europe Sales",
        "Japan Sales",
        "Rest of the world Sales"
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
                     "Publishers (top 20)" = vgsales$Publisher
                     )
    y_axis <- switch(input$y_axis,
                     "Global Sales" = vgsales$Global_Sales,
                     "NA Sales" = vgsales$NA_Sales,
                     "Europe Sales" = vgsales$EU_Sales,
                     "Japan Sales" = vgsales$JP_Sales,
                     "Rest of the world Sales" = vgsales$Other_Sales
                     )
    
    # x_plot <- with(vgsales, 
    #                reorder(x_axis, 
    #                        y_axis, 
    #                        median)
    #                )
    
    ggplot(vgsales, 
           aes(
             x= reorder(x_axis, 
                        y_axis, 
                        median),
             # x=x_axis,
             y=y_axis)) + 
      geom_boxplot() + 
      scale_y_log10() + 
      theme(axis.text.x = element_text(angle = 90))
  })
}

#run the app
shinyApp(ui=ui, server=server)
