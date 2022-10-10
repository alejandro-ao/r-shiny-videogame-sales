###### TEAM MEMBERS
# - David ROJAS
# - Julien BOUHELIER
# - Marc PEZERIL
# - Alejandro AVILA-ORTIZ

library(shiny)
library(ggplot2)
library(data.table)
source("./helpers.R")

vgsales <- read.csv("./data/vgsales.csv")

top_10_publishers <- get_top_publishers(vgsales)
Genre_Sales <- get_top_genres(vgsales)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("Video game sales visualization dashboard", align="center")),
  p("The goal of the of Video Games Sales Visualization Dashboard is to provide an overview of the sales by platform, gender or Publisher across different parts of the world.", align="center"),
  br(),
  
  
  # PANEL 1
  sidebarLayout(
    sidebarPanel(
      h4("Units sold"),
      helpText("Here you can visualize the sales of the highest grossing videogames released worldwide. Every dot in the plot represents one videogame."),
      helpText("Please select the data that you would like to visualize."),
      selectInput("table", "Which question do you want to answer?", c(
        "What type of film was sold the most?",
        "which platform is the most successfull?",
        "Which publisher made the most money?"
      )),
      selectInput("y_axis", label = "Select a region", c(
        "Global",
        "North America",
        "Europe",
        "Japan",
        "Rest of the world"
      )),
      checkboxInput("enable_log_scale", label = "See log scale", value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("vgsalesPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$vgsalesPlot <- renderPlot({
    # PLOT TITLE
    plot_title <- paste(input$y_axis, "Videogame Sales by", input$x_axis)
    
    y_axis_top_10 <- switch(input$y_axis,
                            "Global" = top_10_publishers$Global,
                            "North America" = top_10_publishers$North.America,
                            "Europe" = top_10_publishers$Europe,
                            "Japan" = top_10_publishers$Japan,
                            "Rest of the world" = top_10_publishers$Other
    )
    
    y_axis_vgsales <- switch(input$y_axis,
                             "Global" = vgsales$Global_Sales,
                             "North America" = vgsales$NA_Sales,
                             "Europe" = vgsales$EU_Sales,
                             "Japan" = vgsales$JP_Sales,
                             "Rest of the world" = vgsales$Other_Sales
    )
    
    y_axis_genre_sales <- switch(input$y_axis,
                                 "Global" = Genre_Sales$Global,
                                 "North America" = Genre_Sales$North.America,
                                 "Europe" = Genre_Sales$Europe,
                                 "Japan" = Genre_Sales$Japan,
                                 "Rest of the world" = Genre_Sales$Other
    )
    
    # PLATFORM PLOT
    platformPlot <- ggplot(
      vgsales,
      aes(
        x = reorder(vgsales$Platform, y_axis_vgsales, median),
        y = y_axis_vgsales
      )
    ) +
      geom_boxplot(color = "#47B5FF", fill = "white", alpha = 0.4) +
      xlab("Platform") +
      ylab(paste(input$y_axis, "sales (in millions)")) +
      theme(axis.text.x = element_text(angle = 90))
    if (input$enable_log_scale) {
      platformPlot <- platformPlot + scale_y_log10()
    }
    
    # GENRE PLOT
    genrePlot <- ggplot(
      Genre_Sales,
      aes(
        x = reorder(Genre, y_axis_genre_sales, max),
        y = y_axis_genre_sales
      )
    ) +
      geom_bar(width = 1, stat = "identity", color = "white", fill = "#47B5FF", alpha = 0.4) +
      xlab("Genres") +
      ylab(paste("Total sales in", input$y_axis, "(in millions)"))
      
    
    
    # PUBLISHER PLOT
    publisherPlot <- ggplot(
      top_10_publishers,
      aes(x = reorder(top_10_publishers$Publisher, y_axis_top_10, max), y = y_axis_top_10)
    ) +
      geom_bar(width = 1, stat = "identity", color = "white", fill = "#47B5FF", alpha = 0.5) +
      xlab("Publisher") +
      ylab(paste("Total sales in", input$y_axis, "(in millions)")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 3))
    
    output_result <- switch(input$table,
                            "which platform is the most successfull?" = platformPlot,
                            "Which publisher made the most money?" = publisherPlot,
                            "What type of film was sold the most?" = genrePlot
    )
    
    # SHOW RESULT
    output_result
  })
}

# run the app
shinyApp(ui = ui, server = server)
