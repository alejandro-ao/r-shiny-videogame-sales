library(shiny)
library(ggplot2)
library(data.table)
source("./helpers.R")

vgsales <- read.csv("./data/vgsales.csv")

top_10_publishers <- get_top_publishers(vgsales)

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Video games sales visualisation dashboard"),

  # PANEL 1
  sidebarLayout(
    sidebarPanel(
      h4("Units sold"),
      helpText("Here you can visualize the sales of the highest grossing videogames released worldwide. Every dot in the plot represents one videogame."),
      helpText("Please select the data that you would like to visualize."),
      selectInput("x_axis", label = "Sales by", c(
        "Platform",
        "Genre"
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
  ),

  # PANEL 2
  sidebarLayout(
    sidebarPanel(
      h4("Top publishers"),
      helpText("Here you can visualize the total sales of the 10 best performing platforms by region."),
      helpText("Please select the region that you want to visualize"),
      selectInput("plot2_region", label = "Select a region", c(
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

# Define server logic
server <- function(input, output) {
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

    p <- ggplot(
      vgsales,
      aes(
        x = reorder(
          x_axis,
          y_axis,
          median
        ),
        y = y_axis
      )
    ) +
      geom_boxplot(color = "#47B5FF", fill = "white", alpha = 0.4) +
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
    y_axis <- switch(input$plot2_region,
      "North America" = top_10_publishers$North.America,
      "Europe" = top_10_publishers$Europe,
      "Japan" = top_10_publishers$Japan,
      "Other" = top_10_publishers$Other,
      "Global" = top_10_publishers$Global
    )

    ggplot(
      top_10_publishers,
      aes(x = reorder(Publisher, y_axis, max), y = y_axis)
    ) +
      geom_bar(width = 1, stat = "identity", color = "white", fill = "#47B5FF", alpha = 0.5) +
      xlab("Publisher") +
      ylab(paste("Total sales in", input$plot2_region, "(in millions)")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 3))
  })
}

# run the app
shinyApp(ui = ui, server = server)
