# Packages
library(shiny)

# UI
ui <- fluidPage(

  # App title panel
  titlePanel("PosteriorMethodsApp"),

  # Grid approximation
  h1("Method 1: Grid approximation"),

  # Grid length input
  numericInput(
    "grid_length",
    "Grid length",
    min = 5,
    max = 1000,
    step = 1,
    value = 5
  ),

  # Print grid length
  textOutput("grid_points")
)

# Server
server <- function(input, output, session) {
  grid_points <- reactive({
    input$grid_length
  })

  output$grid_points <- renderText(grid_points())
}

# Construct app
shinyApp(ui, server)
