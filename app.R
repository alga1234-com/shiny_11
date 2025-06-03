library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Pick a number", 1, 100, 50)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    paste("You selected", input$num)
  })
}

shinyApp(ui = ui, server = server)
