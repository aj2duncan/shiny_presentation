library(shiny)

shinyApp(ui = shinyUI(fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins", label = "Number of bins:",
                  min = 1, max = 50, value = 30),
      # add an action button
      actionButton(inputId = "plot", label = "Action Button")
    ),
    # plot and text output
    mainPanel(
      plotOutput(outputId = "disPlot"),
      textOutput(outputId = "disText")
    )
  )
)),

# Define server logic required to draw a histogram
server = function(input, output) {
  output$disPlot = renderPlot({
    input$plot # listen to the actionButton
    x    = faithful[, 2] 
    isolate({     # isolate the plot which depends on the 'bins' input
      bins = seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
  })
  output$disText = renderText({
    # chosen not to isolate the text, it will still listen.
    paste("Number of bins:", input$bins)
  })
})