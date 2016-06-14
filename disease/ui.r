library(shiny)
library(shinyBS)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Basic Disease Models"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Choose model",
                  c("SI", "SIS", "SIR"),
                  selected = "SI"),
      tags$div(
        tags$p("Move the sliders to change the input values of the 
               variables in the system.")
      ),
      sliderInput("S", "Number of Susceptibles", 
                  value = 9, min = 0,  max = 100, step = 1),
      sliderInput("I", "Number of Infected", 
                  value = 1, min = 0,  max = 100, step = 1),
      sliderInput("r", "Rate of Infection", 
                  value = 0.01, min = 0,  max = 1, step = 0.01),
      uiOutput("slider"),
      sliderInput("time", "Length of time",
                  value = 500, min = 100, max = 2000, step = 100),
      bsTooltip("time", "Note that the longer the time period, the longer the plots will take to generate.")
      ),
    
    
    mainPanel(
      withMathJax(),
      uiOutput("model_text"),
      plotOutput("plot")
    )
  )
))