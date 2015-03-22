library(shiny)

shinyUI(fluidPage(
  titlePanel("AirPassenger Time Series Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("lambda",
                  label = "Power Transformer",
                  min = -3, max = 3, value = 0, step = .1),
      
      sliderInput("span",
                  label = "Loess Smoother",
                  min = .21, max = 1, value = .5, step = .01),
  
    
      selectInput("lineColor", label = "Line Color", 
                choices = c( "green", "blue", "red", "yellow", "orange", "black"),
                selected = "black"),
    
      selectInput("pointColor", label = "Point Color", 
                  choices = c( "green", "blue", "red", "yellow", "orange", "black"),
                  selected = "red")
    ),
    
    mainPanel(
      plotOutput("ts")
    )
  )
))


