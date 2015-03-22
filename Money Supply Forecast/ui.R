library(shiny)

shinyUI(fluidPage(
  titlePanel("Money Supply Time Series Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("periods",
                  label = "Numer of Periods to forecast",
                  min = 1, max = 20, value = 1, step = 1),
      
      sliderInput("span",
                  label = "Loess Smoother",
                  min = .01, max = 1, value = 0, step = .01),
  
    
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


