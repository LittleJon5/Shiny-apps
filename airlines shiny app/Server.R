library(shiny)
require(forecast)
require(ggplot2)
require(ggvis)
load("data/AirPassengers.rdata")
plotData <- data.frame(time(AirPassengers), AirPassengers)
names(plotData) <- c("time", "passengers")

shinyServer(function(input, output) {
  output$ts <- renderPlot({
    
    new.data <- BoxCox(plotData, input$lambda)
    
    ggplot(data = new.data, aes(x = as.Date(time), y = passengers)) +
      geom_point(color = input$pointColor) +
      geom_line(color = input$lineColor) +
      geom_smooth(method = "loess", span = input$span)
  
    
                    
  })
  
})



