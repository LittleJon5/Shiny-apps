require(shiny)
require(forecast)
require(ggplot2)
require(fImport)
require(quantmod)

shinyServer(function(input, output) {
  
  data.type <- reactive({
    getSymbols(input$data.type)
  })
  
  output$ts <- renderPlot({
    
    new.data <- fredSeries(data.type(), from = input$date)
    
    plot.data <- data.frame(time(AirPassengers), AirPassengers)
    names(plot.data) <- c("time", "value")
    
    
  ggplot(data = plot.data, aes(x = as.Date(time), y = value)) +
      geom_point(color = "black") +
      geom_line(color = "red") +
      geom_smooth() +
      labs(x = "Time", y = "Value", title = "Fred Time Series \n")
    
  })
  
  }
)


