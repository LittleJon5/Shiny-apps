library(shiny)
library(forecast)
library(ggplot2)
library(fImport)

shinyUI(fluidPage(
# ------------------------- This loads the css file for app style
  includeCSS("styles.css"),
  
  titlePanel(h1("Macro Economic Indicators")),
  
  sidebarLayout(
    sidebarPanel(
      textInput("data.input", label = h3("Fred Ticker")
      ),
      
      actionButton("get.data", "Get Fred Data"),  
      
      selectInput("manipulate", label = h3("Manipulate Options"),
                  choices = c("No Transfromation",
                              "Change",
                              "Change From a Year Ago",
                              "Percent Change",
                              "Percent Change from a Year Ago",
                              "Compounded Annual Rate of Change",
                              "Continuously Compounded Rate of Change",
                              "Countinuously Compounded Annual Rate of Change",
                              "Natural Log"),
                  selected = "No Transformation"),
      
      dateInput("date", 
                label = h3("Date Input"), 
                value = "1959-01-01"),
      
      numericInput("horizon", 
                   label = h3("Forecast Horizon"), 
                   value = 12), 
      
      sliderInput("smooth",
                  label = h3("Smoother"),
                  min=.05,
                  max=.5,
                  value= .21,
                  animate = TRUE),
      
      numericInput("scalefactor", 
                   label = h3("Scaled by:"), 
                   value = 1)
                            
      
    ),
    
    
    
    mainPanel(
        tabsetPanel(
          tabPanel("Forecast Graph", plotOutput("plot")),
          tabPanel("Model", h2(verbatimTextOutput("text"))),
          tabPanel("Forecasts", h2(tableOutput("table")))
        )
      
             )
  )))
