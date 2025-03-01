library(shiny)
library(forecast)
library(ggplot2)
library(fImport)

shinyUI(fluidPage(
# ------------------------- This loads the css file for apps style
  includeCSS("styles.css"),
  
  titlePanel(h1("US Macroeconomic Situation and Forecast" )),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText(h3("Graphs, Models, and Forecasts")),
      
      selectInput("class", label = h4("Goals and Policy"),
                  choices = c("Economic Growth",
                              "Labor Markets",
                              "Inflation",
                              "Monetary Policy",
                              "Fiscal Policy"
                  ),
                  selected = "Economic Growth"
      ),
      
      uiOutput("picker"),
      
      actionButton("getData", "Forecast"),
      
      helpText(h3("Options")),
      
      selectInput("manipulate", label = h4("Transformations"),
                  choices = c("Original Units",
                              "Change",
                              "Change From a Year Ago",
                              "Percent Change",
                              "Percent Change from a Year Ago",
                              "Compounded Annual Rate of Change",
                              "Continuously Compounded Rate of Change",
                              "Countinuously Compounded Annual Rate of Change",
                              "Natural Log"),
                  selected = "No Transformation"),
      
      selectInput("scalefactor", 
                  label = h4("Scale Factor"), 
                  c("No Change" = 1,
                    "Thousands" = 1000,
                    "Millions" = 1000000,
                    "Billions" = 1000000000),
                  selected = "No change"),
      
      sliderInput("smooth",
                  label = h4("Smoothing Parameter"),
                  min=.05,
                  max=.5,
                  value= .21,
                  animate = TRUE),
      
      numericInput("horizon", 
                   label = h4("Forecast Horizon"), 
                   value = 12), 
      
      dateInput("date", 
                label = h4("Initial Date (YYYY-MM-DD)"), 
                value = "1959-01-01")
    ),
    
    
    
    mainPanel(
        tabsetPanel(
          tabPanel("Forecast Graph",
                   helpText(h5("ETS Forecast")),
                   h5(plotOutput("plot")),
                   helpText(h5("Arima Forecast")),
                   h5(plotOutput("arimaPlot"))
                   ),
          tabPanel("Model",
                   helpText(h5("ETS Model")),
                   h2(verbatimTextOutput("text")),
                   helpText(h5("ETS Transition Equation:")),
                   helpText(h5("Smoothing Constants and Estimated Parameters")),
                   h2(verbatimTextOutput("text2")),
                   helpText(h5("ARIMA Model")),
                   h2(verbatimTextOutput("text3")),
                   helpText(h5("ARIMA Estimated Parameters")),
                   h2(verbatimTextOutput("text4"))
                   ),
          tabPanel("Forecasts",
                   helpText(h5("Forecasts")),
                   h5(tableOutput("table"))
#                    helpText(h5("ARIMA Forecast")),
#                    h5(tableOutput("arimaTable"))
        )
      
             )
  ))))
