library(shiny)
library(forecast)
library(ggplot2)
library(fImport)

shinyUI(fluidPage(
  
  includeCSS("styles.css"),
  
  titlePanel(h1("Macro Economic Indicators")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("class", label = h3("Indicator Catigory"),
                  choices = c("Gross Domestic Product",
                              "Labor Markets",
                              "Inflation",
                              "Monetary Policy",
                              "Fiscal Policy"
                  ),
                  selected = "Gross Domestic Product"
      ),
      
      uiOutput("picker"),
      
#       conditionalPanel("input.class == 'Gross Domestic Product'",
#                        selectInput("data.type", label = h3("Fred Ticker"),
#                                    choices = c("GDP",
#                                                "Housing Starts",
#                                                "Industrial Production",
#                                                "ISM Index",
#                                                "Retail Sales Seasonally Adjusted",
#                                                "Retail Sales Not Seasonally Adjusted",
#                                                "Yield Curve Slope",
#                                                "Leading Indicators"),
#                                    selected = "GDP"
#                        )
#       ),
#       conditionalPanel("input.class == 'Labor Markets'",
#                        selectInput("data.type", label = h3("Fred Ticker"),
#                                    choices = c("NonAg Employment",
#                                                "Unemployment Rate"),
#                                    selected = "NonAg Employment"
#                        )
#       ),
#       conditionalPanel("input.class == 'Inflation'",
#                        selectInput("data.type", label = h3("Fred Ticker"),
#                                    choices = c("CPI",
#                                                "Core CPI",
#                                                "Capacity Utilization",
#                                                "Unit Labor Cost",
#                                                "Nonfarm Business Sector"
#                                    ),
#                                    selected = "CPI"
#                        )
#       ),
#       
#       conditionalPanel("input.class == 'Monetary Policy'",
#                        selectInput("data.type", label = h3("Fred Ticker"),
#                                    choices = c("Adjusted Monetary Base",
#                                                "Excess Reserves",
#                                                "M2 Money Supply",
#                                                "Effective Federal Funds Rates"),
#                                    selected = "Adjusted Monetary Base"
#                        )
#       ),
#       conditionalPanel("input.class == 'Fiscal Policy'",
#                        selectInput("data.type", label = h3("Fred Ticker"),
#                                    choices = c("Deficit",
#                                                "Debt",
#                                                "Expenditures",
#                                                "Tax Revenues"),
#                                    selected = "Deficit"
#                        )
#       ),
      
#       selectInput("data.type", label = h3("Fred Ticker"),
#                    choices = c("GDP",
#                                "Housing Starts",
#                                "Industrial Production",
#                                "ISM Index",
#                                "Retail Sales Seasonally Adjusted",
#                                "Retail Sales Not Seasonally Adjusted",
#                                "Yield Curve Slope",
#                                "Leading Indicators",
#                                "NonAg Employment",
#                                "Unemployment Rate",
#                                "CPI",
#                                "Core CPI",
#                                "Capacity Utilization",
#                                "Unit Labor Cost",
#                                "Nonfarm Business Sector",
#                                "Adjusted Monetary Base",
#                                "Excess Reserves",
#                                "M2 Money Supply",
#                                "Effective Federal Funds Rate",
#                                "Deficit",
#                                "Debt",
#                                "Expenditures",
#                                "Tax Revenues"
#                                ),
#                    selected = "GDP"),
      
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
          tabPanel("Forecast Data", h2(tableOutput("table")))
        )
      
             )
  )))
