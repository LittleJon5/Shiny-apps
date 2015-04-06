require(shiny)
require(forecast)
require(ggplot2)
require(fImport)
require(XLConnect)
require(magrittr)
require(lubridate)
source("helper.R")

shinyServer(function(input, output) {
  
###########################
    # This part of the server gets the data from FRED. It uses the 
    # indicator.type() to indicate which fred series I'll pull
    #######################
    
    fred.final <- reactive({
                      file <- input$data
                      readWorksheetFromFile(file$datapath, sheet=1, header = TRUE, startRow = 7) %>% as.timeSeries
                          })
  
##############################
    # This part of the is what we used to dermine the
    # the compound frequncy of the fredSeries
    # this will come into play in the next part of the server
    # we need this information for some of the manipulation functions
    ################################
    ets.forecast <- reactive({
                          (fred.final() / input$scalefactor ) %>%
                          ets %>% forecast(h = input$horizon)
                          })
########################
    # this part out puts the model paramerter the model table on the ui
    #####################
    
   output$plot <- renderPlot({
     
    ets.forecast() %>% plot.forecast
 })
  
  }
)


