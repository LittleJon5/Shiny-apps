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
    
    fred.data <- reactive({
      
                      file <- input$data
                      readWorksheetFromFile(file$datapath, sheet=1, header = TRUE, startRow = 7) %>% as.timeSeries
                          })
    
    fred.final <- reactive({
      switch(input$manipulate,
             "No Transfromation" = fred.data(),
             "Change" = fred.data() %>% chg,
             "Change From a Year Ago" = fred.data() %>% ch1(n_obs_per_year = fred.compound()),
             "Percent Change" = fred.data() %>% pch,
             "Percent Change from a Year Ago" = fred.data() %>% pc1(n_obs_per_year = fred.compound()),
             "Compounded Annual Rate of Change" = fred.data() %>% pca(n_obs_per_year = fred.compound()),
             "Continuously Compounded Rate of Change" = fred.data() %>% cch,
             "Countinuously Compounded Annual Rate of Change" = fred.data() %>% cca(n_obs_per_year = fred.compound()),
             "Natural Log" = fred.data() %>% log
      )   
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
    
    output$text <- renderPrint({
      
      ets.forecast()$model
      
    })
    
    #############################
    # This displays the forecast information
    # it calls the forecast.frame function from the helper script
    #########################
    
    output$table <- renderTable({
      
      validate(
        need(input$data, "Please upload the file you wish to forecast")
      )
      
      forecast.df <- forecast.frame(ets.forecast())
      
      forecast.df
      
    })
    
    #######################
    # This calls three functions from helper.r to get the two data
    # required to use in the plot function
    # #################
    
    output$plot <- renderPlot({
      
      validate(
        need(input$data, "Please upload the file you wish to forecast")
      )
    
      
      forecast.df <- forecast.plot.frame(ets.forecast())
      
      plot.data <- past.data(ets.forecast())
      
      ggforecast(plot.data, forecast.df, input$smooth, input$date)
    })
    
  
  }
)


