require(shiny)
require(forecast)
require(ggplot2)
require(fImport)
require(magrittr)
require(lubridate)
source("helper.R")

shinyServer(function(input, output) {
  
##################
#------------------------------- Indicator category
# This recieves to the Indicator and saves it as category.input
# for later use
                                        
    category.input <- reactive({
      input$class
    })
    
#######################
# ---------------------------- Conditional Panel Section
# This is where we'll use category.input to make conditional pannels.
# This segment outputs a series of conditional pannels to the ui.
# It displays different options for data.type depending on what is selected
# for Indicator category
    
    output$picker <- renderUI({
      
      if(category.input() == "Labor Markets") {
      
        selectInput("data.type", label = h4("Indicator"),
                                                        choices = c("NonAg Employment",
                                                                    "Unemployment Rate",
                                                                    "Average Work Week",
                                                                    "Average Hourly Earning"),
                                                        selected = "NonAg Employment"
        )
      } else if (category.input() == "Economic Growth"){
        selectInput("data.type", label = h4("Indicator"),
                                                       choices = c("GDP",
                                                                   "Housing Starts",
                                                                   "Industrial Production",
                                                                   "ISM Index",
                                                                   "Retail Sales Seasonally Adjusted",
                                                                   "Retail Sales Not Seasonally Adjusted",
                                                                   "Yield Curve Slope",
                                                                   "Leading Indicators"),
                                                       selected = "GDP"
                                           )
      } else if(category.input() == "Inflation"){
        selectInput("data.type", label = h4("Indicator"),
                                                       choices = c("CPI",
                                                                   "Core CPI",
                                                                   "PCE",
                                                                   "Core PCE",
                                                                   "Capacity Utilization",
                                                                   "Unit Labor Cost",
                                                                   "Nonfarm Business Sector"
                                                       ),
                                                       selected = "CPI"
                                           )
      } else if(category.input() == "Monetary Policy"){
        selectInput("data.type", label = h4("Indicator"),
                                                       choices = c("Adjusted Monetary Base",
                                                                   "Excess Reserves",
                                                                   "M2 Money Supply",
                                                                   "Effective Federal Funds Rate"),
                                                       selected = "Adjusted Monetary Base"
        )
      } else {
        selectInput("data.type", label = h4("Indicator"),
                                                       choices = c(
                                                                   "Deficit",
                                                                   "Debt",
                                                                   "Expenditures",
                                                                   "Tax Revenues"),
                                                       selected = "Debt"
                                           )
      }
    })
    
    
####################
    # The next part of this converts the user input for data.type
    # and turns into the associated fred ticker symbol
    ###############
 
    indicator.type <- eventReactive(input$getData, {
      
                        switch(input$data.type,
                               "NonAg Employment" = "PAYEMS",
                               "Unemployment Rate" = "UNRATE",
                               "Average Work Week" = "AWHAETP",
                               "Average Hourly Earning" = "CES0500000003",
                               "GDP" = "GDPC96",
                               "Housing Starts" = "HOUST",
                               "Industrial Production" = "INDPRO",
                               "ISM Index" = "NAPM",
                               "Retail Sales Seasonally Adjusted" = "RSAFS",
                               "Retail Sales Not Seasonally Adjusted" = "RSAFSNA",
                               "Yield Curve Slope" = "T10Y2Y",
                               "Leading Indicators" = "USSLIND",
                               "CPI" = "CPIAUCSL",
                               "Core CPI" = "CPILFESL",
                               "PCE" = "PCEPI",
                               "Core PCE" = "PCEPILFE",
                               "Capacity Utilization" = "TCU",
                               "Unit Labor Cost" = "ULCNFB",
                               "Nonfarm Business Sector" = "OPHNFB",
                               "Adjusted Monetary Base" = "AMBSL",
                               "Excess Reserves" = "EXCSRESNS",
                               "M2 Money Supply" = "M2SL",
                               "Effective Federal Funds Rate" = "FEDFUNDS",
                               "Deficit" = "FYFSD",
                               "Debt" = "GFDEBTN",
                               "Expenditures" = "FGEXPND",
                               "Tax Revenues" = "FGRECPT"
                               )
      })
  
###########################
    # This part of the server gets the data from FRED. It uses the 
    # indicator.type() to indicate which fred series I'll pull
    #######################
    
    fred.data <- reactive({
                            fredSeries(indicator.type(), from = input$date) %>%
                              applySeries(by = "monthly", FUN = mean)
                              
                          })
  
##############################
    # This part of the is what we used to dermine the
    # the compound frequncy of the fredSeries
    # this will come into play in the next part of the server
    # we need this information for some of the manipulation functions
    ##########################
    
                  
    fred.compound <- reactive({
                                frequency(fred.data())
                              })
#######################
    # This function calls on functions located in the helper script that
    # change the time series. 
    ###################
    
    fred.final <- reactive({
                            switch(input$manipulate,
                                   "No Transformation" = fred.data(),
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
######################
    # The next function takes the final time series and scales it
    # next it makes an ets model
    # then if forecast the models out by how ever far the user desires
    ##################
    
    ets.forecast <- reactive({
                          (fred.final()/ as.numeric(input$scalefactor)) %>%
                          ets %>% forecast(h = input$horizon, model = "ZZN")
                          })

######################
    # This creates an arima model on the same data
    # using the same basic syntax as the ets model
    ##################
    
    arima.forecast <- reactive({
      (fred.final() / as.numeric(input$scalefactor)) %>% 
        auto.arima(seasonal = FALSE) %>% 
        forecast(h = input$horizon)
    })

        
########################
    # this part out puts the model paramerter the model table on the ui
    # ETS Method Output
    #####################
    
    output$text <- renderPrint({
      
      ets.forecast()$model$method
                                    
      })
#########################
    # ETS Smoothing Parameters
    ################
    
    output$text2 <- renderPrint({
      
      ets.forecast()$model$par
      
    })
    
##########################
    # ETS Final States 
    ####################
    
    output$text3 <- renderPrint({
      
      nrow.val <- nrow(ets.forecast()$model$states)
      
      ets.forecast()$model$states[nrow.val, ]
      
    })
    
############################
    # Arima Model
    ########################
    
    output$text4 <- renderPrint({
      
      arima.forecast()$method
      
    })
    
#########################
    # ETS Smoothing Parameters
    ################
    
    output$text5 <- renderPrint({
      
      
      arima.forecast()$model$coef
      
    })
    
##########################
    # ARIMA Final States 
    ####################
    
#     output$text6 <- renderPrint({
#       
#       nrow.val <- nrow(arima.forecast()$model$states)
#       
#       arima.forecast()$model$states[nrow.val, ]
#       
#     })
    
#############################
    # This displays the forecast information
    # it calls the forecast.frame function from the helper script
    # This is for the ETS Forecast
    #########################
    
    output$table <- renderTable({
      
      validate(
        need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button,
             when this message disappears your forecast is on its way.")
      )
      
      forecast.frame(ets.forecast())
      
    })
    
######################
    # Arima Table
    ###################
    
    output$arimaTable <- renderTable({
      
      forecast.frame(arima.forecast())
      
    })
    
    
    
#######################
    # This calls three functions from helper.r to get the two data
    # required to use in the plot function
    # #################
    
   output$plot <- renderPlot({
    
    validate(
      need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button.
           When this message disappears your forecast is on its way.")
    )
     
    forecast.df <- forecast.plot.frame(ets.forecast())
 
    plot.data <- past.data(ets.forecast())
    
    ggforecast(plot.data, forecast.df, input$smooth, input$date)
 })
   
##########################3
   # Arima Plot output segment
   ##################
   
   output$arimaPlot <- renderPlot({
     
     validate(
       need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button.
            When this message disappears your forecast is on its way.")
       )
     
     forecast.df <- forecast.plot.frame(arima.forecast())
     
     plot.data <- past.data(arima.forecast())
     
     ggforecast(plot.data, forecast.df, input$smooth, input$date)
     
   })
   
  
  
  }
)


