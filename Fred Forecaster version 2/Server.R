require(shiny)
require(forecast)
require(ggplot2)
require(fImport)
require(magrittr)
require(lubridate)
#require(xts)
source("helper.R")

shinyServer(function(input, output) {
  
    catigory.input <- reactive({
      input$class
    })
    
    output$picker <- renderUI({
      
      if(catigory.input() == "Labor Markets") {
      
        selectInput("data.type", label = h3("Fred Ticker"),
                                                        choices = c("NonAg Employment",
                                                                    "Unemployment Rate"),
                                                        selected = "NonAg Employment"
        )
      } else if (catigory.input() == "Gross Domestic Product"){
        selectInput("data.type", label = h3("Fred Ticker"),
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
      } else if(catigory.input() == "Inflation"){
        selectInput("data.type", label = h3("Fred Ticker"),
                                                       choices = c("CPI",
                                                                   "Core CPI",
                                                                   "Capacity Utilization",
                                                                   "Unit Labor Cost",
                                                                   "Nonfarm Business Sector"
                                                       ),
                                                       selected = "CPI"
                                           )
      } else if(catigory.input() == "Monetary Policy"){
        selectInput("data.type", label = h3("Fred Ticker"),
                                                       choices = c("Adjusted Monetary Base",
                                                                   "Excess Reserves",
                                                                   "M2 Money Supply",
                                                                   "Effective Federal Funds Rates"),
                                                       selected = "Adjusted Monetary Base"
        )
      } else {
        selectInput("data.type", label = h3("Fred Ticker"),
                                                       choices = c("Deficit",
                                                                   "Debt",
                                                                   "Expenditures",
                                                                   "Tax Revenues"),
                                                       selected = "Deficit"
                                           )
      }
    })
 
    indicator.type <- reactive({
                        switch(input$data.type,
                               "NonAg Employment" = "PAYEMS",
                               "Unemployment Rate" = "UNRATE",
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
                               "Capacity Utilization" = "TCU",
                               "Unit Labor Cost" = "ULCNFB",
                               "Nonfarm Business Sector" = "OPHNFB",
                               "Adjusted Monetary Base" = "AMBSL",
                               "Excess Reserves" = "EXCSRESNS",
                               "M2 Money Supply" = "M2SL",
                               "Effective Federal Funds Rate" = "FEDFUNDS",
                               "Debt" = "GFDEBTN",
                               "Expenditures" = "FGEXPND",
                               "Tax Revenues" = "FGRECPT"
                               )
      })
    
    
    fred.data <- reactive({
                            fredSeries(indicator.type(), from = input$date) %>%
                              applySeries(by = "monthly", FUN = mean)
                              #apply.monthly(FUN = mean)
                              
                          })
    
                  
    fred.compound <- reactive({
                                frequency(fred.data())
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
    
    ets.forecast <- reactive({
                          (fred.final() / input$scalefactor ) %>%
                          ets %>% forecast(h = input$horizon)
                          })
    
    
    output$text <- renderPrint({
      
      ets.forecast()$model
                                    
      })
    
    output$table <- renderTable({
      
     forecast.df <-  data.frame(as.Date(time(ets.forecast()$mean)),
                         ets.forecast()$lower[,2],
                         ets.forecast()$lower[, 1],
                         ets.forecast()$mean,
                         ets.forecast()$upper[, 1],
                         ets.forecast()$upper[, 2])
      
      names(forecast.df) <- c('time', 'lower95', 'lower80', 'forecast',
                              'upper80', 'upper95')
      
      forecast.df
      
    })
    

   output$plot <- renderPlot({
     
    forecast.df <-  data.frame(as.Date(time(ets.forecast()$mean)),
                        ets.forecast()$lower[,2],
                        ets.forecast()$lower[, 1],
                        ets.forecast()$mean,
                        ets.forecast()$upper[, 1],
                        ets.forecast()$upper[, 2])
    
    names(forecast.df) <- c('time', 'lower95', 'lower80', 'forecast',
                            'upper80', 'upper95')
    
    plot.data <- data.frame(as.Date(time(ets.forecast()$x)),
                            ets.forecast()$x,
                            ets.forecast()$fitted)
    names(plot.data) <- c("time", "values", "fitted")
    
    load("data\\recessions.RData")
    recessions <- subset(recessions, Start >= input$date)
    
    ggforecast(plot.data, forecast.df, input$smooth, input$date)
             

#    startDate <- as.Date(time(ets.forecast()$residuals))[1]
#    endDate <- as.Date(time(ets.forecast()$mean))[nrow(forecast$mean)]
#         
#     ggplot(data = plot.data) +
#       geom_rect(data = recessions, aes(xmin = Start, xmax = End, 
#                                        ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
#       geom_ribbon(data = forecast.df, fill = 'lightblue',
#                   aes(x = time, ymin = lower95, ymax = upper95)) +
#       geom_ribbon(data = forecast.df, fill = 'yellow',
#                   aes(x = time, ymin = lower80, ymax = upper80)) +
#       geom_line(data = forecast.df, aes(x = time, y = forecast), size = 1.0,
#                 colour = 'red') +
#       geom_point(aes(x = time, y = values), size = 1.0, color = "red") +
#       geom_line(aes(x = time, y = values), color = "blue") +
#       geom_smooth(aes(x = time, y = values), method = "loess", span = input$smooth,
#                   size = 0.50, color = "darkblue", fill = "springgreen4") +
#       scale_x_date("", limits = c(startDate, endDate))
      
    
 })
  
  }
)


