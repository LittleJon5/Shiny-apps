library(shiny)
library(forecast)
library(ggplot2)
library(fImport)

shinyUI(fluidPage(
  titlePanel("Fred App"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data.type", label = h3("Radio buttons"),
                   choices = c("RSAFSNA",
                               "GDPC96"
                               ),
                   selected = "RSAFSNA"),
      
      dateInput("date", 
                label = h3("Date input"), 
                value = "1959-01-01")   
                            
      
    ),
    
    mainPanel(
                plotOutput("ts")
             )
  )
))



