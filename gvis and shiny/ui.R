require(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Example 3: animated geo chart"),
  sidebarPanel(
    sliderInput("Year", "Election year to be displayed:", 
                min=1932, max=2012, value=2012,  step=4,
                format="###0",animate=TRUE)
  ),
  mainPanel(
    h3(textOutput("year")), 
    htmlOutput("gvis")
  )
)
)