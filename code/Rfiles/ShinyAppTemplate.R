#Source: http://rstudio.github.io/shinydashboard/get_started.html
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Energy Consumption"),
  dashboardSidebar(),
  dashboardBody(
    #Arranging boxes in rows (or columns)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
      
    )
    
  )
  
)

server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <-renderPlot({
    data <- histdata[se1_len(input$slider)]
    hist(data)
    
  })  
  
}

shinyApp(ui, server)
