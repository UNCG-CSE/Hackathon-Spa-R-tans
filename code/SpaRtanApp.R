#Example App Source: https://towardsdatascience.com/get-started-with-examples-of-reactivity-in-in-shiny-apps-db409079dd11
#install.packages("shiny")
library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)
library(lubridate)
#library(scales)
#library(plotly)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Energy Consumption Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create a customized graph of Energy Consumption 
                         with data collected from meter readings 
                         since 2015 on UNCG's Campus"),
      
      #Radio button for unit of time
      selectInput("time", 
                  
                  label = "Choose a unit of time to display",
                  
                  choices = c("hour", "day", "week", "month", "year"),
                  
                  selected = "year")
    ),
    #timeInput("time3", "Time:", value = strptime("12:34:56", "%T")),
    
    #dateRangeInput("dates", h3("Date range")))
    
    # dateInput("date", 
    #           h3("Date input"), 
    #           value = "2014-01-01"))
    
# Show a plot of the generated distribution
    mainPanel(

      textOutput("selected_time")
      
      # plotOutput(outputId, width = "100%", height = "400px", click = NULL,
      #            dblclick = NULL, hover = NULL, hoverDelay = NULL,
      #            hoverDelayType = NULL, brush = NULL, clickId = NULL,
      #            hoverId = NULL, inline = FALSE)
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Data load#
  combined_results <- combined_results <- read_csv("/cloud/project/Code/Data/Baseball_results.csv", 
                                                   col_types = cols(X1 = col_skip()))
  #Defining Reactives#
  #meteruserchoices <- array(unique(combined_results$building))
  #meteruserchoices
  
  output$selected_time <- renderText({
    
    paste("You have selected this", input$time)
  })
  
  output$selected_staticplot1 <- renderPlot({
    hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
         xlab = "Duration (minutes)", main = "Geyser eruption duration")

    dens <- density(faithful$eruptions, adjust = input$bw_adjust)
    lines(dens, col = "blue")
  })
  
  # output$selected_interactiveplot1 <- renderPlot({
  #   hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
  #        xlab = "Duration (minutes)", main = "Geyser eruption duration")
  #   
  #   dens <- density(faithful$eruptions, adjust = input$bw_adjust)
  #   lines(dens, col = "blue")
  # })
  
  
  #timeuserchoices <- lubridate format dmy_hms(Datetime)? example output 1 Jan 2017 23:59:59
  #aggregation type <- average- mean() or total mean()
  
  
}

# Run the application 
SparTanApp<-shinyApp(ui = ui, server = server)
SparTanApp