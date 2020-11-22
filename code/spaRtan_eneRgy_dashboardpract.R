if (!require(pacman)) install.packages("pacman") 
pacman::p_load(tidyverse,lubridate,shiny,shinydashboard,DT)

#detach("package:shiny.semantic",unload = T)
#detach("package:semantic.dashboard", unload = T)

# NOTES and TASKS:


## CURRENTLY APP IS RUNNING, BUT PLOT WILL NOT RENDER SINCE FILTER THINK NOT WORKING. PERHAPS NOT REDER PLOT, MAYBE RENDER DATASET OR SOMTHING idk!




# reading in the data

#combined_results <- combined_results <- read_csv("C:/Users/macia/Documents/MSIA-19/Git/Hackathon-Spa-R-tans/code/combined_results.csv", 
#                                                 col_types = cols(X1 = col_skip()))
combined_results <- read_csv("combined_results.csv", 
                             col_types = cols(X1 = col_skip()))


combined_results<-combined_results %>% mutate("Hour" = hour(combined_results$Datetime)) %>% select(-Datetime)
glimpse(combined_results)

#getwd()
#rm(combined_results2)
# Declare meter options

meter_choices = c()

for( i in unique(combined_results$better_label)){
  meter_choices = c(meter_choices,i)
}



str(meter_choices)
# Declare Time options

year_choices  <- unique(combined_results$Year)

# Month of the year (1-12)
month_choices <- unique(combined_results$Month)

# Day of the month (1-31)
day_month     <- unique(combined_results$`Day of the month`)

# Week of the year (1-53)
week_year    <- unique(combined_results$`Week of the year`)

# Day of the week (Sun - Sat)
day_week     <- unique(combined_results$`Day of the week`)


# Define UI

# What does the dashboard look like?

ui <- navbarPage("Spa-R-tans' Energy Consumption", #skin = "blue" , # find appropriate uncg color?
  
  ### HEADER ------------
  
  sidebarPanel(
    #User selects buildings to display
    box(selectInput("plotType", "Choose type of calculations", c(Sum = "sum", Mean = "mean"), selected = "mean")),
    ),
  
  #This panel only shows if the user selects mean on first tabl
  conditionalPanel(
    condition = "input.plotType == 'sum' && input.tabName == '1.1'",
    box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                   "Week of the year","Month"), selected = "Month")),
    box(plotOutput("meter_choice_plot3"), width = "auto"), 
  ),
  #This panel only shows if the user selects sum on first tab
  conditionalPanel(
    condition = "input.plotType == 'mean' && input.tabName == '1.1'",
    box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                   "Week of the year","Month"), selected = "Month")),
    box(plotOutput("meter_choice_plot3"), width = "auto"), 
  ),
  #This panel only shows if the user selects mean on second tab
  conditionalPanel(
    condition = "input.plotType == 'sum' && input.tabName == '1.1'",
    box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                   "Week of the year","Month"), selected = "Month")),
    box(plotOutput("meter_choice_plot3"), width = "auto"), 
  ),
  #This panel only shows if the user selects sum on second tab
  conditionalPanel(
    condition = "input.plotType == 'mean' && input.tabName == '1.1'",
    box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                   "Week of the year","Month"), selected = "Month")),
    box(plotOutput("meter_choice_plot3"), width = "auto"), 
  ),
  #This panel only shows if the user selects mean on third tab
  conditionalPanel(
    condition = "input.plotType == 'sum' && input.tabName == '1.1'",
    box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                   "Week of the year","Month"), selected = "Month")),
    box(plotOutput("meter_choice_plot3"), width = "auto"), 
  ),
  #This panel only shows if the user selects sum on third
  conditionalPanel(
    condition = "input.plotType == 'mean' && input.tabName == '1.1'",
    box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                   "Week of the year","Month"), selected = "Month")),
    box(plotOutput("meter_choice_plot3"), width = "auto"), 
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(title = "About", value = 1, helpText("Brief Summary of Project")),
      
      tabPanel(title = "1.1 Static Plot", value = 2, helpText("Needs to be reactive to several use inputs. Time on the horizonal axes, and energy consumption on the vertical axes.
                                                      Allow the user to chose 4 different units of time, within each, allow them to plot total consumption or average hourly consumption")),
      tabPanel(title = "1.2 Static Plot", value = 2, helpText("Needs to be reactive to several use inputs. Time on the horizonal axes, and energy consumption on the vertical axes.
                                                      Allow the user to chose 4 different units of time, within each, allow them to plot total consumption or average hourly consumption")),
      tabPanel(title = "2.0 Static Plot", value = 2, helpText("For everyplot in this section, user will be allowed to compare predicted values to acutal values"))
      

      )
    )
  )
  


server <- function(input,output){
  
  
  # Reeactive function to filter the dataset
  data_1 <- reactive({ # this is referenced in the ggplot. 
    data <- combined_results %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box) %>% # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      select("Actual","Predicted","Hour",input$time_choice_box,"better_label") %>% 
      gather(key = "Time_Choice","Time_Label",-c("Actual","Predicted","Hour","better_label")) %>% 
      group_by(better_label,Time_Label) %>%   # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual_per_Year" = mean(Actual), "Mean_Energy_Predicted_per_Year" = mean(Predicted))
    
    
    data
    
    
  })
  
  data_2 <- reactive({ # this is referenced in the ggplot. 
    data2 <- combined_results %>% group_by(Year,better_label) %>%  # grouping by year, so this will be a year plot, could ask them for input
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier?
      filter(better_label == input$meter_choice_box) %>% 
      # summarizeing the mean for actual and predicted
      select("Actual","Predicted","Hour",input$time_choice_box,"better_label") %>% 
      gather(key = "Time_Choice","Time_Label",-c("Actual","Predicted","Hour","better_label")) %>% 
      group_by(better_label,Time_Label) %>%   # grouping by year, so this will be a year plot, could ask them for input
      summarize("Sum_Energy_Actual_per_Year" = sum(Actual), "Sum_Energy_Predicted_per_Year" = sum(Predicted)) %>% 
      
    
    data2
  })
  
  # this plot will go the the "task_1" page :)
  #Average Actual and Annual Plot
  output$meter_choice_plot <- renderPlot({ # render a plot, the meter_choice_plot, which is found in task_2 tab
    
    ggplot(data_1(),aes(x = Time_Label)) + # ggplot, year on x axis
      geom_line(aes(y = Mean_Energy_Actual_per_Year, color = "darkred"),show.legend = F)+ # actual both on y axis
      geom_line(aes(y = Mean_Energy_Predicted_per_Year,color = "green"),show.legend = F)+ # predict
      theme_minimal()+ # random theme
      labs(title = paste("Energy for", input$meter_choice_box), subtitle = "subtitle here", caption = "Red line is Actual,Green is predicted")+
      ylab("Mean Energy")   # render labels
    
  })
  
  #Average Actual Annual Plot
  output$meter_choice_plot2 <- renderPlot({ # render a plot, the meter_choice_plot, which is found in task_1 tab
    
    ggplot(data_1(),aes(x = Time_Label)) + # ggplot, year on x axis
      geom_line(aes(y = Mean_Energy_Actual_per_Year, color = "darkred"),show.legend = F)+ # actual both on y axis
      theme_minimal()+ # random theme
      labs(title = paste("Energy for", input$meter_choice_box), subtitle = "subtitle here", caption = "Red line is Actual Energy Consumption")+
      ylab("Mean Energy")   # render labels
    
  })
  
  #Total Annual Plot
  output$meter_choice_plot3 <- renderPlot({ # render a plot, the meter_choice_plot, which is found in task_1 tab
    
    ggplot(data_2(),aes(x = Time_Label)) + # ggplot, year on x axis
      geom_line(aes(y = Sum_Energy_Actual_per_Year, color = "darkred"),show.legend = F)+ # actual both on y axis
      theme_minimal()+ # random theme
      labs(title = paste("Energy for", input$meter_choice_box), subtitle = "subtitle here", caption = "Red line is Actual Energy Consumption")+
      ylab("Total Energy")   # render labels
    
  })
}
  
  # output$thing <- renderDataTable({
  #   data_2 <- data_1() 
  #   
  #   data_2
  # })
  # 


shinyApp(ui = ui, server = server)


