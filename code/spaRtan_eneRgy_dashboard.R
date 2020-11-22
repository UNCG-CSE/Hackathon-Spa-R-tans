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

ui <- dashboardPage(#skin = "blue" , # find appropriate uncg color?
                    
                    ### HEADER ------------
                    
                    dashboardHeader(title = "Green Fund Hackathon: Energy Dashboard"), # Title of the dashboard
                    
                    ### SIDE BAR -----------
                    
                    dashboardSidebar( # declare a sidebar... hey R, theres a side bar!
                      sidebarMenu( # now what does the sidebar contain? let's fill it with menu items
                        
                        # First arg is what is seen on the dashboard, Second indicates to output where to show output
                        # icon is not needed, just adds a little icon. 
                        
                        menuItem("Task 1: What are we supposed to do?", tabName = "task_1", icon = icon("dashboard")),
                        menuItem("Task 2: hmmmm... I forgot",  tabName = "task_2", icon = icon("signal"))
                        
                      )
                    ),
                    
                    ### Body/Content ----------
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "task_1",# this links back to the sidebar item :) 
                                
                          h1("Create a real-time interactive plot of energy consumption and prediction"), ## adding simple text...
                          h3("1.1 Static Plot"),
                          h4("Needs to be reactive to several use inputs. Time on the horizonal axes, and energy consumption on the vertical axes.
                             Allow the user to chose 4 different units of time, within each, allow them to plot total consumption or average hourly consumption"),
                          sidebarPanel(
                          #User selects buildings to display
                          box(selectInput("plotType", "Choose type of calculations", c(Sum = "sum", Mean = "mean"), selected = "mean")),
                          ),
                          
                          #This panel only shows if the user selects mean
                          conditionalPanel(
                            condition = "input.plotType == 'mean'",
                            box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                                           "Week of the year","Month"), selected = "Month")),
                            box(plotOutput("meter_choice_plot2"), width = "auto"), 
                          ),
                          
                          #This panel only shows if the user selects sum
                          conditionalPanel(
                            condition = "input.plotType == 'sum'",
                            box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                                           "Week of the year","Month"), selected = "Month")),
                            box(plotOutput("meter_choice_plot3"), width = "auto"), 
                          ),
                          
                          box(selectInput("meter_choice_box","Choose builing to display",meter_choices, selected = "Elliott University Center (040) - Main Meter")),
                          #User selcts type of computations to be made: average or total energy consumption
                          h3("1.2 Predictive plot"),
                          h4("For everyplot above, allow user to plot predicted values"),
                          
                           # Creating a visual box for user input
                          # First arg is what renderplot will use, second is what shows on the dashbooard
                          # third are the designated choices
                         
                         box(selectInput("time_choice_box","Choose time aggregation", c("Hour of the day","Day of the week",
                                                                                         "Week of the year","Month"), selected = "Month")),
                         
                         box(plotOutput("meter_choice_plot"), width = "auto") 
                        
                      ),
                      
                      tabItem(tabName = "task_2", # leaving this page empty for now
                              
                              h1("this is an empty page"),
                              box(selectInput("meter_choice_box","Choose builing to display",meter_choices, selected = "Elliott University Center (040) - Main Meter")),
                              DT::dataTableOutput("thing")
                              
                              )
                    )
                  ))

server <- function(input,output){
  
  
  # Reeactive function to filter the dataset
  data_1 <- reactive({ # this is referenced in the ggplot. 
    data <- combined_results %>% filter(better_label == input$meter_choice_box) %>% 
    select("Actual","Predicted","Hour",input$time_choice_box,"better_label") %>% 
    gather(key = "Time_Choice","Time_Label",-c("Actual","Predicted","Hour","better_label")) %>% 
    group_by(better_label,Time_Label) %>%   # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
    summarize("Mean_Energy_Actual_per_Year" = mean(Actual), "Mean_Energy_Predicted_per_Year" = mean(Predicted))
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
    
    data
    
    
  })
    
  data_2 <- reactive({ # this is referenced in the ggplot. 
    data2 <- combined_results %>% group_by(Year,better_label) %>%  # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Sum_Energy_Actual_per_Year" = sum(Actual), "Sum_Energy_Predicted_per_Year" = sum(Predicted)) %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier?
      filter(better_label == input$meter_choice_box)
    
    data2
  })
  
  # this plot will go the the "task_1" page :)
  #Average Actual and Annual Plot
  output$meter_choice_plot <- renderPlot({ # render a plot, the meter_choice_plot, which is found in task_1 tab
       
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
      ylab("Mean Energy")   # render labels
    
  })
  output$thing <- renderDataTable({
    data_2 <- data_1() 
    
    data_2
  })
  
  }

shinyApp(ui = ui, server = server)


