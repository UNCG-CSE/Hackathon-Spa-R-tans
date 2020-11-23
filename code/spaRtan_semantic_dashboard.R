if (!require(pacman)) install.packages("pacman") 
pacman::p_load(tidyverse,lubridate,shiny,shinydashboard,DT)

#detach("package:shiny.semantic",unload = T)
#detach("package:semantic.dashboard", unload = T)

# NOTES and TASKS:


## CURRENTLY APP IS RUNNING, BUT PLOT WILL NOT RENDER SINCE FILTER THINK NOT WORKING. PERHAPS NOT REDER PLOT, MAYBE RENDER DATASET OR SOMTHING idk!




# reading in the data

#combined_results <- combined_results <- read_csv("C:/Users/macia/Documents/MSIA-19/Git/Hackathon-Spa-R-tans/code/combined_results.csv", 
#                                                 col_types = cols(X1 = col_skip()))

combined_results <- read_csv("D:/Hackathon/Hackathon/combined_results.csv", 
                             col_types = cols(X1 = col_skip()))


combined_results<-combined_results %>% mutate("Hour" = hour(Datetime))
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

start_date<-min(combined_results$Datetime)
end_date<-max(combined_results$Datetime)

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
      menuItem("Task 1: ", tabName = "task_1",icon = icon("signal")),
      menuItem("Task 2: What are we supposed to do?", tabName = "task_2", icon = icon("dashboard")),
      menuItem("Datatable: View some data",  tabName = "Data_Table", icon = icon("signal"))
      
    )
  ),
  
  ### Body/Content ----------
  
  dashboardBody(
    tabItems(
      tabItem("task_1",
              h1("Static plot of energy consumption"),
              box(selectInput("meter_choice_box_1","Choose Building",meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                  selectInput("agg_level", "Choose sum or mean energy",
                              c("Mean_Energy_Actual","Mean_Energy_Predicted",
                                "Total_Energy_Actual", "Total_Energy_Predicted"), selected = "Total_Energy_Predicted"),
                  wellPanel(
                  dateRangeInput("daterange_1", "Filter by date", start = as.Date(start_date), end = as.Date(end_date))),
                  selectInput("time_choice_box_1", "Choose Time aggredation" , c("year",
                                                                                 "month"),
                              selected = "month")),
              box(plotOutput("task_1.1_plot"),width = "auto")),
      
    tabItem("task_2",# this links back to the sidebar item :) 
            
            h1("Create a real-time interactive plot of energy consumption and prediction"), ## adding simple text...
            h3("1.1 Static Plot"),
            h4("Needs to be reactive to several use inputs. Time on the horizonal axes, and energy consumption on the vertical axes.
                             Allow the user to chose 4 different units of time, within each, allow them to plot total consumption or average hourly consumption"),
            
            h3("1.2 Predictive plot"),
            h4("For everyplot above, allow user to plot predicted values"),
            
            # Creating a visual box for user input
            # First arg is what renderplot will use, second is what shows on the dashbooard
            # third are the designated choices
            
            box(selectInput("time_choice_box_3","Choose time aggregation", c("Day of the month",
                                                                             "Week of the year","Month"), selected = "Month")),
            box(selectInput("meter_choice_box_3","Choose builing to display", meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T)),
            
            box(plotOutput("meter_choice_plot_3"), width = "auto")),
    
    tabItem("Data_Table", # leaving this page empty for now
            
            h1("this is an empty page"),
            box(selectInput("meter_choice_box_4","Choose builing to display", meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T)),
            DT::dataTableOutput("thing")
            
    )
  )
))

server <- function(input,output){
  
  
  # Reeactive function to filter the dataset
  data_1 <- reactive({ # this is referenced in the ggplot. 
    data <- combined_results %>% filter(better_label == input$meter_choice_box_3,
                                        Datetime >= input$daterange_1[1] & Datetime <= input$daterange_1[2]) %>% 
      select("Actual","Predicted","Hour",input$time_choice_box_3,"better_label") %>% 
      gather(key = "Time_Choice","Time_Label",-c("Actual","Predicted","Hour","better_label")) %>% 
      group_by(better_label,Time_Label) %>%   # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Actual))
    # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
    
    data
    
    
  })
  data_1.1 <- reactive({ # this is referenced in the ggplot. 
    data <- combined_results %>% filter(better_label == input$meter_choice_box_1) %>% 
      select("Actual","Predicted","Hour","Datetime","better_label") %>% 
      mutate("Time_Choice" = case_when(input$time_choice_box_1 == "year" ~ year(Datetime),
                                       input$time_choice_box_1 == "month" ~ month(Datetime, label = T))) %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
    
    data
    
    
  })
  
  data_2 <- reactive({ # this is referenced in the ggplot. 
    data2 <- combined_results %>% filter(better_label == input$meter_choice_box_4) %>% 
      group_by(Year,better_label) %>%  # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" = sum(Predicted))
    # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier?
    data2
  })
  
  
  output$task_1.1_plot <- renderPlot({
    ggplot(data_1.1(),aes(x = Time_Choice, color = better_label)) + # ggplot, year on x axis
      geom_line(aes_string(y = input$agg_level),show.legend = T)+
      theme_minimal()+ # random theme
      labs(title = "Energy consumtion for selected building/buildings", subtitle = paste0("Buildings: ", input$meter_choice_box_1),
           caption = "Red line is Actual, Green is predicted",color = "Building/Meter")+
      ylab(if_else(input$agg_level == "Mean_Energy_Actual","Mean Energy Used",
                   if_else(input$agg_level == "Mean_Energy_Predicted","Mean Energy Predicted",
                           if_else(input$agg_level == "Total_Energy_Actual","Total Energy Used",
                                   if_else(input$agg_level == "Total_Energy_Predicted","Total Energy Predicted","Nothing")))))
  })
  
  # this plot will go the the "task_1" page :)
  
  output$meter_choice_plot_3 <- renderPlot({ # render a plot, the meter_choice_plot, which is found in task_1 tab
    
    ggplot(data_1(),aes(x = Time_Label, color = better_label)) + # ggplot, year on x axis
      geom_line(aes(y = `Mean_Energy_Actual`),show.legend = T)+ # actual both on y axis
      geom_line(aes(y = `Mean_Energy_Predicted`), linetype = "dashed",show.legend = T)+ # predict
      theme_minimal()+ # random theme
      labs(title = paste("Energy for", input$meter_choice_box_3), subtitle = "subtitle here", caption = "Red line is Actual, Green is predicted")+
      ylab("Mean Energy")   # render labels
    
  })
  
  output$thing <- renderDataTable({
    data_2 <- data_2() 
    
    data_2
  })
  
}

shinyApp(ui = ui, server = server)
