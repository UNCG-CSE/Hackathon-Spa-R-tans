if (!require(pacman)) install.packages("pacman") 
pacman::p_load(tidyverse,lubridate,shiny,shinydashboard)



# NOTES and TASKS:


## CURRENTLY APP IS RUNNING, BUT PLOT WILL NOT RENDER SINCE FILTER THINK NOT WORKING. PERHAPS NOT REDER PLOT, MAYBE RENDER DATASET OR SOMTHING idk!




# reading in the data

# combined_results <- combined_results <- read_csv("C:/Users/macia/Documents/MSIA-19/Git/Hackathon-Spa-R-tans/code/combined_results.csv", 
#                                                  col_types = cols(X1 = col_skip()))
combined_results <- read_csv("combined_results.csv", 
                              col_types = cols(X1 = col_skip()))

glimpse(combined_results)

#Create User Options
## Declare meter options

meter_choices = c()
for (i in unique(combined_results$better_label)){
  meter_choices = c(meter_choices, i)
  }
str(meter_choices)

# Declare Time options
year_choices = c()
for (i in unique(combined_results$Year)){
  year_choices = c(year_choices, i)
}
str(year_choices)

# Month of the year (1-12)
month_choices=c()
for (i in unique(combined_results$Month)){
  month_choices = c(month_choices, i)
}
str(month_choices)

# Week of the year (1-53)
week_year_choices = c()

for (i in unique(week_year_choices$`Week of the year`)){
  week_year_choices = c(week_year_choices, i)
}
str(week_year_choices)

# Day of the week (Sun - Sat)
day_week_choices = c()
for (i in unique(combined_results$`Day of the week`)){
  day_week_choices = c(day_week_choices, i)
}
str(day_week_choices)

# Time of the Day (12:01AM - 11:59PM)
time_day_choices = c()
for (i in unique(combined_results$Datetime)){
  time_day_choices = c(time_day_choices, i)
}
str(time_day_choices)

time_unit = c("Annually", "Monthly", "Weekly", "Daily", "Hourly")
for (i in time_unit){
  time_unit = c(time_unit, i)
}
str(time_unit)

# explainprompt1 = c("Since 2015", "up to 12 months", "up to 12 weeks", "up to 31 days")
# for (i in explainprompt1){
#   explainprompt1 = c(explainprompt1, i)
# }
# str(explainprompt1)

# Define UI

# What does the dashboard look like?

ui <- dashboardPage( # find appropriate uncg color?
                    
                    ### HEADER ------------
                    
                    dashboardHeader(title = "Green Fund Hackathon: Energy Dashboard"), # Title of the dashboard
                    
                    ### SIDE BAR -----------
                    
                    dashboardSidebar( # declare a sidebar... hey R, theres a side bar!
                      sidebarMenu( # now what does the sidebar contain? let's fill it with menu items
                        
                        # First arg is what is seen on the dashboard, Second indicates to output where to show output
                        # icon is not needed, just adds a little icon. 
                        
                        menuItem("Task 1a: User Actual Static Plots", tabName = "task_1", icon = icon("dashboard")),
                        menuItem("Task 1b: User Predictive Static Plots", tabName = "task_1", icon = icon("dashboard")),
                        menuItem("Task 2: Analysis of Predictions",  tabName = "task_2", icon = icon("signal"))
                        
                      )
                    ),
                    
                    ### Body/Content ----------
                    
                    dashboardBody(
                      tabItems(
                        tabItem("task_1",# this links back to the sidebar item :) 
                                  h1("Insert text here, this is a heading one"), ## adding simple text...
                                  h3("heading 3... try h4, h2 etc..."),
                                  h3("now let's try to plot something"),
                                  # Creating a visual box for user input
                                  # First arg is what renderplot will use, second is what shows on the dashbooard
                                  # third are the designated choices
                                  box(selectInput("meter_choicebox","Choose meter to display", meter_choices, selected = "Elliott University Center (040) - Main Meter")),
                                  box(selectInput("time_choicebox","How would you like to display your data?",time_unit, selected = "Annually")),
                                  box(plotOutput("AnnualAverageBuildingEnergyConsumptionPlot"), width = "auto"),
                                  box(plotOutput("AnnualTotalBuildingEnergyConsumptionPlot"), width = "400")
                                  )
                                ),
                        # tabItem("task_1b",# this links back to the sidebar item :) 
                        #           h1("Insert text here, this is a heading one"), ## adding simple text...
                        #           h3("heading 3... try h4, h2 etc..."),
                        #           h3("now let's try to plot something"),
                        #           # Creating a visual box for user input
                        #           # First arg is what renderplot will use, second is what shows on the dashbooard
                        #           # third are the designated choices
                        #           box(selectInput("meter_choicebox","Choose meter to display", meter_choices, selected = "Elliott University Center (040) - Main Meter")),
                        #           box(selectInput("time_choicebox","How would you like to display your data?",time_unit)),
                        #           box(helpText("explainprompt1", "Note", explainprompt1)),
                        #           #box(plotOutput("PredictedAnnualAverageBuildingEnergyConsumptionPlot"), width = "400"),
                        #           #box(plotOutput("PredictedAnnualTotalBuildingEnergyConsumptionPlot"), width = "400")
                        #           
                        #         
                        # ),
                        
                      
                      tabItem("task_2", # leaving this page empty for now
                                h1("this is an empty page"),
                                box(selectInput("meter_choicebox","Choose meter to display", meter_choices, selected = "Elliott University Center (040) - Main Meter")),
                                box(selectInput("time_choicebox","How would you like to display your data?", time_unit, selected = "Annually")),
                                box(plotOutput("AnnualAverageBuildingEnergyConsumptionPlot"), width = "400"),
                                box(plotOutput("AnnualTotalBuildingEnergyConsumptionPlot"), width = "400")
                                
                              )
                      )
                    )
                      

server <- function(input, output, session){
  # observe({
  #   x <-input$time_unit
  #   
  #   if(is.null(x))
  #     x<- character(0)
  #   
  #   #Set label and select times
  #   
  #   updateSelectInput(session, "time_choicebox",
  #                     label = "explainprompt",
  #                     choices = x)
  # })
  output$time <- renderText({paste("You selected:",input$time_unit)
    
    })
  
 
  #This first reactive table calulates the sums of the energy consumed in buildings selected by user over time period selected by user
  ReactiveAnnualAverageTable <- reactive({ # this is referenced in the ggplot. 
    TotalAnnualBuildingResults <- combined_results %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier?
      filter(better_label == input$meter_choice_box) %>% 
      mutate("Usertime" = case_when(input$time_choicebox =="Annually"~ year(Datetime), 
                                  input$time_choicebox =="Monthly"~ as.Date(Datetime, format = "%m%Y"),
                                  input$time_choicebox =="Weekly"~ isoweek(Datetime),
                                  input$time_choicebox =="Daily"~ wday(Datetime),
                                  input$time_choicebox =="Hourly"~ hm(Datetime))) %>% 
      select("better_label", "Usertime", "Actual", "Predicted") %>% 
      group_by(better_label, Usertime) %>%  # grouping by Datetime, so this will be a user-input-based plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual_per_Year" = mean(Actual), "Mean_Energy_Predicted_per_Year" = mean(Predicted))
    TotalAnnualBuildingResults
  })
  #This second reactive table calulates the averages of the energy consumed in buildings selected by user over time period selected by user
  ReactiveAnnualTotalTable <- reactive({ # this is referenced in the ggplot. 
    AnnualAverageBuildingResults <- combined_results %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier?
      filter(better_label == input$meter_choice_box) %>% 
      mutate("Usertime" = case_when(input$time_choicebox =="Annually"~ year(Datetime), 
                                    input$time_choicebox =="Monthly"~ as.Date(Datetime, format = "%m%Y"),
                                    input$time_choicebox =="Weekly"~ isoweek(Datetime),
                                    input$time_choicebox =="Daily"~ wday(Datetime),
                                    input$time_choicebox =="Hourly"~ hm(Datetime))) %>% 
      select("better_label", "Usertime", "Actual", "Predicted") %>% 
      group_by(better_label, Usertime)  # grouping by Datetime, so this will be a user-input-based plot, could ask them for input) %>%  # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual_per_Year" = sum(Actual), "Mean_Energy_Predicted_per_Year" = sum(Predicted))
      AnnualAverageBuildingResults
    })
    
 
  # this Average plot will go the the "task_1" page :)
  output$AnnualAverageBuildingEnergyConsumptionPlot <- renderPlot({
    ggplot(ReactiveAnnualAverageTable(), aes(x = Usertime))+
      geom_line(aes(y = ActualBuildingTotal, color = better_label), position = position_dodge())+
      theme(legend.position = 'none')+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))
    })
  
  # this sum plot will go the the "task_1" page :)
  output$AnnualTotalBuildingEnergyConsumptionPlot <- renderPlot({
    ggplot(ReactiveAnnualTotalTable(), aes(x = Usertime))+
      geom_line(aes(y = ActualBuildingTotal, color = better_label), position = position_dodge())+
      theme(legend.position = 'none')+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))
    })
  
  }
  
# output$thing1 <- renderDataTable({
#   
#   ReactiveAnnualAverageTable()
#   
#   
# })
# output$thing2 <- renderDataTable({
#   ReactiveAnnualTotalTable()
#   
#   
#   
# })
  

shinyApp(ui = ui, server = server)

