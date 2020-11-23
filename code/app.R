if (!require(pacman)) install.packages("pacman") 
pacman::p_load(tidyverse,lubridate,shiny,shinydashboard)



# NOTES and TASKS:


# reading in the data

combined_results <- read_csv("D:/Hackathon/Hackathon/combined_results.csv", 
                             col_types = cols(X1 = col_skip()))
combined_results<-combined_results %>% 
  mutate(hour = hour(Datetime))%>%
  mutate(month = Month, weekday = `Day of the week`, week = `Week of the year`)%>%
  select(-c(Month,`Day of the week`, `Week of the year`, `Day of the month`))



glimpse(combined_results)
#Creating variables referenced from the data table. 

meter_choices = c()

for( i in unique(combined_results$better_label)){
  meter_choices = c(meter_choices,i)
}

str(meter_choices)


#glimpse(combined_results)

ui <- navbarPage("Spa-R-tans' Energy Consumption",
                   #create tab navigation for task 1.1
                   navbarMenu("Where will Spartan Energy Consumpution Be?",
                                tabPanel("Total Energy Consumption", value = 1,
                                         box(selectInput("meter_choice_box_1","Choose Building",
                                                         choices = meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_1", "Choose Time aggregation" , c("year","month","week", "weekday", "hour"),
                                                         selected = "month")),
                                         box(plotOutput("BuildingActualTotalplot"), width = "auto")),
                                tabPanel("Average Energy Consumption", value = 2,
                                         box(selectInput("meter_choice_box_2","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_2", "Choose Time aggregation" , c("year","month","week", "weekday", "hour"),
                                                         selected = "Month")),
                                             box(plotOutput("BuildingActualAverageplot"), width = "auto")
                                         )),
                   #create tab navigation for task 1.2
                   navbarMenu("How have predictions done so far?",
                                tabPanel("Total Energy Consumption", value = 3,
                                         box(selectInput("meter_choice_box_3","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_3", "Choose Time aggregation" , c("year","month","week", "weekday", "hour"),
                                                         selected = "Month")),
                                             box(plotOutput("BuildingPredictedTotalplot"), width = "auto")),
                                tabPanel("Average Energy Consumption", value = 4,
                                         box(selectInput("meter_choice_box_4","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_4", "Choose Time aggregation" , c("year","month","week", "weekday", "hour"),
                                                         selected = "Month")),
                                             box(plotOutput("BuildingPredictedAverageplot"), width = "auto")
                                             )),
                   #create tab navigation for task 2
                   navbarMenu("Where is Spartan Energy Consumpution Now?",
                                tabPanel("Total Energy Consumption", value = 5, 
                                         box(selectInput("meter_choice_box_5","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_5", "Choose Time aggregation" , c("year","month","week", "weekday", "hour"),
                                                         selected = "Month")),
                                             box(plotOutput("BuildingComparedTotalplot"), width = "auto")),
                                tabPanel("Average Energy Consumption", value = 6, 
                                         box(selectInput("meter_choice_box_6","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_6", "Choose Time aggregation" , c("year","month","week", "weekday", "hour"),
                                                         selected = "Month")),
                                             box(plotOutput("BuildingComparedAverageplot"), width = "auto"))))
                 


server <- function(input,output){
  
#####################################DATA TABLES################################################################
  #Reactive function to filter the dataset for the Actual Averages and Predicted Averages
  Actualdata_1 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata <- combined_results %>% # data table
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box_1) %>% #selecting user selected buildings
      mutate("Time_Choice" = case_when(input$time_choice_box_1 =="year"~ as.character(year(Datetime)),
                                       input$time_choice_box_1 =="month"~ as.character(strftime(Datetime, "%b")),
                                       input$time_choice_box_1 =="week"~ as.character(isoweek(Datetime)),
                                       input$time_choice_box_1 =="weekday"~ as.character(wday(Datetime)),
                                       input$time_choice_box_1 =="hour"~ as.character(hm(Datetime)),
                                       T ~ as.character("Time_Choice"))) %>% 
      select("Actual","Predicted","better_label", "Time_Choice") %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    
    Actualdata
    })
  Actualdata_2 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata2 <- combined_results %>% # data table
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box_2) %>% #selecting user selected buildings
      select("Actual","Predicted","better_label", "Datetime") %>% 
      mutate("Time_Choice" = case_when(input$time_choice_box_2 =="year"~ as.character(year(Datetime)), 
                                       input$time_choice_box_2 =="month"~ as.character(strftime(Datetime, "%b")), 
                                       input$time_choice_box_2 =="week"~ as.character(isoweek(Datetime)),
                                       input$time_choice_box_2 =="weekday"~ as.character(wday(Datetime)),
                                       input$time_choice_box_2 =="hour"~ as.character(hm(Datetime)),
                                       T ~ as.character("Time_Choice"))) %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    
    Actualdata2
  })
  
  Actualdata_3 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata3 <- combined_results %>% # data table
      select("Actual","Predicted","better_label", "Datetime") %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box_3) %>% #selecting user selected buildings
      select("Actual","Predicted","better_label", "Datetime") %>% 
      mutate("Time_Choice" = case_when(input$time_choice_box_3 =="year"~ as.character(year(Datetime)), 
                                       input$time_choice_box_3 =="month"~ as.character(strftime(Datetime, "%b")), 
                                       input$time_choice_box_3 =="week"~ as.character(isoweek(Datetime)),
                                       input$time_choice_box_3 =="weekday"~ as.character(wday(Datetime)),
                                       input$time_choice_box_3 =="hour"~ as.character(hm(Datetime)),
                                       T ~ as.character("Time_Choice"))) %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    
    Actualdata3
  })
  
  Actualdata_4 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata4 <- combined_results %>% # data table
      select("Actual","Predicted","better_label", "Datetime") %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box_4) %>% #selecting user selected buildings
      mutate("Time_Choice" = case_when(input$time_choice_box_4 =="year"~ as.character(year(Datetime)), 
                                       input$time_choice_box_4 =="month"~ as.character(strftime(Datetime, "%b")), 
                                       input$time_choice_box_4 =="week"~ as.character(isoweek(Datetime)),
                                       input$time_choice_box_4 =="weekday"~ as.character(wday(Datetime)),
                                       input$time_choice_box_4 =="hour"~ as.character(hm(Datetime)),
                                       T ~ as.character("Time_Choice"))) %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    
    Actualdata4
  })
  
  Actualdata_5 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata5 <- combined_results %>% # data table
      select("Actual","Predicted","better_label", "Datetime") %>% 
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box_5) %>% #selecting user selected buildings
      mutate("Time_Choice" = case_when(input$time_choice_box_5 =="year"~ as.character(year(Datetime)), 
                                       input$time_choice_box_5 =="month"~ as.character(strftime(Datetime, "%b")),
                                       input$time_choice_box_5 =="week"~ as.character(isoweek(Datetime)),
                                       input$time_choice_box_5 =="weekday"~ as.character(wday(Datetime)),
                                       input$time_choice_box_5 =="hour"~ as.character(hm(Datetime)),
                                       T ~ as.character("Time_Choice"))) %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    
    Actualdata5
  })
  
  Actualdata_6 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata6 <- combined_results %>% # data table
      # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
      filter(better_label == input$meter_choice_box_6) %>% #selecting user selected buildings
      mutate("Time_Choice" = case_when(input$time_choice_box_6 =="year"~ as.character(year(Datetime)), 
                                       input$time_choice_box_6 =="month"~ as.character(strftime(Datetime, "%b")), 
                                       input$time_choice_box_6 =="week"~ as.character(isoweek(Datetime)),
                                       input$time_choice_box_6 =="weekday"~ as.character(wday(Datetime)),
                                       input$time_choice_box_6 =="hour"~ as.character(hm(Datetime)),
                                       T ~ as.character("Time_Choice"))) %>% 
      select( "Time_Choice","Actual","Predicted","better_label") %>% 
      group_by(better_label,Time_Choice) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    
    Actualdata6
  })
  
  #####################################PLOTS##############################################################
  
  # Adapt code to if they select tabs 2, 4, or 6 and possibly actual /predicted
  
  output$BuildingActualTotalplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_col(aes(x = Time_Choice, y = "Total_Energy_Actual", fill = better_label), show.legend = F, position = position_dodge())+
      labs(title = paste("Total Energy Consumed for", input$meter_choice_box_1), subtitle = "subtitle here", caption = "Red line is Actual")+
      ylab(paste("Total Energy in 1000 BTUs", input$meter_choice_box_1))   # label rendered from userchoice labels
    
  })
  #Average Actual Energy Consumed Plot
  output$BuildingActualAverageplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_2())+ #user Time choice on x-axis
      geom_line(aes(x = Time_Choice, y = "Mean_Energy_Actual", color = better_label, group=1), show.legend = F)+
      labs(title = paste("Average Energy Consumed for", input$meter_choice_box_2), subtitle = "subtitle here", caption = "Red line is Actual")+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_2))   # label rendered from userchoice labels
      
  })
  
  output$BuildingPredictedTotalplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_3())+ #user Time choice on x-axis
      geom_col(aes(x = Time_Choice, y = "Total_Energy_Predicted", fill = better_label), show.legend = F, position = position_dodge())+
      labs(title = paste("Predicted Total Energy Consumed for", input$meter_choice_box_3), subtitle = "subtitle here", caption = "Green is predicted")+
      ylab(paste("Total Energy in 1000 BTUs", input$meter_choice_box_3))   # label rendered from userchoice labels
    
  })
  
  output$BuildingPredictedAverageplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_4())+ #user Time choice on x-axis
      geom_line(aes(x = Time_Choice, y = "Mean_Energy_Predicted", color = better_label, group=1), show.legend = F)+
      labs(title = paste("Predicted Average Energy Consumed for", input$meter_choice_box_4), subtitle = "subtitle here", caption = "Green is predicted")+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_4))   # label rendered from userchoice labels
    
  })
  
  output$BuildingComparedTotalplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_5())+ #user Time choice on x-axis
      geom_col(aes(x = Time_Choice, y = "Total_Energy_Actual"), fill = "red", show.legend = F, position = position_dodge())+
      geom_col(aes(x = Time_Choice, y = "Total_Energy_Predicted"), fill = "green", show.legend = F, position = position_dodge())+
      labs(title = paste("Comparing Average Energy Consumed for", input$meter_choice_box_5), subtitle = "subtitle here", caption = "Red line is Actual, Green is predicted")+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_5))   # label rendered from userchoice labels
    
  })
  
  
  output$BuildingComparedAverageplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_6())+ #user Time choice on x-axis
      geom_line(aes(x = Time_Choice, y = "Mean_Energy_Actual", group=1), color = "red", show.legend = F)+
      geom_line(aes(x = Time_Choice, y = "Mean_Energy_Predicted", group=1), color = "green", show.legend = F)+
      labs(title = paste("Comparing Average Energy Consumed for", input$meter_choice_box_6), subtitle = "subtitle here", caption = "Red line is Actual, Green is predicted")+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_6))   # label rendered from userchoice labels
    
  })
  
}

shinyApp(ui = ui, server = server)

