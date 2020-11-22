if (!require(pacman)) install.packages("pacman") 
pacman::p_load(tidyverse,lubridate,shiny,shinydashboard)



# NOTES and TASKS:


# reading in the data

combined_results <- read_csv("combined_results.csv", 
                             col_types = cols(X1 = col_skip()))

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
                                             selectInput("time_choice_box_1", "Choose Time aggredation" , c("Day of the month", "Week of the year","Month"),
                                                         selected = "Month"),
                                             box(plotOutput("BuildingActualTotalplot"), width = "auto")
                                             )),
                                tabPanel("Average Energy Consumption", value = 2,
                                         box(selectInput("meter_choice_box_2","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_2", "Choose Time aggredation" , c("Day of the month","Week of the year","Month"),
                                                         selected = "Month"),
                                             box(plotOutput("BuildingActualAverageplot"), width = "auto")
                                         ))),
                   #create tab navigation for task 1.2
                   navbarMenu("How have predictions done so far?",
                                tabPanel("Total Energy Consumption", value = 3,
                                         box(selectInput("meter_choice_box_3","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_3", "Choose Time aggredation" , c("Day of the month","Week of the year","Month"),
                                                         selected = "Month"),
                                             box(plotOutput("BuildingPredictedTotalplot"), width = "auto")
                                             )),
                                tabPanel("Average Energy Consumption", value = 4,
                                         box(selectInput("meter_choice_box_4","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_4", "Choose Time aggredation" , c("Day of the month",
                                                                                                            "Week of the year","Month"),
                                                         selected = "Month"),
                                             box(plotOutput("BuildingPredictedAverageplot"), width = "auto")
                                             ))),
                   #create tab navigation for task 2
                   navbarMenu("Where is Spartan Energy Consumpution Now?",
                                tabPanel("Total Energy Consumption", value = 5, 
                                         box(selectInput("meter_choice_box_1","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_1", "Choose Time aggredation" , c("Day of the month",
                                                                                                            "Week of the year","Month"),
                                                         selected = "Month"),
                                             box(plotOutput("BuildingComparedTotalplot"), width = "auto")
                                             )
                                         ),
                                tabPanel("Average Energy Consumption", value = 6, 
                                         box(selectInput("meter_choice_box_1","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_1", "Choose Time aggredation" , c("Day of the month",
                                                                                                            "Week of the year","Month"),
                                                         selected = "Month"),
                                             box(plotOutput("BuildingComparedAverageplot"), width = "auto")
                                         )
                                         ))
)
                 

                              
                     

server <- function(input,output){
  #Reactive function to filter the dataset for the Actual Averages and Predicted Averages
  Actualdata_1 <- reactive({ # Reactive data table is referenced in the ggplot.
    Actualdata <- combined_results %>% # data table
      filter(better_label == input$meter_choice_box_2) %>% #selecting user selected buildings
      select("Actual","Predicted","Hour",input$time_choice_box_2,"better_label") %>% 
      gather(key = "Time_Choice","Time_Label",-c("Actual","Predicted","Hour","better_label")) %>% 
      group_by(better_label,Time_Label) %>%   # grouping by user input for time, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
    
    Actualdata
    })
  
  # Adapt code to if they select tabs 2, 4, or 6 and possibly actual /predicted
  #Average Actual Energy Consumed Plot
  output$BuildingActualAverageplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_line(aes(x = "Time_Label", y = "Mean_Energy_Actual", color = "better_label"), show.legend = F)+
      labs(title = paste("Average Energy Consumed for", input$meter_choice_box_2), subtitle = "subtitle here", caption = "Red line is Actual")+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_2))   # label rendered from userchoice labels
      
  })
  
  output$BuildingPredictedAverageplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_line(aes(x = "Time_Label", y = "Mean_Energy_Predicted", color = "better_label"), show.legend = F)+
      labs(title = paste("Predicted Average Energy Consumed for", input$meter_choice_box_4), subtitle = "subtitle here", caption = "Green is predicted")+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_4))   # label rendered from userchoice labels
    
  })
  
  output$BuildingComparedAverageplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_line(aes(x = "Time_Label", y = "Mean_Energy_Actual"), color = "red", show.legend = F)+
      geom_line(aes(x = "Time_Label", y = "Mean_Energy_Predicted"), color = "green", show.legend = F)+
      labs(title = paste("Comparing Average Energy Consumed for", input$meter_choice_box_6), subtitle = "subtitle here", caption = "Red line is Actual, Green is predicted")+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_6))   # label rendered from userchoice labels
    
  })
  
  output$BuildingActualTotalplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_col(aes(x = "Time_Label", y = "Total_Energy_Actual", color = "better_label"), show.legend = F)+
      labs(title = paste("Total Energy Consumed for", input$meter_choice_box_2), subtitle = "subtitle here", caption = "Red line is Actual")+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))+
      ylab(paste("Total Energy in 1000 BTUs", input$meter_choice_box_2))   # label rendered from userchoice labels
    
  })
  
  output$BuildingPredictedTotalplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_col(aes(x = "Time_Label", y = "Total_Energy_Predicted", color = "better_label"), show.legend = F)+
      labs(title = paste("Predicted Total Energy Consumed for", input$meter_choice_box_4), subtitle = "subtitle here", caption = "Green is predicted")+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))+
      ylab(paste("Total Energy in 1000 BTUs", input$meter_choice_box_4))   # label rendered from userchoice labels
    
  })
  
  output$BuildingComparedTotalplot<- renderPlot({ #render a plot using the Actual Averages
    ggplot(Actualdata_1())+ #user Time choice on x-axis
      geom_col(aes(x = "Time_Label", y = "Total_Energy_Actual"), color = "red", show.legend = F)+
      geom_col(aes(x = "Time_Label", y = "Total_Energy_Predicted"), color = "green", show.legend = F)+
      labs(title = paste("Comparing Average Energy Consumed for", input$meter_choice_box_6), subtitle = "subtitle here", caption = "Red line is Actual, Green is predicted")+
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000))+
      ylab(paste("Mean Energy in 1000 BTUs", input$meter_choice_box_6))   # label rendered from userchoice labels
    
  })

}

shinyApp(ui = ui, server = server)

