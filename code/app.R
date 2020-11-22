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
                                                         selected = "Month")
                                             )),
                                tabPanel("Average Energy Consumption", value = 2,
                                         box(selectInput("meter_choice_box_2","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_2", "Choose Time aggredation" , c("Day of the month","Week of the year","Month"),
                                                         selected = "Month")
                                         ))),
                   #create tab navigation for task 1.2
                   navbarMenu("How have predictions done so far?",
                                tabPanel("Total Energy Consumption", value = 3,
                                         box(selectInput("meter_choice_box_3","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_3", "Choose Time aggredation" , c("Day of the month","Week of the year","Month"),
                                                         selected = "Month")
                                             )),
                                tabPanel("Average Energy Consumption", value = 4,
                                         box(selectInput("meter_choice_box_4","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_4", "Choose Time aggredation" , c("Day of the month",
                                                                                                            "Week of the year","Month"),
                                                         selected = "Month")
                                             ))),
                   #create tab navigation for task 2
                   navbarMenu("Where is Spartan Energy Consumpution Now?",
                                tabPanel("Total Energy Consumption", value = 5, 
                                         box(selectInput("meter_choice_box_1","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_1", "Choose Time aggredation" , c("Day of the month",
                                                                                                            "Week of the year","Month"),
                                                         selected = "Month")
                                             )
                                         ),
                                tabPanel("Average Energy Consumption", value = 6, 
                                         box(selectInput("meter_choice_box_1","Choose Building",
                                                         meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                                             selectInput("time_choice_box_1", "Choose Time aggredation" , c("Day of the month",
                                                                                                            "Week of the year","Month"),
                                                         selected = "Month")
                                         )
                                         ))
)
                 

                              
                     

server <- function(input,output){
  
 
  
}

shinyApp(ui = ui, server = server)

