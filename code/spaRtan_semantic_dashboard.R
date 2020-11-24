if (!require(pacman)) install.packages("pacman") 
pacman::p_load(tidyverse,lubridate,shiny,shinydashboard,DT,dashboardthemes,zoo)

#detach("package:shiny.semantic",unload = T)
#detach("package:semantic.dashboard", unload = T)

# NOTES and TASKS:


## CURRENTLY APP IS RUNNING, BUT PLOT WILL NOT RENDER SINCE FILTER THINK NOT WORKING. PERHAPS NOT REDER PLOT, MAYBE RENDER DATASET OR SOMTHING idk!




# reading in the data

combined_results <- combined_results <- read_csv("C:/Users/macia/Documents/MSIA-19/Git/Hackathon-Spa-R-tans/code/combined_results.csv", 
                                                 col_types = cols(X1 = col_skip()))

#combined_results <- read_csv("D:/Hackathon/Hackathon/combined_results.csv", 
#                             col_types = cols(X1 = col_skip()))


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

# Creating Custom Theme -----------------

customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(15,32,68)"
    ,colorMiddle = "rgb(15,32,68)"
    ,colorEnd = "rgb(15,32,68)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(255,183,27,1)"
    ,colorMiddle = "rgba(255,183,27,1)"
    ,colorEnd = "rgba(255,183,27,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(44,222,201)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)





# Define UI ----------

# What does the dashboard look like?

ui <- dashboardPage(#skin = "blue" , # find appropriate uncg color?
  
  ### HEADER ------------
  
  dashboardHeader(title = shinyDashboardLogo(
    theme = "grey_dark",
    boldText = "Green Fund Hackathon:",
    mainText = "Energy Dashboard",
    badgeText = "V1.1"), titleWidth = 350), # Title of the dashboard
  
  ### SIDE BAR -----------
  
  dashboardSidebar( # declare a sidebar... hey R, theres a side bar!
    width = 350,
    sidebarMenu( # now what does the sidebar contain? let's fill it with menu items
      
      # First arg is what is seen on the dashboard, Second indicates to output where to show output
      # icon is not needed, just adds a little icon. 
      menuItem("Task 1.1: ", tabName = "task_1",icon = icon("signal")),
      menuItem("Task 1.2: What are we supposed to do?", tabName = "task_1_2", icon = icon("dashboard"))
      
    )
  ),
  
  ### Body/Content ----------
  
  dashboardBody(
    customTheme,
    tabItems(
      tabItem("task_1",
              h1("Static plot of energy consumption"),
              box(width = 10,title = "Select Plot Parameters",selectInput("meter_choice_box_1","Choose Building",meter_choices, selected = "Elliott University Center (040) - Main Meter", multiple = T),
                  selectInput("agg_level", "Choose sum or mean energy",
                              c("Mean_Energy_Actual","Mean_Energy_Predicted",
                                "Total_Energy_Actual", "Total_Energy_Predicted"), selected = "Total_Energy_Predicted"),
                  selectInput("time_choice_box_1", "Choose Time aggredation" , c("year",
                                                                                 "month","day","hour"),
                              selected = "month")),
              wellPanel(
                dateRangeInput("daterange_1", "", start = as.Date(start_date), end = as.Date(end_date)),
                plotOutput("task_1.1_plot"),width = "auto")),
      #box(plotOutput("task_1.1_plot"),width = "auto")),
      
      tabItem("task_1_2",# this links back to the sidebar item :) 
              
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
              
              box(plotOutput("task_1.2_plot"), width = "auto")))
  ))

server <- function(input,output){
  
  
  # Reeactive function to filter the dataset
  data_1.2 <- reactive({ # this is referenced in the ggplot. 
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
    
    data <- combined_results %>% filter(better_label == input$meter_choice_box_1,
                                        Datetime >= input$daterange_1[1] & Datetime <= input$daterange_1[2]) %>% 
      select("Actual","Predicted","Datetime","better_label") %>% 
      mutate("Time_Choice" = case_when(input$time_choice_box_1 == "year" ~ floor_date(as.Date(Datetime),"year"),
                                       input$time_choice_box_1 == "month" ~ floor_date(as.Date(Datetime),"month"),
                                       input$time_choice_box_1 == "day" ~ floor_date(as.Date(Datetime),"day",
                                                                                     input$time_choice_box_1 == "hour" ~ hour(Datetime))))%>% # combines months 
      group_by(better_label,Time_Choice) %>%   # grouping by year, so this will be a year plot, could ask them for input
      # summarizeing the mean for actual and predicted
      summarize("Mean_Energy_Actual" = mean(Actual), "Mean_Energy_Predicted" = mean(Predicted),
                "Total_Energy_Actual" = sum(Actual), "Total_Energy_Predicted" =  sum(Predicted))
    # fitering the dataset for their selected label -> could do this before hand maybe before running stats, would be easier
    
    
    
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
    ggplot(data_1.1(),aes(x = Time_Choice, group = better_label, color = better_label)) + # ggplot, year on x axis
      geom_line(aes_string(y = input$agg_level),show.legend = T)+
      scale_x_date(date_labels = "%m-%Y")+ 
      theme_minimal()+ # random theme
      labs(title = "Energy consumtion for selected building/buildings", subtitle = paste0("Buildings: ", input$meter_choice_box_1),
           caption = "Red line is Actual, Green is predicted",color = "Building/Meter")+
      ylab(if_else(input$agg_level == "Mean_Energy_Actual","Mean Energy Used",
                   if_else(input$agg_level == "Mean_Energy_Predicted","Mean Energy Predicted",
                           if_else(input$agg_level == "Total_Energy_Actual","Total Energy Used",
                                   if_else(input$agg_level == "Total_Energy_Predicted","Total Energy Predicted","Nothing")))))
  })
  
  # this plot will go the the "task_1" page :)
  
  output$task_1.2_plot <- renderPlot({ # render a plot, the meter_choice_plot, which is found in task_1 tab
    
    ggplot(data_1.2(),aes(x = Time_Label, color = better_label)) + # ggplot, year on x axis
      geom_line(aes(y = `Mean_Energy_Actual`),show.legend = T)+ # actual both on y axis
      geom_line(aes(y = `Mean_Energy_Predicted`), linetype = "dashed",show.legend = T)+ # predict
      theme_minimal()+ # random theme
      labs(title = paste("Energy for", input$meter_choice_box_3), subtitle = "subtitle here", caption = "Red line is Actual, Green is predicted")+
      ylab("Mean Energy")   # render labels
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)

              
