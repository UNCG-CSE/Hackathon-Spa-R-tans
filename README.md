# Hackathon-Spa-R-tans
In this file, we are using data collected by UNCG of meter readings for various meetings on campus. This dashboard will update with data included in files combined from all of the meter readings provided. 
This dashboard was created using R commands and the RStudio Platform.
In order to begin, run the File_Combination.Rmd to recreate the combined_results csv. file. 
This file path will be required in order to utilize the spaRtan_semantic_dashboard.R file.
This project was funded by GreenFund and sponsored by UNCG.
The ui-contains the elements visible to the user. 
The server contains the data tables first and followed by the plots created with the ggplot2 platform. 



For the time being, this will be use as a daily diary for the comp.

We will be using R and Rshiny to build and deploy our dashboard. **May the best teams win** 😤. 


# Day 1 (Start Day)
---
Jaz- Created working sample dashboard template with Rshiny App, Determined method of extracting components of date using lubridate to enable filtering of year, hour and day, created possible visualizations addressing tasks 1 and 2 for single data file, created visualizations incorporating University Brand Colors.  

Mowgli - Wrote code that would read file names from data directory and combine all data into one data frame (3.6 million observations). This file was too large to push to github, so the file produced was ignored. The forloop still useful for "furture" / incoming data. Script also parses building name from file name, by removing *_results.csv* from file name

### Next steps:
Tasks | (STATUS)
----- | -------
Parse useful information from dates |                                complete for single file
 Begin Visualisation |                                                         complete for single file
 Begin building shiny to house visualizations |                                         started
 Find a place to store "combined_results" ? |                                  Very large file, perhaps we'll just point to raw data. 
 Ask about external source data usage |                                        completed
 Customize Fonts to align with University Brand |                              completed
 Apply single file visualization to combined File Visualizations |             completed
 Organize Story to be of interest to students, staff, and community |          to be started
 Generalize/parametrize code to accept updated files |                         completed
 Discuss/Revise Appropriate enhanced visualizations |                          to be started
 Add detail to graphs, determine appropriate tables, and boxes to add |        to be continued
 DEBUG color issue with second geom_col chart |                                completed
 Add way to indicate specific meter |                                          completed
 ShinyApp file (SpaRtanApp.R) discussion                                       to be started
 Additional aggregation variables?                                             to be started/discussed

# Day 2
---
Mowgli - Adjusted file script to clean label names and inner-join them to larger dataframe with the correct building labels. 
Also generalized the date parse to clean all the dates once its merged

Jaz- Created Shiny Application with minimum input and output variables, Created r markdown file document for documentation of process for presentation, Debug/Traceback Crash error in Rstudiocloud (Is the file just too big to process?)

### Tasks for tomorow
- [ ] pick up with shiny (review the SpaRtanApp.R file in Github)
- [ ] clone data into our own repo, create credentials to read data remotely. 
- [ ] Research/locate building descriptions / longitude and latitude
- [ ] Plot with user input in shiny (research/discuss)
- [!] Should we add a building type column to dataframe? (Academic/NonAcademic Factor, or Libraries, lecture-halls, residence hall, student center, dining hall, athletic, etc)?
- [!] How will date be selected by user? (Range, specific date, both?)
- [!] Determine Input and Output variables for Shiny App
- [ ] Story of interest to students, staff and community
- [ ] Map of Campus?
- [ ] Palette of Colors for 81 buildings?
- [X] Get data in repo
# Day 3
---

- Mowgli looked into git lfs, or getting oauth for private repo set up with R. Deemed unnessesary, so jazzy copied the data over to our repo. 
didn't do a whole lot today, sister-in-law birthday, and catching up on other project. Will continue the fight tomorow. 

-Jaz created user choices, debugged app
### Tasks for tomorow
- [ ] pick up with shiny (review the SpaRtanApp.R file in Github)
- [ ] Plot with user input in shiny (research/discuss)
- [] Add building type column to dataframe? (Academic/NonAcademic Factor, or Libraries, lecture-halls, residence hall, student center, dining hall, athletic, etc)?
- [!] How will date be selected by user? (Range, specific date, both?)
- [!] Determine Input and Output variables for Shiny App
- [ ] Story of interest to students, staff and community
- [ ] Map of Campus?
- [ ] Palette of Colors for 81 buildings?

# Day 4
---

- Mowgli - jazzy's shiny app ran into utf8 errors... something still wrong with the labels. Fixed labels, and adjusted shiny to work with labels. 
created new shinydashboard app (seperate file, as to not overwhelm other app, 100+ new lines) currently plot not rendering because user input not read correctly. 

- [ ] either fix the plot issuse.
- [ ] Adjust asthetics of the dashboard. 
- [ ] Add pages/content to the dashbaord. 

# Day 5
---

Mowgli and jaz worked to get files sync'd up. added reactive data to plot and now we have a working plot (task 2..ish)--- missing the date input(fixed at Year level)

Next is to build out the rest now that we have essential pieces. Also adding Quality of life features (color, font), and important text. 


# Day 6
Updated a layout scheme using navbarPage, but need to add headings. Created corresponding tables and plots for each of the user possibilities. Hour is working, but would like to address label titles and axes. Include more graphs, but need to double check filtering on tables or just create new tables, but figure out how do I filter for only Times that match Time_Label. I did try filter(Time_Label == input$time_choice_box_1), but unsure if that works well? Addressed the titles of axes, but not the labels of tick marks. Retried the Usertime function would be nice to use for display of tickmarcks.

# Filters for graphs
- [ ] fix date range plots, totals on columns.
- [ ] show label on y-axis and add subtitles. 
- [ ] sidepanel for inputs? Conditional Panels (if we have time), fix user based plots
- [ ] colors and finalize layout choice

---

# Day 7 (Final Day)
---

# Day 8 ? (Actual final day?)
