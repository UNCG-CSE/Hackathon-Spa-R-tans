# Hackathon-Spa-R-tans

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
- [ ] Get data in repo
# Day 3
---

# Day 4
---


# Day 5
---

# Day 6
---

# Day 7 (Final Day)
---
