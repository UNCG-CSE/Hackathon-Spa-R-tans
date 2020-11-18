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
 Customize Fonts to align with University Brand |                              in progress...waiting for a reasonable time to send already typed message :))
 Apply single file visualization to combined File Visualizations |             to be started
 Organize Story to be of interest to students, staff, and community |          to be started
 Generalize/parametrize code to accept updated files |                         to be started
 Discuss/Revise Appropriate enhanced visualizations |                          to be started
 Add detail to graphs, determine appropriate tables, and boxes to add |        to be continued
 DEBUG color issue with second geom_col chart |                                to be started
 Add way to indicate specific meter |                                          combined file created...append new column)?

# Day 2
---
Mowgli - Adjusted file script to clean label names and inner-join them to larger dataframe with the correct building labels. 
Also generalized the date parse to clean all the data once its merged

### Tasks for tomorow
- [ ] pick up with shiny
- [ ] clone data into our own repo, create credentials to read data remotely. 

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
