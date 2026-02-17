# Repo Purpose

the purpose of this repository is to hold useful example code for creating RShiny applications and functionality. There is a strong focus for this repo to record code that will make it faster for future development e.g. shiny template or features that are not well documented else where. 

# General info

created by: Aaron Creighton <br>
Why: First created after my time working on shiny applications to save valuable learnings. <br>
Why public: initially this was a private repo, however the need arose to demonstrate my R coding skills. Thus a review of the code for public consumption was done. This review aimed to improve the code for anybody to use or read. Please reach out if you have any suggestions for improvement.

# Current contents of Repo

 - Shiny_template_full > is a shiny template with a full file architecture, designed to scale easily for complex apps. Not all apps need a full file architecture. Selecting the file architecture for an app is based on how complex the app is will be. See "ShinyFunctionOptions.md" under docs for different types of functions and uses.
 - Shiny_template_full-old > uses shinydashboard (instead of bslib), which is older dashboard tool with no new development, but also more mature. 

## Code Examples
- passing & returning static or reactive variables > small shiny app for experimenting with passing variables to module functions
- trigger a return value from sub-module > shows how to pass a value from a sub-module triggered by an event in the sub-module. 
- trigger a return dataframe from sub-module > shows how to pass a value from a sub-module triggered by an event in the sub-module. 



# updates to consider

shiny template full architecture:
- css examples
- add renv
