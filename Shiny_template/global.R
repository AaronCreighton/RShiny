library(shinydashboard)

# hide warnings etc.
# suppressWarnings(suppressMessages(library(tidyverse)))


#Bookmarking####
enableBookmarking(store = "url")


#log event to console
console_log <- function(x){
  message(paste0("LOG - ",Sys.time()," - ",x))
}

#pull in functions for development
#source("R/moduleFunctions.R", local = TRUE)
#ource("R/generalFunctions.R", local = TRUE)

# declare global static variables here e.g.:

## user identification based on windows login
user <- Sys.info()["user"] %>%
  as.character() %>%
  str_to_title()

## global static lists etc

CustomerList <- c("A","B", "C")