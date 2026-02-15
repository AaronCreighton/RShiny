
# libraries

library(shinydashboard)

# hide warnings etc.
suppressWarnings(suppressMessages(library(tidyverse)))


## Security audit on packages - load all packages first
# s <- sessionInfo()
# df <- map_dfr(c(s$otherPkgs,s$loadedOnly),function(x){
#   tibble(package = x$Package,version = x$Version)
# })
# sec_audit <- oysteR::audit(pkg = df$package, version = df$version, type = "cran")


#Bookmarking####
enableBookmarking(store = "url")

#function to log event function
log_event <- function(x){
  message(paste0("LOG - ",Sys.time()," - ",x))
}

## set up a custom log file in environments - if needed
#as the R session dies should get no files held open by sink

env = "DEV"

start_time <- Sys.time() %>% as.numeric() %>% round()

if(env != "DEV"){
  if(!dir.exists("logs")){dir.create("logs")}

  if({Sys.glob("logs/*") %>% length()} > 10){
    log_df <- file.info(list.files("logs",full.names=TRUE))
    oldest_log <- row.names(log_df)[which.min(log_df$mtime)]
    file.remove(oldest_log)
  }

  log <- file(paste0("logs/",start_time,"_log.txt"), open = "wt")
  sink(log, type = "message")
}

message("----------------------------------")
log_event(paste0("Operating in", env))
log_event("libraries loaded")
log_event('load global objects')


#pull in functions for development
source("R/util_f_generalFunctions.R", local = TRUE)
source("R/db_f_generalFunctions.R", local = TRUE)
source("R/db_mf_dbTableActions.R", local = TRUE)

log_event("custom functions loaded")

# declare global static variables here e.g.:

## user identification based on windows login
user <- Sys.info()["user"] %>%
  as.character() %>%
  str_to_title()

## global static lists etc e.g.

CustomerList <- c("A","B", "C")


log_event("loaded global ojects")
