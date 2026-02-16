# Shiny Function Options
this is not an exhaustive list of all types of functions
this is a list with basic structure of the functions and current use-cases for them in the Shiny_template_full app

## Standard Functions
A couple can be found here > util_f_generalFunctions.R
this file contained utility type standard functions, with a general purpose
these function does not have any error handling & is your basic R function

```{r}
util_f_parse_str_sql <- function(x){
  r <- as.character(x) %>%
    str_replace_all("'","''") %>%
    str_replace_all("/","\\/")
  return(
    ifelse(
      r == "", # Don't submit "" as value to SQL Server... its annoying
      "null",
      paste0("'",r,"'")
    )
  )
}
```

## Module Functions
there is the file util_mf_generalFunctions.R for utility module functions that have a general purpose
 the examples below are found here > db_mf_ReportsUtils.R
 this file has Report specific utility module functions that interact with the database & Report only Helper functions
 there are three types of functions in here

 ### Setup
in global.R, in order to pull seperate function files in to development context
then the links to the files need the ```local = TRUE"``` flag.
 ```
 if(env == "DEV"){
  dbMPI <- "Sandbox"
  server <- "....,1435"

  #pull in functions for development
  source("R/db_mf_ReportsUtils.R", local = TRUE)
  source("R/util_f_generalFunctions.R", local = TRUE)
  source("R/util_mf_generalFunctions.R", local = TRUE)
} 

```

### --- Helper functions that specific for Report/item, are standard R functions
 these are standard R functions & do not require error handling

```{r}
validateReportInput <- function (singleReport_df,
                                  isBulletin){
  # validates code
  # returns if it is valid or not
}
```
### --- get module functions
 these functions require error handlings (using the custom gr message method)
 they return either the data or an error code

this example has been stripped down function to the basics

```{r}
mf_getSingleReportExists <- function(id, gr,
                                      userCurrent = user,
                                      serial_in,
                                      customer_in,
                                      type_in,
                                      datePub_in){
  moduleServer(id, function(input, output, session) {

    posReports_df <- tryCatch(con %>%
                                 tbl(in_schema(schema, "Reports")) %>%
                                 # .... filters table etc
                                 collect(),
                               error = function(e) {
                                 gr$errorHandling$action <- "Search table for Report"
                                 gr$errorHandling$error <- e
                                 gr$errorHandling$state <- "ERROR"
                               }

    )
    if(is.null(posReports_df)){
      log_event("Get Report ID - Failed to access")
      shiny::showNotification(
        "Error - Failed to Access DB",
        closeButton = TRUE,
        type = "error"
      )
      return(-1)
    } else if (nrow(posReports_df)) {
      log_event(paste0("DB Check - Report is in table with id: ", toString(posReports_df$ReportID)))
      shiny::showNotification(
        "Report is already in Database",
        closeButton = TRUE,
        type = "warning"
      )
      return(posReports_df)
    }
  })
}
```
### --- transaction module functions
they clean the data for the DB
they wrap a transaction around a function call to the database, that changes the database
they require error handling

```{r}
mf_addNewReportTrans <- function(id, gr,
                                  entryIDs,
                                  singleReport_df){
  moduleServer(id, function(input, output, session) {

    # code to clean the data

    tryCatch(
      pool::poolWithTransaction(con, function(conn) {
        # code to change the database
      }
      ),
      error = function(e){
        gr$errorHandling$action = "Saving an Report to tables"
        gr$errorHandling$error = e
        gr$errorHandling$state = "ERROR"
      }
    )
  })
}

```

### --- General wrapper module function
they or sub-functions use the custom gr$ error handling
they check and validate the input
they create the dataframe / entry that will be sent to the db
they add ```waiter_show(``` and messaging around the transaction
they return errors and sometimes the row that was added for processing in the higher module

```{r}
mf_updateReport <- function(id, gr,
                             #entryIDs = NULL,
                             itemIDs = NULL,
                             ReportID_in,
                             serial_in,
                             title_in,
                             summary_in,
                             customer_in,
                             contactAnalyst_in,
                             type_in,
                             status_in,
                             draftLink_in,
                             finalLink_in,
                             datePub_in,
                             invalidated_in = 0,
                             isBulletin = FALSE){
  moduleServer(id, function(input, output, session) {

    if(type_in %in% notReportList){
      isReport_in = 0
    } else if(type_in == ""){
      return(FALSE)
    } else {
      isReport_in = 1
    }

    if (invalidated_in == 1){
      singleReport_df <- tibble::tibble(
        ReportID = ReportID_in,
        dateEdit = Sys.Date(),
        invalidated = 1
      )
    } else {
      singleReport_df <- tibble::tibble(
        ReportID = ReportID_in,
        isReport = isReport_in,
        dateEdit = Sys.Date(),
        serial = serial_in,
        title = title_in,
        summary = summary_in,
        customer = customer_in,
        contactAnalyst = contactAnalyst_in,
        type = type_in,
        status = status_in,
        draftLink = draftLink_in,
        finalLink = finalLink_in,
        datePublished = datePub_in,
        invalidated = 0
      )
      if(!validateReportInput(singleReport_df = singleReport_df,
                               isBulletin = isBulletin)){
        return(FALSE)
      }
    }
    if(!isBulletin){
      waiter_show( # show the waiter
        html = tagList(spin_1(), "Loading ..."), color = "#fdb913" # use a spinner
      )
      log_event("Submitting updated Report to server")
      message("----BEGIN UPDATE----")
      mf_updateReportTrans("updateUtil", gr,
                            entryIDs = entryIDs,
                            singleReport_df = singleReport_df)
      message("----END UPDATE----")
      waiter_hide()
      shiny::showNotification(
        "Data Updated",
        closeButton = TRUE,
        type = "message"
      )
    }
  })
}

```

### a Modules that acts as a function
A simplified version is found here https://stackoverflow.com/questions/76974297/how-to-pass-and-receive-variables-to-a-module-function-then-act-on-it or in the example code folder.
This is an example: ui_m_ReportsFields.R
this is a user interface module that holds the Report fields
this module updates fields in the form based on what is passed
This module returns the user input based on a user action, button click to the upper module for processing


#### - Higher/Upper Module
this is how it is called in the higher module

```{r}
# UI code
bs4Dash::box(
  width = 12,

  status = "primary",
  solidHeader = T, collapsible = T, title = "New Report",
  id = "collapsing", collapsed = T,
  ReportFieldsUI(ns("newProd"))
)
```

```{r}
# server code:

manageReport$values <- reactive(ReportFieldsServer("newProd", r, (!is_empty(r$viewTable$multiSelectIDs))))

observeEvent(manageReport$values()(),{
  if(!is.null(manageReport$values) && is_tibble(manageReport$values()())){
    manageReport$savedReport <- mf_addNewReport("newProd", r,
                                                   userCurrent = user,
                                                   entryIDs = r$viewTable$multiSelectIDs,
                                                   serial_in = manageReport$values()()$serial_in,
                                                   title_in = manageReport$values()()$title_in,
                                                   summery_in = manageReport$values()()$summary_in,
                                                   customer_in = manageReport$values()()$customer_in,
                                                   contactAnalyst_in = manageReport$values()()$contactAnalyst_in,
                                                   type_in = manageReport$values()()$type_in,
                                                   status_in = manageReport$values()()$status_in,
                                                   draftLink_in = manageReport$values()()$draftLink_in,
                                                   finalLink_in = manageReport$values()()$finalLink_in,
                                                   datePub_in = manageReport$values()()$datePub_in
    )
    r$Reports$freshness <- r$Reports$freshness + 1
    if(is_tibble(manageReport$savedReport)){
      showModal(
        modalDialog(
          p("This Report Appears to Exist.",style="font-weight:bold;text-align:center"),
          br(),
          "Show it in the View Reports table?",
          br(),br(),
          footer = div(
            actionButton(session$ns("showInReportsT"), label = "Yes", icon = icon("ok", lib = "glyphicon")),
            modalButton("No", icon = icon("remove", lib = "glyphicon"))
          )
        )
      )

    }


  }

})


```
####  Inner module
inside the module that is called
here is the server, the only part that changes from a regular module
things to pay attention to is the level of re-activity
code as been stripped to focus on the core elements

```{r}
ReportFieldsServer <- function(id, r, singleReportFields_df = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    ReportFields <- reactiveValues(datePub = "",
                                    returnReport = NULL,
                                    setFields = singleReportFields_df # df stored in local reactive variable
                                    )

    # doesn't seem to work with bind event
    observeEvent(ReportFields$setFields, ignoreNULL = FALSE, handlerExpr = {
      if(is_tibble(ReportFields$setFields)){

        # code to update the fields with values
        shiny::updateSelectInput(session, "type_in", selected = singleReport_df$type)


        log_event(paste0("setup Report input fields"))
      } else {
        # code to update the fields with empty
        shiny::updateSelectInput(session, "type_in", selected = "")
      }
    })

    observe({
      #create the return data frame
      singleReport_df <- tibble::tibble(
        title_in = input$title_in,
        summary_in = input$summary_in,
        customer_in = input$customer_in,
        serial_in = input$serial_in,
        contactAnalyst_in = input$pocAnalyst_in,
        type_in = input$type_in,
        status_in = input$ReportStatus_in,
        draftLink_in = input$draftLink_in,
        finalLink_in = input$finalLink_in,
        datePub_in = ReportFields$datePub,
        invalidated = 0
      )

      # by storing it in df as a variable the correct behavior is achieved.
      # If I pass a variable  the information is streamed back to the parent module.
      ReportFields$returnReport <- singleReport_df

      log_event(paste0("user submitted Report input fields"))


    }) %>% bindEvent(input$ReportSubmit, ignoreInit = T, ignoreNULL = T)

    # a reactive layer around the reactive variable
    return(reactive(ReportFields$returnReport))


  })
}

```