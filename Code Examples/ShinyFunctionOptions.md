# Shiny Function Options
this is not an exhaustive list of all type of functions
this is a list with basic structure of the functions and current use-cases for them in Manutaki

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
 the examples below are found here > db_mf_productsUtils.R
 this file has product specific utility module functions that interact with the database & product only Helper functions
 there are three types of functions in here

 ### Setup
in global.R, in order to pull seperate function files in to development context
then the links to the files need the ```local = TRUE"``` flag.
 ```
 if(build == "TEST"){
  dbMPI <- "Sandbox"
  server <- "ADS-UAT,1435"

  #pull in functions for development
  source("R/db_mf_productsUtils.R", local = TRUE)
  source("R/util_f_generalFunctions.R", local = TRUE)
  source("R/util_mf_generalFunctions.R", local = TRUE)
} else {
  dbMPI <- "DataRaw"
  server <- "ADS"
}
 ```

### --- Helper functions that specific for product, are standard R functions
 these are standard R functions & do not require error handling

```{r}
validateProductInput <- function (singleProduct_df,
                                  isBulletin){
  # validates code
  # returns if it is valid or not
}
```
### --- get module functions
 these functions require error handlings
 they return either the data or an error code

this example has been stripped down function to the basics

```{r}
mf_getSingleProductExists <- function(id, r,
                                      userCurrent = user,
                                      serial_in,
                                      customer_in,
                                      type_in,
                                      datePub_in){
  moduleServer(id, function(input, output, session) {

    posProducts_df <- tryCatch(con %>%
                                 tbl(in_schema(schema, "products")) %>%
                                 # .... filters table etc
                                 collect(),
                               error = function(e) {
                                 r$errorHandling$action <- "Search table for product"
                                 r$errorHandling$error <- e
                                 r$errorHandling$state <- "ERROR"
                               }

    )
    if(is.null(posProducts_df)){
      diag_rep("Get Product ID - Failed to access")
      shiny::showNotification(
        "Error - Failed to Access DB",
        closeButton = TRUE,
        type = "error"
      )
      return(-1)
    } else if (nrow(posProducts_df)) {
      diag_rep(paste0("DB Check - Product is in table with id: ", toString(posProducts_df$productID)))
      shiny::showNotification(
        "Product is already in Database",
        closeButton = TRUE,
        type = "warning"
      )
      return(posProducts_df)
    }
  })
}
```
### --- transaction module functions
they clean the data for the DB
they wrap a transaction around a function call to the database, that changes the database
they require error handling

```{r}
mf_addNewProductTrans <- function(id, r,
                                  entryIDs,
                                  singleProduct_df){
  moduleServer(id, function(input, output, session) {

    # code to clean the data

    tryCatch(
      pool::poolWithTransaction(con, function(conn) {
        # code to change the database
      }
      ),
      error = function(e){
        r$errorHandling$action = "Saving an product to tables"
        r$errorHandling$error = e
        r$errorHandling$state = "ERROR"
      }
    )
  })
}

```

### --- General wrapper module function
they or sub-functions use the MT r$ error handling
they check and validate the input
they create the dataframe / entry that will be sent to the db
they add ```waiter_show(``` and messaging around the transaction
they return errors and sometime the row that was added for processing in the higher module

```{r}
mf_updateProduct <- function(id, r,
                             #entryIDs = NULL,
                             itemIDs = NULL,
                             productID_in,
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

    if(type_in %in% notProductList){
      isProduct_in = 0
    } else if(type_in == ""){
      return(FALSE)
    } else {
      isProduct_in = 1
    }

    if (invalidated_in == 1){
      singleProduct_df <- tibble::tibble(
        productID = productID_in,
        dateEdit = Sys.Date(),
        invalidated = 1
      )
    } else {
      singleProduct_df <- tibble::tibble(
        productID = productID_in,
        isProduct = isProduct_in,
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
      if(!validateProductInput(singleProduct_df = singleProduct_df,
                               isBulletin = isBulletin)){
        return(FALSE)
      }
    }
    if(!isBulletin){
      waiter_show( # show the waiter
        html = tagList(spin_1(), "Loading ..."), color = "#fdb913" # use a spinner
      )
      diag_rep("Submitting updated product to server")
      message("----BEGIN UPDATE----")
      mf_updateProductTrans("updateUtil", r,
                            entryIDs = entryIDs,
                            singleProduct_df = singleProduct_df)
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
A simplified version is found here https://stackoverflow.com/questions/76974297/how-to-pass-and-receive-variables-to-a-module-function-then-act-on-it
This is an example: ui_m_productsFields.R
this is a user interface model that holds the product fields
this module updates fields in the form based on what is passed
This module returns the user input based on a user action, button click to the upper module for processing


#### - Higher/Upper Module
this is how it is called in the higher module

```{r}
# UI code
bs4Dash::box(
  width = 12,

  status = "primary",
  solidHeader = T, collapsible = T, title = "New Product",
  id = "collapsing", collapsed = T,
  productFieldsUI(ns("newProd"))
)
```

```{r}
# server code:

manageProduct$values <- reactive(productFieldsServer("newProd", r, (!is_empty(r$viewTable$multiSelectIDs))))

observeEvent(manageProduct$values()(),{
  if(!is.null(manageProduct$values) && is_tibble(manageProduct$values()())){
    manageProduct$savedProduct <- mf_addNewProduct("newProd", r,
                                                   userCurrent = user,
                                                   entryIDs = r$viewTable$multiSelectIDs,
                                                   serial_in = manageProduct$values()()$serial_in,
                                                   title_in = manageProduct$values()()$title_in,
                                                   summery_in = manageProduct$values()()$summary_in,
                                                   customer_in = manageProduct$values()()$customer_in,
                                                   contactAnalyst_in = manageProduct$values()()$contactAnalyst_in,
                                                   type_in = manageProduct$values()()$type_in,
                                                   status_in = manageProduct$values()()$status_in,
                                                   draftLink_in = manageProduct$values()()$draftLink_in,
                                                   finalLink_in = manageProduct$values()()$finalLink_in,
                                                   datePub_in = manageProduct$values()()$datePub_in
    )
    r$products$freshness <- r$products$freshness + 1
    if(is_tibble(manageProduct$savedProduct)){
      showModal(
        modalDialog(
          p("This product Appears to Exist.",style="font-weight:bold;text-align:center"),
          br(),
          "Show it in the View Products table?",
          br(),br(),
          footer = div(
            actionButton(session$ns("showInProductsT"), label = "Yes", icon = icon("ok", lib = "glyphicon")),
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
productFieldsServer <- function(id, r, singleProductFields_df = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    productFields <- reactiveValues(datePub = "",
                                    returnProduct = NULL,
                                    setFields = singleProductFields_df # df stored in local reactive variable
                                    )

    # doesn't seem to work with bind event
    observeEvent(productFields$setFields, ignoreNULL = FALSE, handlerExpr = {
      if(is_tibble(productFields$setFields)){

        # code to update the fields with values
        shiny::updateSelectInput(session, "type_in", selected = singleProduct_df$type)


        diag_rep(paste0("setup product input fields"))
      } else {
        # code to update the fields with empty
        shiny::updateSelectInput(session, "type_in", selected = "")
      }
    })

    observe({
      #create the return data frame
      singleProduct_df <- tibble::tibble(
        title_in = input$title_in,
        summary_in = input$summary_in,
        customer_in = input$customer_in,
        serial_in = input$serial_in,
        contactAnalyst_in = input$pocAnalyst_in,
        type_in = input$type_in,
        status_in = input$productStatus_in,
        draftLink_in = input$draftLink_in,
        finalLink_in = input$finalLink_in,
        datePub_in = productFields$datePub,
        invalidated = 0
      )

      # by storing it in df as a variable the correct behavior is achieved.
      # If I pass a variable  the information is streamed back to the parent module.
      productFields$returnProduct <- singleProduct_df

      diag_rep(paste0("user submitted product input fields"))


    }) %>% bindEvent(input$productSubmit, ignoreInit = T, ignoreNULL = T)

    # a reactive layer around the reactive variable
    return(reactive(productFields$returnProduct))


  })
}

```