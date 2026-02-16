### This App shows a number of cool things shiny can do.
# - The main demonstration is how send a dataframe from a sub-module to the module above triggered by an event in the sub-module.
# - a practice example is a reusable form: one used for multiple situations, where the save button should be part of the forum,
# - but what is done with the data different.

## It also shows
# how to stream data from an input field, which could be recorded or sent somewhere as needed.

# >> Not working yet!!


library(shiny)
library(tidyverse)

lower_UI <- function(id) {
    ns <- NS(id)
    tagList(
      shiny::textInput(ns("inputText1"), "input text box 1"),
      shiny::textInput(ns("inputText2"), "input text box 2"),
      textOutput(ns("displayText")),
      actionButton(ns("saveButton"), label = "save text", class = "btn-success")
    )

}

lowerServer <- function(id, pass_df = NULL) {
  moduleServer(id, function(input, output, session) {

    retV = reactiveValues(lower_output = NULL, lower_df = pass_df)

    observeEvent(retV$lower_df, ignoreNULL = FALSE, handlerExpr = {
      if(is_tibble(retV$lower_df)) {
        shiny::updateTextInput(session, "inputText1", value = retV$lower_df$field1)
        shiny::updateTextInput(session, "inputText2", value = retV$lower_df$field2)
      } else {
        shiny::updateTextInput(session, "inputText1", value = "")
        shiny::updateTextInput(session, "inputText2", value = "")
      }
    })
    output$displayText <- renderText(input$inputText1) #stream text

    observe({

      field_df <- tibble::tibble(
        field1 = input$field1,
        field2 = input$field2
      )

      retV$lower_output = field_df

    }) %>% bindEvent(input$saveButton, ignoreInit = T, ignoreNULL = T)

    return(reactive(retV$lower_output)) # note the added layer of reactive
})
}

upper_UI <- function(id) {
  lower_UI(NS(id, "lower"))
}

upperServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  upper_retV <- reactiveValues(to_print = NULL)

  setFields = NULL

  upper_retV$to_print <- reactive(lowerServer("lower", pass_df = setFields))

  # inside of an observe take any actions needed with the module's return value.
  observeEvent(upper_retV$to_print()(),{
    print(upper_retV$to_print()())
    })
  })
}

ui <- fluidPage(
  upper_UI("upper")
)

server <- function(input, output, session) {
  upperServer("upper")
}

shinyApp(ui, server)
