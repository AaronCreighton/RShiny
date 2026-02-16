### This App shows a number of cool things shiny can do.
# - The main demonstration is how send a single variable from a sub-module to the module above triggered by an event in the sub-module.
# - a practice example is a reusable form: one used for multiple situations, where the save button should be part of the forum,
# - but what is done with the data different.

## It also shows
# how to stream data from an input field, which could be recorded or sent somewhere as needed.


library(shiny)
library(tidyverse)

lower_UI <- function(id) {
  tagList(
    uiOutput(NS(id, "inputText")),
    textOutput(NS(id,"displayText")),
    actionButton(NS(id,"saveButton"), label = "save text", class = "btn-success")
  )
}

lowerSever <- function(id) {
  moduleServer(id, function(input, output, session) {

    retV = reactiveValues(lower_output = NULL)
    output$inputText <- renderUI(textInput(session$ns("inputText"), "input text box", "test"))
    output$displayText <- renderText(input$inputText) #stream text
    observe({

      retV$lower_output = input$inputText

    }) %>% bindEvent(input$saveButton)

    return(reactive(retV$lower_output)) # note the added layer of reactive
})
}

upper_UI <- function(id) {
  lower_UI(NS(id, "lower"))
}

upperServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  upper_retV <- reactiveValues(one = NULL)
  upper_retV$to_print <- lowerSever("lower")

  # inside of an observe take any actions needed with the module's return value.
  observe({
    print(upper_retV$to_print())
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
