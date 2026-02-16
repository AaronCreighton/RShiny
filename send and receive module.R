# This R shiny app is for experimenting with sending static or reactive values to a module
# in the current version, sending a reactive variable resets the lower text field





library(shiny)
library(tidyverse)

lower_UI <- function(id) {
  tagList(
    uiOutput(NS(id, "inputText")),
    textOutput(NS(id,"displayText")),
    actionButton(NS(id,"saveButton"), label = "save text", class = "btn-success")
  )

}

lowerServer <- function(id, pass = NULL) {
  moduleServer(id, function(input, output, session) {

    retV = reactiveValues(lower_output = NULL, local_pass = pass)

    output$inputText <- renderUI(textInput(session$ns("inputText"), "lower text box", "test"))
    output$displayText <- renderText(paste0(input$inputText, " ", retV$local_pass)) #stream text

    observe({
      retV$lower_output = paste0("beeped: ", input$inputText, " ", retV$local_pass)
    }) %>% bindEvent(input$saveButton, ignoreInit = T, ignoreNULL = T) #need at least ignoreInit or else it streams

    return(reactive(retV$lower_output)) # note the added layer of reactive
  })
}

upper_UI <- function(id) {
  tagList(
    uiOutput(NS(id, "text_in")),
    lower_UI(NS(id, "lower"))
  )
}

upperServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    upper_retV <- reactiveValues(to_print = NULL, react = NULL)

    static = "static"

    output$text_in <- renderUI(textInput(NS(id, "upper_text"), "upper text input", "reactive"))


    observe({
      upper_retV$react <- input$upper_text
    }) %>% bindEvent(input$upper_text)

    upper_retV$to_print <- reactive(lowerServer("lower", pass = upper_retV$react))
    #upper_retV$to_print <- reactive(lowerServer("lower", pass = static))


    # inside of an observe take any actions needed with the module's return value.
    observe({
      if(!is.null(upper_retV$to_print()())){
        print(upper_retV$to_print()())
      }
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
