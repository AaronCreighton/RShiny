library(shiny)
library(tidyverse)

lower_UI <- function(id) {
  tagList(
    uiOutput(NS(id, "text3")),
    textOutput(NS(id,"verbose3")),
    actionButton(NS(id,"goButton"), label = "beep me", class = "btn-success")
  )
}

lower <- function(id) {
  moduleServer(id, function(input, output, session) {

    retV = reactiveValues(output = NULL)
    output$text3 <- renderUI(textInput(session$ns("text3"), "test", "test"))
    output$verbose3 <- renderText(input$text3)
    observe({

      retV$output = input$text3
     # return(input$text3)
    }) %>% bindEvent(input$goButton)

    return(reactive(retV$output))
})
}

upper_UI <- function(id) {
  lower_UI(NS(id, "test"))
}

upper <- function(id) {
  moduleServer(id, function(input, output, session) {

  output <- reactiveValues(one = NULL)
  output$one <- lower("test")

  observe({
    print(output$one())
    })# %>% bindEvent(output$one, ignoreInit = T)
  })
}

ui <- fluidPage(
  upper_UI("upper")
)

server <- function(input, output, session) {
  upper("upper")
}

shinyApp(ui, server)
