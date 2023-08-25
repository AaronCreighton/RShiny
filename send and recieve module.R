library(shiny)
library(tidyverse)

lower_UI <- function(id) {
  tagList(
    uiOutput(NS(id, "text3")),
    textOutput(NS(id,"verbose3")),
    actionButton(NS(id,"goButton"), label = "beep me", class = "btn-success")
  )
}

lower <- function(id, pass) {
  moduleServer(id, function(input, output, session) {

    lowerV = reactiveValues(returnV = NULL, inV = pass)


    output$text3 <- renderUI(textInput(session$ns("text3"), "test", "test"))
    output$verbose3 <- renderText(paste0(input$text3, " ", lowerV$inV))

    observe({
      lowerV$returnV = paste0("beeped: ", input$text3, " ", lowerV$inV)
    }) %>% bindEvent(input$goButton)



    return(reactive(lowerV$returnV))
})
}

upper_UI <- function(id) {
  tagList(
    uiOutput(NS(id, "text_in")),
    lower_UI(NS(id, "test"))
  )

}

upper <- function(id) {
  moduleServer(id, function(input, output, session) {

    upperV <- reactiveValues(one = NULL, react = NULL)

    static = "static"

    output$text_in <- renderUI(textInput(NS(id, "upper_text"), "test-upper", "reactive"))

    observe({
      upperV$react <- input$upper_text
    }) %>% bindEvent(input$upper_text)

    #upperV$one <- reactive(lower("test", pass = upperV$react))
    upperV$one <- reactive(lower("test", pass = static))

    observe({
      if(!is.null(upperV$one()())){
        print(upperV$one()())
      }
      # print(upperV$two()())
      # if(!is.null(upperV$two())){
      #   print(upperV$two())
      # }
    })
  })
}

ui <- fluidPage(
  upper_UI("upper")
)

server <- function(input, output, session) {
  upper("upper")
}

shinyApp(ui, server)
