homePageUI <- function(id){
  ns <- NS(id)
  tagList(
    # Application title
    titlePanel("Hello"),
    textOutput(ns("greeting"))
  )

}

homePageServer <- function(id){
  moduleServer(id, function (input, output, session){
    output$greeting <- renderText({
      paste0("Welcome ", user, ", to this app!")
    })
  })
}



