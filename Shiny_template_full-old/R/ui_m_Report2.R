report2UI <- function(id){
  ns <- NS(id)
}

report2Server <- function(id, gr){
  moduleServer(id, function (input, output, session){
    output$distPlot <- renderPlot({

    })

  })
}

