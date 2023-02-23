report1UI <- function(id){
  ns <- NS(id)
  tagList(
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(sidebarPanel(
      sliderInput(
        ns("bins"),
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )

    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput(ns("distPlot")))
    )
  )
}

report1Server <- function(id){
  moduleServer(id, function (input, output, session){
    ns <- NS(id)

    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })
  })
}

