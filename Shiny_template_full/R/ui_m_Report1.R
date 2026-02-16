report1UI <- function(id, gr){
  ns <- NS(id)
    # Application title
    titlePanel("Old Faithful Geyser Data")

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

}

report1Server <- function(id, gr){
  moduleServer(id, function (input, output, session){

    gr$save_plot <- reactive({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      log_event("r1 plot updated")
      ggplot(faithful, aes(x = waiting)) +
        geom_histogram(
          bins = input$bins,
          fill = 'darkgray',
          color = 'white'
        ) +
        labs(
          title = 'Histogram of waiting times',
          x = 'Waiting time to next eruption (in mins)',
          y = 'Frequency'
        ) +
        theme_minimal()

    })

    output$distPlot <- renderPlot({
      gr$save_plot()
    })
  })
}

