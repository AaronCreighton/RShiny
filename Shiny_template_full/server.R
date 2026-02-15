# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # stop processes when page closed
  # may not need this for basic apps:
  session$onSessionEnded(function(){
    stopApp()
  })


  # set up global nested reactive values according to golem recommendations
  # https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  # its much easier to communicate, but modules can't easily be used in other apps
  gr <- reactiveValues()

  homePageServer('home')
  report1Server('rpt1', gr)
  report2Server('rpt2', gr)
  # to remove global storage, don't forget
  # to update the server function file with report1Server <- function(id, gr)
  #

  output$download1 <- downloadHandler(
    filename = function() {
      paste0(user, "-histogram-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = gr$save_plot(), # Call the reactive to get the ggplot object
        width = input$plot_width,
        height = 4,
        units = "in",
        dpi = 300
      )
    }
  )

}

