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
  r <- reactiveValues()

  homePageServer('home')
  report1Server('rpt1')
  # with global variable storage
  # don't forget to update the server function file with report1Server <- function(id, r)
  # report1Server('rpt1', r)
}

