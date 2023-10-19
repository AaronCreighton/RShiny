
# the function is for bookmarking, needs to be at top of page
# consider other methods if no bookmarking
# ui <- fluidPage(
#   ...
# )
# or
# ui <- dashboardPage()

ui <- function(request){



Header <- dashboardHeader(title = "testing testing v2")


sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(
    id = "tabs",
    menuItem("Home Page", tabName = "home", icon = icon("file-text-o"), selected = TRUE),
    menuItem(
      "Data Entry",
      tabName = "dataentry",
      icon = icon("table")
    ),
    menuItem(
      "Reports",
      icon = icon("chart-line"),
      menuSubItem(
        "Report 1",
        tabName = "report1",
        icon = icon("angle-right")
      ),
      menuSubItem("Report 2", tabName = "report2", icon = icon("angle-right"))
    ),
    menuItem(
      "ReadMe",
      tabName = "readme",
      icon = icon("mortar-board")
    ),
    menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  conditionalPanel("input.tabs == 'home'|input.tabs == 'report1'",
                   fluidRow(
                     column(1),
                     column(
                       10,
                       h4("Other goodies"),
                       hr(),
                       sliderInput(
                         "plot_width",
                         "Plot width",
                         value = 3,
                         min = 1,
                         max = 5,
                         step = .5
                       ),
                       h5("save current state"),
                       bookmarkButton(),
                       hr(),
                       h5("Download Plot"),
                       downloadButton('download1', 'Download')
                     )
                   ))
)

body <-
  dashboardBody(# Define UI for application that draws a histogram

    tabItems(
      tabItem(tabName = "home",
              fluidPage(
                homePageUI("home")
              )),
      tabItem(tabName = "dataentry",
              fluidPage(
               )
    ),
      tabItem(tabName = "report1",fluidPage(
        report1UI('rpt1')
      )),
      tabItem(tabName = "report2",fluidPage()),
      tabItem(tabName = "readme",
              fluidPage(
                tags$iframe(
                  src = './readme.html',
                  width = '100%',
                  height = '800px',
                  frameborder = 0,
                  scrolling = 'auto'
                )
              )),
      tabItem(tabName = "about",
              fluidPage(
                tags$iframe(
                  src = './about.html',
                  width = '100%',
                  height = '800px',
                  frameborder = 0,
                  scrolling = 'auto'
                )
              ))

    ))


  dashboardPage(Header,
                    sidebar,
                    body
                )
}