
# the function is for bookmarking, needs to be at top of page
# consider other methods if no bookmarking
# ui <- fluidPage(
#   ...
# )
# or
# ui <- ..

ui <- function(request){

page_navbar(
  title = "Template App",
  id = "nav_id",
  navbar_options = navbar_options(
    bg = "#0062cc"
  ),
  navset_pill_list(
    id = "tabs",
    selected = "home",
    nav_panel(
      "Home Page",
      value =  "home",
      icon = icon("file-text-o"),
      homePageUI("home")
      ),
    nav_panel(
      "Data Entry",
      value =  "dataEntry",
      icon = icon("table"),
      h2("Data Entry Content")
    ),
    nav_menu(
      "Reports",
      icon = icon("chart-line"),
      nav_panel(
        "Report 1",
        value = "report1",
        icon = icon("angle-right"),
        report1UI('rpt1')
        ),
      nav_panel(
        "Report 2",
         value = "report2",
         icon = icon("angle-right"),
         h2("Report 2 Content")
         )
    ),
    nav_panel(
      "ReadMe",
      value = "readme",
      icon = icon("mortar-board"),
      tags$iframe(
        src = 'readme.html',
        width = '100%',
        height = '800px',
        frameborder = 0,
        scrolling = 'auto'
      )
    ),
    nav_panel(
      "About",
      value = "about",
      icon = icon("question"),
      tags$iframe(
       src = 'about.html',
       width = '100%',
       height = '800px',
       frameborder = 0,
       scrolling = 'auto'
      )
    ),
  hr(),
  conditionalPanel(
    condition = "input.tabs == 'report1'||input.tabs == 'report2'",
    h4("Other goodies"),
    sliderInput("plot_width", "Plot width", value = 3, min = 1, max = 5, step = .5),
    downloadButton('download1', 'Download'),
    hr(),
    bookmarkButton()
   )
)
)}