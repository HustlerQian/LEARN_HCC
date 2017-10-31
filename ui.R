library(timevis)
########Ui START#########
ui <- dashboardPage(
  dashboardHeader(title = "Learn_HCC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Intro',tabName = 'introduction',icon = icon('home')),
      menuItem('Graph',tabName = 'graph',icon = icon('refresh')),
      menuItem('Feedback',tabName = 'feedback',icon = icon('refresh'))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              'Need write.',
              h2('Big Pictures'),
              timevisOutput("timeline_bigpic")
      ),
      tabItem(tabName = "graph",
              fluidPage(style="padding-top: 80px;",
                        #sidebarLayout(
                        #column(2,
                        #),
                        column(10,
                               absolutePanel(
                                 top = 20, right = 20, width = 300,
                                 draggable = T,
                                 wellPanel(
                                   HTML('Just test for Drag!'),
                                   verbatimTextOutput("debug"),
                                   verbatimTextOutput("debug2"),
                                   selectInput('Relation','Relation:',unique(rdf$P_name),multiple = T,selected = 'TREATS')
                                 ),
                                 style = "opacity: 0.92"
                               ),
                               # Show a plot of the generated distribution
                               visNetworkOutput("Network")
                               
                        )
              )
      )
    )
  )
)