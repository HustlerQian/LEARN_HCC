library(timevis)
########Ui START#########
ui <- dashboardPage(
  dashboardHeader(title = "Learn_HCC"),
  ########MENU########
  dashboardSidebar(
    #Search function need exploration
    #sidebarSearchForm(label = "Search for Gene or Biomarker:", "searchText", "searchButton"),
    #progressBar(id = "progress01", value = 50),
    sidebarMenu(
      #Introdution
      menuItem('Intro',tabName = 'introduction',icon = icon('home')),
      menuItem('Search',tabName = 'search',icon = icon('search')),
      #Graph section
      menuItem('Graph',tabName = 'graph',icon = icon('wikipedia-w'),
      #SubItem
        menuItem('Network',tabName = 'network',icon = icon('wikipedia-w')),
        menuSubItem('Barchart',tabName = 'barchart',icon = icon('bar-chart')),
        menuSubItem('DT',tabName = 'table',icon = icon('table'))
      ),
      menuItem('Feedback',tabName = 'feedback',icon = icon('refresh')),
      #menuItem('Adv',tabName = 'adv',icon = icon('refresh')),
      menuItem('Filter',tabName = 'filter',icon = icon('filter'),
               selectInput('Relation','Relation:',Relation_raw,multiple = T,selected = c('TREATS','AFFECTS','ASSOCIATED_WITH')),
               uiOutput('Group')
               )
    )
  ),
  ##########BODY############
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              #'Need write.',
              h2('Big Pictures'),
              HTML("<p><a href='http://www.ncbi.nlm.nih.gov/pubmed/?term=10496524' target='_blank'>The       observed altered gene expression may be related to biological phenotypes of       hepatic tumors, and IL-1ra in particular may positively influence tumor cell       growth through its antagonism of IL-1.</br></a><font color='red'>hepatic tumors</font> <font color='blue'>influence </font> <font color='green'>tumor cell       growth</font></p>"),
              timevisOutput("timeline_bigpic")
      ),
      tabItem(tabName = "search",
              #h2('Big Pictures')
              box(
                title = 'Plz choose entity:',
                width = 4
              ),
              box(
                title = 'Proof'
              ),
              box(
                title = 'Summary'
              )
      ),
      tabItem(tabName = "network",
              fluidPage(style="padding-top: 10px;",
                        column(10,
                               
                        # Show a plot of the generated distribution
                               visNetworkOutput("Network",height ="600px"),
                              br()
                               
                        ),
                        # Show a summary for the network
                        box(
                              title='Infobox',
                              width = 2,
                              h3('Summary'),
                              verbatimTextOutput('debug'),
                              #dataTableOutput('prooflist'),
                              #Prooflist
                              conditionalPanel('input.Network_selected',
                                               HTML("<p><a href='http://www.ncbi.nlm.nih.gov/pubmed/?term=10582682' target='_blank'>See more.</br></a><font color='red'>Bcl10</font> <font color='blue'>in </font> <font color='green'>solid tumors</font></p>")
                                               
                                               ),
                              absolutePanel(
                                top = 20, right = 20, width = 300,
                                draggable = T,
                                wellPanel(
                              HTML('Custom for Visualization')
                              ),
                           style = "opacity: 0.92"
                          )
                               )
              ),
              br(),
              #Trend for paper in the graph
              dygraphOutput('Trend',height = '150px')
      ),
      tabItem(tabName = "barchart",
              'Need write.',
              tabBox(
                title = "Barplot",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "Barplot1",width = 8,
              tabPanel('Relationship',plotOutput("barplot_relation")),
              tabPanel('Group',plotOutput("barplot_group"))

              )
      ),
      tabItem(tabName = "table",
              'Need write.',
              dataTableOutput("links.dt"),
              dataTableOutput("edges.dt")
      ),
      tabItem(tabName = "feedback",
              'Need write.',
              textAreaInput('Make a post:','Post')
              
      ),
      tabItem(tabName = "adv",
              'Need write.',
              HTML('<p>广告：<br>
                   </p>')
              )
    )
  )
)