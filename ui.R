
#########Library#######
library(shiny)
library(visNetwork)
library(shinydashboard)

# #########UI###########
# 
# # Define UI for application that draws a HCC knowledge
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("KnowledgeBase for HCC Demo"),
#   
#   # Sidebar with a slider input for number of bins 
#   #sidebarLayout(
#   column(2,
#          selectInput('Relation','Relation:',unique(rdf$P_name),multiple = T)
#   ),
#   
#   # Show a plot of the generated distribution
#   column(10,
#          visNetworkOutput("Network")
#   )
#   #)
# ))

ui <- dashboardPage(
  dashboardHeader(title = "Learn_HCC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Intro',tabName = 'introduction',icon = icon('home')),
      menuItem('Graph',tabName = 'graph',icon = icon('refresh'))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              'Need write.'
              ),
      tabItem(tabName = "graph",
              fluidRow(
                #sidebarLayout(
                column(2,
                       selectInput('Relation','Relation:',unique(rdf$P_name),multiple = T)
                ),
                # Show a plot of the generated distribution
                column(10,
                       visNetworkOutput("Network")
                                )
              )
      )
    )
  )
)