
#########Library#######
library(shiny)


########Server#########

# Define server logic required to draw a HCC knowledge
shinyServer(function(input, output) {
  

  output$Network <- renderVisNetwork({
    edges=links[links$label %in% input$Relation,]
    select_nodes=unique(c(edges$from,edges$to))
    nodes.df=nodes[which(nodes$id %in% select_nodes),]
    #windows()
    ntwk=visNetwork(nodes.df, edges, height = "800px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    
    ntwk
  })
})
