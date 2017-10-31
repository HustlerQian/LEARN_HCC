library(timevis)
#####Data#######
rdf=read.csv('./Predication.csv',header = T,stringsAsFactors = F)
group_dic=data.frame(name=c(rdf$S_name,rdf$O_name),group=c(rdf$S_SMTY,rdf$O_SMTY),stringsAsFactors = F)

links=data.frame(S_name=rdf$S_name,O_name=rdf$O_name,label=rdf$P_name,stringsAsFactors = F)

nodes=data.frame(name=unique(c(rdf$S_name,rdf$O_name)),stringsAsFactors = F)

#Make data description
nodes_dic=nodes
rownames(nodes_dic)=nodes$name
nodes_dic$name=0:(nrow(nodes)-1)
nodes_dic$group=NA
#unique group by name
group_dic_uniq=group_dic[rownames(unique(subset(group_dic,select='name'))),]
rownames(group_dic_uniq)=group_dic_uniq$name
nodes$group=group_dic_uniq[nodes$name,'group']



links$from=nodes_dic[links$S_name,'name']
links$to=nodes_dic[links$O_name,'name']
links$arrows = 'to'
nodes$group=1
nodes$id=0:(nrow(nodes)-1)
nodes$label=nodes$name

########Server START#########

# Define server logic required to draw a HCC knowledge
shinyServer(function(input, output) {
  
  #####INTRO SECTION#######
  output$timeline_bigpic <- renderTimevis({
    TL_bigpic=read.csv(normalizePath('./timeline_bigpics.csv'),header = T)
    timevis(TL_bigpic)
  })
  
  
  ######GRAPH SECTION#######
  output$Network <- renderVisNetwork({
    edges=links[links$label %in% input$Relation,]
    select_nodes=unique(c(edges$from,edges$to))
    nodes.df=nodes[which(nodes$id %in% select_nodes),]
    #windows()
    ntwk=visNetwork(nodes.df, edges, height = "800px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
      visInteraction(hover = TRUE) %>%
      visEvents(hoverNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}") 
    
    ntwk
  })
  #Observe edges
  observe({
    input$getEdges
    visNetworkProxy("Network") %>%
      visGetEdges()
  })
  output$debug <- renderPrint({
    input$current_node_id
  })
  output$debug2 <- renderPrint({
    input$current_edge_id
  })
})
