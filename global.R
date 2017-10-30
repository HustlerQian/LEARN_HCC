rm(list=ls())
#####Data#######
rdf=read.csv('./Predication.csv',header = T,stringsAsFactors = F)
group_dic=data.frame(name=c(rdf$S_name,rdf$O_name),group=c(rdf$S_semtypes,rdf$O_semtypes),stringsAsFactors = F)

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