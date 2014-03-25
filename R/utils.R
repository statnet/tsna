# utility methods for tsna stuff

# create a tree network from the results of a path search
create_tree<-function(path.results){
  distance<-path.results$distance
  previous<-path.results$previous
  vids<-which(distance<Inf)
  n<-length(vids)
  tree<-network.initialize(n,directed=TRUE)
  network.vertex.names(tree)<-vids
  for(v in seq_along(vids)){
    
    if(previous[vids[v]]!=0){ # source vertex will have previous id of 0, so out of range
      fromId<-match(previous[vids[v]],vids)
      add.edges.active(tree,tail=fromId,head=v,onset=distance[vids[v]],terminus=Inf)
    }
  }
  return(tree)
}

# plot a network with a hilited path
# and some sensible defaults
plotpath<-function(nd,path.results,path.col="#FF000055",...){
  # plot the network normally and save coords
  coords<-plot.network(nd,...)
  # create another network that is the tree
  tree<-create_tree(path.results)
  # get an appropriate coordinate subset
  treeCoords<-coords[which(path.results$distance<Inf),]
  # get a set of onset times as edge labels
  edgeTimes<-sapply(get.edge.activity(tree),'[',1)
  # plot the tree as an overlay
  plot.network(tree,coord=treeCoords,
               edge.lwd=10,
               edge.col=path.col,
               edge.label=edgeTimes,
               edge.label.col=path.col,
               edge.label.cex=0.7,
               new=FALSE,vertex.cex=0,
               jitter=FALSE)
  invisible(coords)
}