#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2014 the statnet development team
######################################################################

# utility methods for tsna stuff

# create a tree network from the results of a path search
createPaths<-function(path.results){
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
plotPaths<-function(nd,path.results,path.col="#FF000055",...){
  # plot the network normally and save coords
  coords<-plot.network(nd,...)
  # create another network that is the tree
  tree<-createPaths(path.results)
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

# helper function to determine an appropriate finite start and
# end range for the network using net.obs.period if it exists
get_bounds<-function(nd){
  bounds<-c(0,1)
  obs<-nd%n%'net.obs.period'
  if (!is.null(obs)){
    bounds<-range(unlist(obs$observations))
  } else {
    times<-get.change.times(nd)
    # its possible that network has only INFs
    if(length(times)>0){
      bounds<-range(times)
    }
  }
  return(bounds)
}