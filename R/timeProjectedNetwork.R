# create a crude version of discritized time projected graph from nD object

timeProjectedNetwork<-function(nd,start = NULL, end = NULL, time.increment = NULL, 
                               onsets = NULL, termini = NULL, ...){
  # slice up the dynamic network into a list of static networks
  nets<-get.networks(nd,start = start, end = end, time.increment = time.increment, 
                     onsets = onsets, termini = termini, 
                     retain.all.vertices=TRUE, # make sure to keep network size consistent so ids won't scramble
                     ...)
  if(is.bipartite(nd)){
    warning('input network is bipartite and projected network will not be')
  }
  if(is.hyper(nd)){
    stop('time projected network not supported for hypergraphic networks')
  }
  # init a new network of appropriate size
  projected<-network.initialize(length(nets)*network.size(nd),loops = has.loops(nd))
  # TODO: copy network-level attributes
  
  # loop over edges to create within-slice edges
  for(s in seq_along(nets)){
    el<-as.matrix.network.edgelist(nets[[s]])
    # multiply the vertex indices by s to give them for the next slice
    el<-el+(network.size(nd)*(s-1))
    add.edges(projected,tail=el[,1],head=el[,2],
              names.eval = 'edge.type',vals.eval = 'within_slice')
    # handle un-directed edges by adding arc in each direction
    if(!is.directed(nd)){
      add.edges(projected,tail=el[,2],head=el[,1],
                names.eval = 'edge.type',vals.eval = 'within_slice')
    }
  }
  # TODO: copy edge attributes from slice networks
  # copy vertex attributes from slice networks
  for(attr in list.vertex.attributes(nets[[1]])){
    set.vertex.attribute(projected,attrname=attr,
                         value=unlist(lapply(nets,get.vertex.attribute,attrname=attr,unlist=FALSE),recursive=FALSE)
    )
  }
  
  # loop again to create between-slice vertex self ties
  # TODO: omit ties as indicated by vertex activity?
  if (length(nets)>1){
    for(s in 1:(length(nets)-1)){
      tails<-1:network.size(nd)+network.size(nd)*(s-1)
      heads<-1:network.size(nd)+network.size(nd)*s
      add.edges(projected,tail=tails, head=heads, 
                names.eval = 'edge.type',vals.eval = 'identity_arc' ) 
    }
  }
  return(projected)
}