

# find the set of vertices reachable from seed vertex within time range

forward.reachable<-function(nd,v,start=NULL,end=NULL,per.step.depth=Inf ){
  if(!is.networkDynamic(nd)){
    stop('the first argument to forward.reachble must be a networkDynamic object')
  }
  if(missing(v) || !is.numeric(v)){
    stop('v argument to forward.reachable must be a vector of valid numeric vertex ids')
  }
  if(max(v)>network.size(nd) | min(v)<1){
    stop('v argument to forward.reachable must be a vector of numeric vertex ids within the range of the network size')
  }
  
  # set the interval to be whatever the observed changes are
  times <-get.change.times(nd,vertex.attribute.activity=FALSE,edge.attribute.activity=FALSE,network.attribute.activity=FALSE)

  if(length(times)==0){
    times <-c(0,Inf)
  }
  if(is.null(start)){
    #start<-min(times)
    start<--Inf
  }
  if(is.null(end)){
    #end<-max(times)
    end<-Inf
  }
  # trim times to desired range, making sure to include start and end
  times<-unique(c(start,times[times>=start]))
  times<-unique(c(times[times<=end],end))
  
  distance<-rep(Inf,network.size(nd))
  distance[v]<-times[1]
  
  #TODO: could probably skip all times earlier that the active times in v?
  reached <-v
  for(t in 1:(length(times)-1)){
    # BFS to depth rate
    new<-reached
    # how long until next change?
    duration<-times[t+1]-times[t]
    
    # remove any in the set we've already visited
    if (duration>0){
      d<-1 # we are assuming all geodesic steps count as 1, harder if we calc per edge..
      # keep searching until we reach bounds or run out of verts to find
      # also stop if we find all the vertices
      while(d <= per.step.depth*duration & length(reached)<network.size(nd)){
        ngs<-unlist(unique(sapply(new,function(i){get.neighborhood.active(nd,v=i,at=times[t],type='out')})))
        new<-setdiff(ngs,reached)
        if(length(new)==0){
          break # no more verts to find
        }
        distance[new]<-times[t]
        reached<-c(reached,new)
        d<-d+1
      }
    }
  }
  return(reached)
}

forward.reachable2<-function(nd,v,start=NULL,end=NULL,interval='changes',per.step.depth=Inf ){
  if(!is.networkDynamic(nd)){
    stop('the first argument to forward.reachble must be a networkDynamic object')
  }
  if(missing(v) || !is.numeric(v)){
    stop('v argument to forward.reachable must be a vector of valid numeric vertex ids')
  }
  if(max(v)>network.size(nd) | min(v)<1){
    stop('v argument to forward.reachable must be a vector of numeric vertex ids within the range of the network size')
  }
  
  # if start or end is missing set the interval to be whatever the observed changes are
  if (is.null(start)){
    start <-min(get.change.times(nd,vertex.attribute.activity=FALSE,edge.attribute.activity=FALSE,network.attribute.activity=FALSE))
  }
  if (is.null(end)){
    end <-max(get.change.times(nd,vertex.attribute.activity=FALSE,edge.attribute.activity=FALSE,network.attribute.activity=FALSE))
  }
    
  # lets not loop for ever!
  if (is.infinite(start) | is.infinite(end)){
    stop("start and end values cannot be infinite because search will not terminate")
  }
  
  #TODO: could probably skip all times earlier that the active times in v?
  reached <-v
  now<-start
  while(now<end){
    # BFS to depth rate
    new<-reached
    # how long until next change?
    nextTime<-when.next.edge.change(nd,at=now,v=reached)
    duration<-nextTime-now
    
    # remove any in the set we've already visited
    if (duration>0){
      d<-1 # we are assuming all geodesic steps count as 1, harder if we calc per edge..
      # keep searching until we reach bounds or run out of verts to find
      # also stop if we find all the vertices
      while(d <= per.step.depth*duration & length(reached)<network.size(nd)){
        ngs<-unlist(unique(sapply(new,function(i){get.neighborhood.active(nd,v=i,at=now,type='out')})))
        new<-setdiff(ngs,reached)
        if(length(new)==0){
          break # no more verts to find
        }
        reached<-c(reached,new)
        d<-d+1
      }
    }
    # update time
    now<-nextTime
  }
  return(reached)
}


# compute the forward-shortest path with a Dijkstra-style earch
# search stats from vertex v
# temporal search is bounded by 'start' and 'end' times. 

# TODO: add param for a set of alters and option to stop when they have been reached?
# TODO: add option to make direct of edge evaluation explicit?

paths.fwd.earliest<-function(nd,v,start,end,active.default=TRUE,graph.step.time=0){
  
  if (!is.networkDynamic(nd)){
    stop('to be able to calculate forward paths, the first argument must be a networkDynamic object')
  }
  if (missing(v) || !is.numeric(v)){
    stop("a 'v' argument with valid vertex ids was not given to specify starting vertex")
  }
  if (missing(start)){
     # TODO: use obs.period if it exists
      changes<-get.change.times(nd)
      if(length(changes)>0){
        start<-min(changes)
        # message("'start' parameter was not specified, using value first network change '",start)
      } else {
        # can't use inf, because all distances will be inf
        start<-0
        message("'start' time parameter for paths was not specified, no network changes found,  using start=",start)
      }
  }
  if (missing(end)){
    # TODO: use obs.period if it exists
    end<-Inf
  }
  
  
  # TODO: self-loop behavior?
  # TODO: multiplex behavior?
  
  dist<-rep(Inf,network.size(nd))
  previous<-numeric(network.size(nd)) # array used for reconstructing the path
  dist[v]<-0
  toCheck<-rep(TRUE,network.size(nd))
  while(sum(toCheck)>0){
    minToCheck<-which.min(dist[toCheck])
    # have to translate index found back
    u<-which(toCheck)[minToCheck]
    toCheck[u]<-FALSE
    if (dist[u]>= end){  #TODO: DANGER DISTANCE IS NOT ABSOLUTE TIME  should be end-start
      break;  # no more vertices are reachable from v within time range
    }
    
    nghE<-get.edgeIDs(nd,v=u,neighborhood='out') # check neighbors of u
    for (e in nghE){
      w <- ifelse(nd$mel[[e]]$inl==u,nd$mel[[e]]$outl,nd$mel[[e]]$inl)   
      # we ignore graph hop time
      # so "distance" is how long we have to wait from 'now' until onset of edge
      spls<-nd$mel[[e]]$atl$active
      if (is.null(spls)){ # handle possibly missing activity value, assume always active
        if (active.default){
          dist_u_w<-0 #TODO: need to check active default here to know if returning Inf or dist[w]
        } else {
          dist_u_w<-Inf
        }
        
      } else {
        # need to include
        splIndex<-spells.hit(needle=c(start+dist[u],end),haystack=spls)
        
        if (splIndex<0){
          dist_u_w<-Inf  # vertex is never reachable in the future / within time bound
        } else {
         # otherwise distance is the later of dist[u] or the onset of the edge
         dist_u_w<-max(0,(spls[splIndex,1]-start)-dist[u]+graph.step.time)
        }
      }
      dist_v_w <-dist[u]+dist_u_w # TODO: could add graph hop time in here
      if (dist_v_w < dist[w]){ # if this new value is shorter, update
        dist[w]<-dist_v_w
        previous[w]<-u
      }
    }
  }
  return(list(distance=dist,previous=previous))
}

# I think to avoid getting into "longest path problem" territory, 
# need to start at the end and minimize backwards

paths.bkwd.latest<-function(nd,v,start,end,active.default=TRUE,graph.step.time=0){
  
  if (!is.networkDynamic(nd)){
    stop('to be able to calculate forward paths, the first argument must be a networkDynamic object')
  }
  if (missing(v) || !is.numeric(v)){
    stop("a 'v' argument with valid vertex ids was not given to specify starting vertex")
  }
  if (missing(end)){
    # TODO: use obs.period if it exists
    changes<-get.change.times(nd)
    if(length(changes)>0){
      end<-max(changes)
      # message("'start' parameter was not specified, using value first network change '",start)
    } else {
      stop("'end' time parameter for paths was not specified, no network changes found")
    }
  }
  if (missing(start)){
    # TODO: use obs.period if it exists
    start<- -Inf
  }
  
  
  # TODO: self-loop behavior?
  # TODO: multiplex behavior?
  
  dist<-rep(Inf,network.size(nd))
  previous<-numeric(network.size(nd)) # array used for reconstructing the path
  dist[v]<-0
  toCheck<-rep(TRUE,network.size(nd))
  while(sum(toCheck)>0){
    minToCheck<-which.min(dist[toCheck])
    # have to translate index found back
    u<-which(toCheck)[minToCheck]
    toCheck[u]<-FALSE
    if (dist[u]>= end-start){
      break;  # no more vertices are reachable from v within time range
    }
    
    # we are going backwards, so use 'in' edges instead of 'out'
    nghE<-get.edgeIDs(nd,v=u,neighborhood='in') # check neighbors of u
    for (e in nghE){
      w <- ifelse(nd$mel[[e]]$inl==u,nd$mel[[e]]$outl,nd$mel[[e]]$inl)   
      # we ignore graph hop time
      # so "distance" is how long we have to wait from 'now' until onset of edge
      spls<-nd$mel[[e]]$atl$active
      if (is.null(spls)){ # handle possibly missing activity value, assume always active
        if (active.default){
          dist_u_w<-0 #TODO: need to check active default here to know if returning Inf or dist[w]
        } else {
          dist_u_w<- Inf
        }
        
      } else {
        # can't use spells.hit because it returns earliest spell, not latest
        splIndex<- -1
        # loop backwards over spells so we find latest first
        for (s in nrow(spls):1) {
          if (spells.overlap(c(start,end-dist[u]), spls[s, ])) {
            splIndex<-s
            break
          }
        }
        
        if (splIndex<0){
          dist_u_w<- Inf  # vertex is never reachable in the future / within time bound
        } else {
          # otherwise distance is the later of dist[u] or the terminus of the edge
          dist_u_w<-max(0,(spls[splIndex,2]-end)*-1-dist[u]+graph.step.time)
        }
      }
      dist_v_w <-dist[u]+dist_u_w 
      if (dist_v_w < dist[w]){ # if this new value is shorter, update
        dist[w]<-dist_v_w
        previous[w]<-u
      }
    }
  }
  
  # TODO: we are measuring distance backwards from the end
  # so need to flip distance measure
  
  return(list(distance=dist,previous=previous))
}


# compute the sets of vertices reachable from each vertex on the graph
reachable_set_sizes<-function(nd,direction='fwd',sample=FALSE){
  if (is.numeric(sample)){
    seeds<-sample.int(network.size(nd),size=sample)
  } else {
    seeds<-seq_len(network.size(nd))
  }
  
  sizes<-sapply(seeds,function(v){
    sum(paths.fwd.earliest(nd,v=v)$distance<Inf)
    })
  return(sizes)
}

