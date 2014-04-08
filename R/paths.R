# functions for evaluating temporal paths in networks


# this is a wrapper function to check args and call the appropriate paths method
tPathDistance<-function(nd,v, 
                 direction=c('fwd','bkwd'),
                 constraint=c('earliest.arrive', 'latest.depart'),
                 start,end,active.default=TRUE,
                 graph.step.time=0){
  
  if (!is.networkDynamic(nd)){
    stop('to be able to calculate temporal paths, the first argument must be a networkDynamic object')
  }
  if (missing(v) || !is.numeric(v)){
    stop("a 'v' argument with valid vertex ids was not given to specify starting vertex")
  }
  
  direction<-match.arg(direction)
  constraint<-match.arg(constraint)
  
  # check and determine starting and ending times
  
  # determine which version to call
  values<-NULL
  if (direction=='fwd'){
    if (constraint=='earliest.arrive'){
      values <- paths.fwd.earliest(nd=nd,v=v,start=start,end=end,active.default=active.default,graph.step.time=graph.step.time)
    } 
    
  } else {  # direction is backwards
    if (constraint=='latest.depart'){
      values <- paths.bkwd.latest(nd=nd,v=v,start=start,end=end,active.default=active.default,graph.step.time=graph.step.time)
    } 
  }
  if (is.null(values)){
    stop('unable to calculate ',direction, ' ',constraint, ' paths because the method is not yet implemented')
  }
  
  return(values)
}


# compute the forward-shortest path with a Dijkstra-style earch
# search stats from vertex v
# temporal search is bounded by 'start' and 'end' times. 

# TODO: add param for a set of alters and option to stop when they have been reached?
# TODO: add option to make direct of edge evaluation explicit?

# this finds an earliest-ending path
paths.fwd.earliest<-function(nd,v,start,end,active.default=TRUE,graph.step.time=0){
  
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
    # TODO: max distance needs a factor for graph.step.time
    if (dist[u]>= end-start){  #NOTICE:  DISTANCE IS NOT ABSOLUTE TIME  should be end-start
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
          dist_u_w<-0 
        } else {
          dist_u_w<-Inf
        }
        
      } else {
        # need to include
        splIndex<-spells.hit(needle=c(start+dist[u],end),haystack=spls)
        
        if (splIndex<0){
          dist_u_w<-Inf  # vertex is never reachable in the future / within time bound
        } else {
          # otherwise additional distance is the later of 0 or the difference between the 
          # 'current' time and the onset of the edge
          # if we are counting graph steps as part of the distance, add that in here also
          dist_u_w<-max(0,(spls[splIndex,1]-start)-dist[u]+graph.step.time)
        }
      }
      dist_v_w <-dist[u]+dist_u_w 
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
  warning("paths.bkwd.latest has not be fully tested and may not be correct")
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

# this version tries to minimize the distance of the latest time forward
# calculate the latest ending
# THIS DOES NOT WORK (and I think it can't, due to longest path problem)
paths.fwd.latestBAD<-function(nd,v,start,end,active.default=TRUE,graph.step.time=0){
  
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
    
    # we are going forwards, so use 'out' edges instead of 'in'
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
          dist_u_w<- Inf
        }
        
      } else {
        # can't use spells.hit because it returns earliest spell, not latest
        splIndex<- -1
        # loop backwards over spells so we find latest first
        for (s in nrow(spls):1) {
          if (spells.overlap(c(start+dist[u],end), spls[s, ])) {
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


# this actually calculates the forward path to find the reachable set, 
# then calculates the latest *backward path* from each reached vertex, 
# and then reverse the values
paths.fwd.latest<-function(nd,v,start,end,active.default=TRUE,graph.step.time=0)
{
  warning("paths.fwd.latest has not be fully tested and may not be correct")
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
    changes<-get.change.times(nd)
    if(length(changes)>0){
      end<-max(changes)
      # message("'start' parameter was not specified, using value first network change '",start)
    } else {
      # can't use inf, because all distances will be inf
      end<-Inf
      message("'end' time parameter for paths was not specified, no network changes found,  using end=",end)
    }
  }
  latest<-rep(Inf, network.size(nd))
  previous<-as.list(rep(0,network.size(nd)))
  # find the earliest path to reachable vertices
  fwdReachable<-which(paths.fwd.earliest(nd=nd,v=v,start=start,end=end,active.default=active.default,graph.step.time=graph.step.time)$distance<Inf)
  
  # for each reachable vertex, find the latest return path
  
  latestResults<-lapply(fwdReachable,function(w){
    backwards<-paths.bkwd.latest(nd=nd,v=w,start=start,end=end,active.default=active.default,graph.step.time=graph.step.time)
    return(list(backwards$distance[v],backwards$previous))
  })
  latest[fwdReachable]<-sapply(latestResults,'[[',1)
  previous[fwdReachable]<-lapply(latestResults,'[[',2)
  
  # because distances are from the end, need to reverse by subtracting the end time
  latest[fwdReachable]<-end-latest[fwdReachable]
  return(list(distance=latest,previous=previous))
}

