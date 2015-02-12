#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2014 the statnet development team
######################################################################

# functions for calculating and estimating edge durations

tEdgeDuration<-function(nd,mode=c('duration','counts'),subject=c('edges','spells','dyads'),e=seq_along(nd$mel), start=NULL, end=NULL,active.default=TRUE){
  del<-as.data.frame.networkDynamic(nd,e=e,start=start,end=end,active.default=active.default)
  # if looking per edge, group by edge id
  subject<-match.arg(subject)
  mode<-match.arg(mode)
  
  # if network has no edges, return nothing
  if (nrow(del)==0){
    return(numeric(0))
  }
  
  # determine function type for aggregation
  aggFun<-'sum' # function to use for aggregation sum = 'duration'
  if (mode=='counts'){
    aggFun<-'length'  # function to use to count events
  }
  
  # determine unit of aggragation
  if (subject=='edges'){
    del<-aggregate.data.frame(del[,c('duration','edge.id')],by=list(edges=del$edge.id),FUN=aggFun)
  } else if (subject=='dyads'){
  # if looking per dyad, group by tail,head pair
    if (is.hyper(nd)){
      stop('dyad-based comparison is not appropriate for hypergraphic networks')
    }
    del<-aggregate.data.frame(del[,c('duration','tail','head')],by=list(dyads=del$tail,del$head),FUN=aggFun)
  } else {
  # if looking at spells, just use the raw frame
    del<-aggregate.data.frame(del[,'duration',drop=FALSE],by=list(seq_len(nrow(del))),FUN=aggFun) 
  }
    
  return(del$duration)
  
}


tEdgeFormation<-function(nd, start, end, time.interval=1){
    
    if(missing(start) | missing(end)){
      times <- get.change.times(nd)
      if (length(times) == 0) {
        warning("network does not appear to have any dynamic information. Using start=0 end=1")
        start = 0
        end = 0
      }
      times[times == Inf] <- NA
      times[times == -Inf] <- NA
      start = min(times, na.rm = T)
      end = max(times, na.rm = T)
    }
    
    # figure out the times where we will do evaluations
    times<-seq(from = start, to=end,by = time.interval)
    
    tel<-as.data.frame.networkDynamic(nd)
    formation<-sapply(times,function(t){sum(tel$onset==t)})
    return(ts(formation,start=start,end=times[length(times)],deltat=time.interval))
}

tEdgeDissolution<-function(nd, start, end, time.interval=1){
  
  if(missing(start) | missing(end)){
    times <- get.change.times(nd)
    if (length(times) == 0) {
      warning("network does not appear to have any dynamic information. Using start=0 end=1")
      start = 0
      end = 0
    }
    times[times == Inf] <- NA
    times[times == -Inf] <- NA
    start = min(times, na.rm = T)
    end = max(times, na.rm = T)
  }
  
  # figure out the times where we will do evaluations
  times<-seq(from = start, to=end,by = time.interval)
  
  tel<-as.data.frame.networkDynamic(nd)
  dissolution<-sapply(times,function(t){sum(tel$terminus==t)})
  return(ts(dissolution,start=start,end=times[length(times)],deltat=time.interval))
}

edgeFormationAt<-function(nd,at){
  tel<-as.data.frame.networkDynamic(nd)
  return(sum(tel$onset==at))
}

edgeDissolutionAt<-function(nd,at){
  tel<-as.data.frame.networkDynamic(nd)
  return(sum(tel$terminus==at))
}

# return the total amount of time that each vertex was connected via active edges
connectedDuration<-function(nd, active.default=TRUE,neighborhood=c('out','in','combined')){
  neighborhood<-match.arg(neighborhood)
  if(!is.directed(nd)){
    neighborhood<-'combined'
  }
  bounds<-get_bounds(nd)
  durations<-rep(0,network.size(nd))
  if(neighborhood=='out'){
    spls<-as.data.frame.networkDynamic(nd,start=bounds[1],end=bounds[2],active.default=active.default)
    connectDur<-aggregate(spls['duration'],list(spls$tail),sum)
    durations[connectDur[,1]]<-connectDur[,2]
  } else if (neighborhood=='in'){
    spls<-as.data.frame.networkDynamic(nd,start=bounds[1],end=bounds[2],active.default=active.default)
    connectDur<-aggregate(spls['duration'],list(spls$head),sum)
    durations[connectDur[,1]]<-connectDur[,2]
  } else {  # ngh is combined
    spls<-as.data.frame.networkDynamic(nd,start=bounds[1],end=bounds[2],active.default=active.default)
    connectDurHead<-aggregate(spls['duration'],list(spls$head),sum)
    connectDurTail<-aggregate(spls['duration'],list(spls$tail),sum)
    durations[connectDurHead[,1]]<-connectDurHead[,2]
    durations[connectDurTail[,1]]<-durations[connectDurTail[,1]]+connectDurTail[,2]
  }
  return(durations)
}


# how much model clock time does it take on average for a single edge to change?
# divide the duration by count the number of non-censored toggles in the network
meanTimeToChange<-function(nD){
  tel<-as.data.frame.networkDynamic(nD)
  bounds<-get_bounds(nD)
  changeCount<-sum(!tel$onset.censored)+sum(!tel$terminus.censored)
  return((bounds[2]-bounds[1])/changeCount  )
}

