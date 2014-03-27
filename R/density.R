# functions for computing temporal density

# if regular graph density is the fraction of possible ties that exist
# temporal density would be the fraction of total possible tie-time that is active
# OR  fraction of time existing ties are active?


# TODO: should self-loops be counted? Only if loops=TRUE?
# TODO: what about multiplex networks?

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


# how many events are there per time step within the graph time period
edge_event_density<-function(nd){
  
  # trap some edge cases
  if (network.edgecount(nd)==0 | network.size(nd)==0){
    return(0)
  }
  bounds<-get_bounds(nd)
  # count the number of events occuring
  spls<-get.edge.activity(nd)
  nulls<-sapply(spls, is.null)
  counts<-sapply(spls[!nulls], nrow)
  numSpells<-sum(counts)
  # TODO: don't count always active or always inactive because they don't change
  return(numSpells/(network.edgecount(nd)*bounds[2]-bounds[1]))
}



dyad_duration_density<-function(nd,active.default=TRUE){
  # trap some edge cases
  if (network.edgecount(nd)==0 | network.size(nd)==0){
    return(0)
  }
  bounds<-get_bounds(nd)
  spls<-as.data.frame.networkDynamic(nd,start=bounds[1],end=bounds[2],active.default=active.default)
  total_dur<-sum(spls$duration)
  # total number of possible dyads
  num_dyads<-network.dyadcount(nd)
  # correct for self loops
  if (has.loops(nd)){
    num_dyads<-num_dyads+network.size(nd)
  }
  return(total_dur/(num_dyads*(bounds[2]-bounds[1])))
}

edge_duration_density<-function(nd,active.default=TRUE){
  # trap some edge cases
  if (network.edgecount(nd)==0 | network.size(nd)==0){
    return(NA)
  }
  bounds<-get_bounds(nd)
  spls<-as.data.frame.networkDynamic(nd,start=bounds[1],end=bounds[2],active.default=active.default)
  total_dur<-sum(spls$duration)
  return(total_dur/(network.edgecount(nd)*(bounds[2]-bounds[1])))
}