# function to compute the rate at which new vertices can be reached for a network
#get a list of seed vertices
#for each seed
#walk the forward reachable path
#record the size of the forward reachable set
#divide by the duration of the network
reachableRate<-function(nD, start, end, seeds){
  if (missing(start) | missing(end)) {
    times <- get.change.times(nD)
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
  
  reachableRates<-sapply(seeds,function(s){
    reachableN<-sum(tPathDistance(nD,v=s,start=start,end=end)$distance<Inf)
    reachableN/(end-start)
  })
  #average over all the seeds
  return(mean(reachableRates))
}


# martina points out that this measure may suffer from the boundry condition created by network size: As time goes on, there are fewer and fewer unreached vertices for the path to discover, so its rate will slow.  Perhaps a solution is to measure the paths from sets of random start times? 