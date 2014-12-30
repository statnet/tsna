# function to apply ergm's summary formula at multiple time points
tSummaryStats<-function(nd, f,start, end, time.interval=1){
    
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
    
    # rquires that ergm is loaded
    if(require(ergm,quietly=TRUE)){
      stats<-lapply(times,function(t){
        net<-network.collapse(nd,at=t)
        summary(as.formula(paste('net',f)))
      })
      
    } else {
      stop(" the ergm package could not be loaded to provide summary functions and terms")
    }
    # rearrange list into matrix
    stats<-do.call(rbind,stats)
    return(ts(stats,start=start,end=times[length(times)],deltat=time.interval))
}