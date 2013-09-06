

# find the set of vertices reachable from seed vertex within time range

forward.reachable<-function(nd,v,start=NULL,end=NULL,per.step.depth=Inf ){
  if(!is.networkDynamic(nd)){
    stop('the first argument to forward.reachble must be a networkDynamic object')
  }
  if(!is.numeric(v)){
    stop('v argument to forward.reachable must be a vector of valid numeric vertex ids')
  }
  if(max(v)>network.size(nd) | min(v)<1){
    stop('v argument to forward.reachable must be a vector of valid numeric vertex ids')
  }
  
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
      while(d <= per.step.depth*duration){
        ngs<-unlist(unique(sapply(new,function(i){get.neighborhood.active(nd,v=i,at=times[t],type='out')})))
        new<-setdiff(ngs,reached)
        if(length(new)==0){
          break # no more verts to find
        }
        reached<-c(reached,new)
        d<-d+1
      }
    }
  }
  return(reached)
}