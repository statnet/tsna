

# find the set of vertices reachable from seed vertex within time range

forward.reachable<-function(nd,v,start=NULL,end=NULL,per.step.depth=1 ){
  if(!is.networkDynamic(nd)){
    stop('the first argument to forward.reachble must be a networkDynamic object')
  }
  if(!is.numeric(v)){
    stop('v argument to forward.reachable must be a vector of valid numeric vertex ids')
  }
  if(max(v)>network.size(nd) | min(v)<1){
    stop('v argument to forward.reachable must be a vector of valid numeric vertex ids')
  }
  if(!per.step.depth%in%c(1,Inf)){
    stop('forward.reachable currently only supports per.step.depth arguments of 1 and Inf')
  }
  
  times <-get.change.times(nd,vertex.attribute.activity=FALSE,edge.attribute.activity=FALSE,network.attribute.activity=FALSE)
  if(length(times)==0){
    times <-c(0,Inf)
  }
  if(is.null(start)){
    start<-min(times)
  }
  if(is.null(end)){
    end<-max(times)
  }
  # trim times to desired range
  times<-times[times>=start]
  times<-times[times<=end]
  
  
  reached <-v
  for(t in times){
    # BFS to depth rate
    # find out-connected vertices
    ngs<-unique(sapply(v,function(i){get.neighborhood.active(nd,v=i,at=t,type='out')}))
    new<-setdiff(ngs,reached)
    reached<-c(reached,new)
    # remove any in the set we've already visited
    if (per.step.depth > 1 & length(new)>0){
      d<-1
      while(d <= per.step.depth){
        ngs<-unique(sapply(new,function(i){get.neighborhood.active(nd,v=i,at=time,type='out')}))
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