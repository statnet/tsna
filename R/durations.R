# functions for calculating and estimating edge durations

durations<-function(nd,mode=c('duration','counts'),subject=c('edges','spells','dyads'),e=seq_along(nd$mel), start=NULL, end=NULL,active.default=TRUE){
  del<-as.data.frame.networkDynamic(nd,e=e,start=start,end=end,active.default=active.default)
  # if looking per edge, group by edge id
  subject<-match.arg(subject)
  mode<-match.arg(mode)
  
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