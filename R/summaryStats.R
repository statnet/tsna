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

# function to provide a wrapper for calling sna measures
tSnaStats<-function(nd, snafun,start, end, time.interval=1,...){
  
  # table of supported sna functions and key terms 
  # (i.e is directedness arg named 'mode' or 'gmode' )
                      # fun name  directed, type, diag
  funTerms<- matrix(c('closeness','gmode', 'VLI', 'diag',
                      'betweenness','gmode','VLI','diag',
                      'bonpow',    'gmode','VLI','diag',
                      'components', '', 'GLI', '',
                      'degree','gmode','VLI','diag',
                      'triad.census','mode','other','',
                      'connectedness','','GLI','',
                      'dyad.census','','other','',
                      'efficiency','','GLI','diag',
                      'evcent','gmode','VLI','diag',
                      'flowbet','gmode','VLI','diag',
                      'gden'     ,'mode',  'GLI', 'diag',
                      'graphcent', 'gmode','VLI','diag',
                      'grecip','','GLI','',
                      'gtrans','mode','GLI','diag',
                      'infocent','gmode','VLI','diag',
                      'hierarchy','','GLI','',
                      'loadcent','gmode','VLI','diag',
                      'lubness','','GLI','',
                      'mutuality','','GLI','',
                      'prestige','gmode','VLI','diag',
                      'centralization','mode','GLI','diag'
                     ),ncol=4,byrow=TRUE)
  
  if (!is.character(snafun)){
    stop('the "snafun" argument must be a character string giving the name of one of the supported sna package descriptive statistics')
  }
  if (!snafun%in%funTerms[,1]){
    stop('the function "', snafun,'" is not one of the sna package descriptive statistics currently supported')
  }
  
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
  
  args<-list(...)
  
  # rquires that sna package is loaded
  if(require(sna,quietly=TRUE)){
    stats<-lapply(times,function(t){
      # extract the network for the time
      net<-network.collapse(nd,at=t)
      # sna functions can't handle zero-order networks
      if(network.size(net)==0){
        return(NA)
      }
      args<-c(dat=list(net),args)
      # construct appropriate args list
      # a bit messy because sometimes named mode, sometimes gmode
      directTerm<-funTerms[which(funTerms[,1]==snafun),2]
      if(!directTerm%in%names(args)){
        if(directTerm=='mode'){
          if(is.directed(net)){
            args<-c(args,mode='digraph')
          } else {
            args<-c(args,mode='graph')
          }
        } else if (directTerm=='gmode'){
          if(is.directed(net)){
            args<-c(args,gmode='digraph')
          } else {
            args<-c(args,gmode='graph')
          }
        }
        # otherwise don't add a derm for directedness 
      }
      diagTerm<-funTerms[which(funTerms[,1]==snafun),4]
      if(diagTerm=='diag' && !'diag'%in%names(args)){
        if(has.loops(net)){
          args<-c(args,diag=TRUE)
        } else {
          args<-c(args,diag=FALSE)
        }
      }
      
      do.call(snafun,args = args)

    })
    
  } else {
    stop(" the sna package could not be loaded to provide summary functions")
  }
  # rearrange list into matrix
  # TODO: this may not work if the sizesof networks vary
  stats<-do.call(rbind,stats)
  # if it is producing one statistic per vertex, name columns appropriately
  if (funTerms[which(funTerms[,1]==snafun),3]=='VLI'){
    stats<-ts(stats,start=start,end=times[length(times)],deltat=time.interval,names=network.vertex.names(nd))
  } else {
    stats<-ts(stats,start=start,end=times[length(times)],deltat=time.interval)
  }
  return(stats)
}