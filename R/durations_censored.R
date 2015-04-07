

edgeDurationEst <- function(nd,mode=c("right.censoring","left.truncation","both"),subject=c('edges','spells','dyads'),e=seq_along(nd$mel), start=NULL, end=NULL,active.default=TRUE, rmean=c("common","individual","extant")){
	
	requireNamespace('survival')
	
	del<-as.data.frame.networkDynamic(nd,e=e,start=start,end=end,active.default=active.default)
	# if looking per edge, group by edge id
	subject<-match.arg(subject)
	mode<-match.arg(mode)
	rmean<-match.arg(rmean)
	
	# if network has no edges, return nothing
	if (nrow(del)==0){
		return(numeric(0))
	}
	
	if(mode == "right.censoring"){
		if(rmean %in% c("common","individual")){
			# currently for left censoring, will include left truncation in future
			event.status <- !(del$onset.censored | del$terminus.censored) 
			
			# True for non-censored cases (died), FALSE for censored cases (alive)
			survRes <- survival::survfit(Surv(del$duration,event.status) ~ 1,)
			survRes$'call'<-NULL # so that it won't print the call
			print(survRes, rmean= "common")
      output<-summary(survRes,rmean='common')$table # redo the summary calcs so we can export them (hackish)
			
			# use standard restricted mean estimate in survival package
			# need to think above the use case and corresponding option
			
		} else if (rmean=="extant"){
			output<-rep(NA,9)
      names(output)<-c('records', 'n.max', 'n.start','events', '*rmean', '*se(rmean)',   'median',    '0.95LCL',    '0.95UCL')
      output[1]<-nrow(del)
			output[2]<-nrow(del)
      output[5]<-mean(del$duration[del$terminus.censored==TRUE])
      output[6]<-median(del$duration[del$terminus.censored==TRUE])
      print(output)
		}
	} else if(mode == "left.truncation"){
		
		del_org<-as.data.frame.networkDynamic(nd,e=e,start=NULL,end=NULL,active.default=active.default)
		truncation.time <- ifelse(start>del_org$onset,start-del_org$onset,0)
		
		names(truncation.time) <- del_org$edge.id
		
		truncation.time <- truncation.time[del$edge.id]
		duration.time <-  del_org$duration[del$edge.id]
		
		event.status <- !(del$onset.censored | del$terminus.censored) 
		
		survRes <- survival::survfit(Surv(truncation.time, duration.time, event.status, type= "counting") ~ 1,)
		survRes$'call'<-NULL # so that it won't print the call
		print(survRes, rmean= "common")
    output<-summary(survRes, rmean='common')$table
		
		# use standard restricted mean estimate in survival package
		# need to think above the use case and corresponding option
		
	} else if(mode == "both"){
    stop('estimating duration with right censoring and left truncation not yet supported')
	}
  invisible(output)
}