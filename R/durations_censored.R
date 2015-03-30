#library(networkDynamic,lib=trunk)
#library(tsna,lib=trunk)
#require(testthat)
#require(networkDynamicData)
#data(moodyContactSim)
#nd=moodyContactSim
#require(survival)
#subject="edges"
#start=60
#end=500
#e=seq_along(nd$mel)
#active.default=TRUE

#summaryDuration.censor(nd,start=200,end=500,mode="left truncation")
#summaryDuration.censor(nd,start=200,end=500,rmean="extant")


summaryDuration.censor <- function(nd,mode=c("right censoring","left truncation","both"),subject=c('edges','spells','dyads'),e=seq_along(nd$mel), start=NULL, end=NULL,active.default=TRUE, rmean=c("common","individual","extant")){
	
	require(survival)
	
	del<-as.data.frame.networkDynamic(nd,e=e,start=start,end=end,active.default=active.default)
	# if looking per edge, group by edge id
	subject<-match.arg(subject)
	mode<-match.arg(mode)
	rmean<-match.arg(rmean)
	
	# if network has no edges, return nothing
	if (nrow(del)==0){
		return(numeric(0))
	}
	
	if(mode == "right censoring"){
		if(rmean %in% c("common","individual")){
			# currently for left censoring, will include left truncation in future
			event.status <- !(del$onset.censored | del$terminus.censored) 
			
			# True for non-censored cases (died), FALSE for censored cases (alive)
			survRes <- survfit(Surv(del$duration,event.status) ~ 1,)
			
			as.list(print(survRes, rmean= c("common"))$call)
			
			# use standard restricted mean estimate in survival package
			# need to think above the use case and corresponding option
			
		} else if (rmean=="extant"){
			mean(del$duration[del$terminus.censored==TRUE])		
		}
	}
	
	if(mode == "left truncation"){
		
		del_org<-as.data.frame.networkDynamic(nd,e=e,start=NULL,end=NULL,active.default=active.default)
		truncation.time <- ifelse(start>del_org$onset,start-del_org$onset,0)
		
		names(truncation.time) <- del_org$edge.id
		
		truncation.time <- truncation.time[del$edge.id]
		duration.time <-  del_org$duration[del$edge.id]
		
		event.status <- !(del$onset.censored | del$terminus.censored) 
		
		survRes <- survfit(Surv(truncation.time, duration.time, event.status, type= "counting") ~ 1,)
		
		as.list(print(survRes, rmean= c("common"))$call)
		
		# use standard restricted mean estimate in survival package
		# need to think above the use case and corresponding option
		
	}
	
	if(mode == "both"){
		return()
	}
	
}