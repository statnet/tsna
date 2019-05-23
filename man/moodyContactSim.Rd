%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{moodyContactSim}
\alias{moodyContactSim}
\docType{data}
\title{
Jim Moody's example dynamic contact simulation network
}
\description{
A \code{networkDynamic} object containing the output of a simulation of 1000 timestep simulation of a sex contact network with 16 vertices and 18 edges. Each edge has a single activity spell. 
}
\usage{data(moodyContactSim)}
\format{
  A \code{networkDynamic} object.
}
\details{
The object has a \code{net.obs.period} attribute describing the observation model. This is a useful network for testing path-based algorithms because it is small enough to visually inspect. 
}
\source{
Figure 5 of James Moody (2008) "Static Representations of Dynamic Networks" Duke Population Research Institute
On-line Working Paper Series. \code{http://papers.ccpr.ucla.edu/papers/PWP-DUKE-2009-009/PWP-DUKE-2009-009.pdf}
}

\examples{
data(moodyContactSim)
# plot a view of network with edge and vertex labels
plot(moodyContactSim,
     displaylabels=TRUE,
     label.cex=0.8,
     label.pos=5,
     vertex.col='white',
     vertex.cex=2,
     edge.label=sapply(get.edge.activity(moodyContactSim),function(e){
       paste('(',e[,1],'-',e[,2],')',sep='')
     }),
     edge.label.col='blue',
     edge.label.cex=0.8
   )
\dontrun{
# data object was created with
moodyContactSim<-network.initialize(16,directed=FALSE)
tel<-matrix(c(674,701,1,9,
              214,247,1,11,
              621,651,1,12,
              583,615,1,16,
              749,793,11,8,
              719,745,8,13,
              712,739,13,5,
              634,660,13,3,
              769,795,13,7,
              453,479,13,4,
              494,524,13,2,
              224,256,7,10,
              40,72,10,4,
              665,692,4,14,
              709,740,2,15,
              575,599,2,16,
              748,782,4,16,
              701,733,16,6),
            ncol=4,byrow=TRUE)
moodyContactSim<-networkDynamic(moodyContactSim,edge.spells=tel)
obs<-moodyContactSim\%n\%'net.obs.period'
obs$mode<-'discrete'
obs$time.increment<-1
obs$time.unit<-'step'
obs$observations<-list(c(0,1000))
moodyContactSim\%n\%'net.obs.period'<-obs
}   
}
\keyword{datasets}
