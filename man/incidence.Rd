\name{formation_and_dissolution}
\alias{tEdgeFormation}
\alias{tEdgeDissolution}

\title{
Counts or fractions of edge transitions in a networkDynamic object
}
\description{
The functions \code{tEdgeFormation} and \code{tEdgeDissolution} evaluate a network object at multiple time points and return counts (or fractions) of the number of edges forming (edge onset at time point) and dissolving (edge terminus at time point). The counts are returned as numeric vector which is a time-series object (class \code{\link[networkDynamic]{ts}}). 

}
\usage{
tEdgeFormation(nd, start, end, time.interval = 1, 
               result.type=c('count','fraction'), include.censored=FALSE)

tEdgeDissolution(nd, start, end, time.interval = 1, 
               result.type=c('count','fraction'), include.censored=FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{\link[networkDynamic]{networkDynamic}} object
}
  \item{start}{
optional numeric time value at which evaluation should start (default is first observed time)
}
  \item{end}{
optional numeric time value at which evaluation should end (default is last observed time)
}
  \item{time.interval}{
optional numeric value giving time interval between evaluations (default is 1)
}
  \item{result.type}{ either \code{'count'} indicating that results should be returned as counts of tie changes or \code{'fraction'} meaning that results should be returned as fraction of ties dissolving (for dissolution) or fraction of empty dyads forming ties (for formation). In the later case the number of dyads is corrected for network directedness, loops, and bipartite partition size. 
  
  }
  \item{include.censored}{
  logical, should ties with truncated/censored onset or termination times be included in the respective formation or dissolution counts?
  }
}
\details{
Uses \code{as.data.frame.networkDynamic} internally.  TODO: dyad formation rate is not yet corrected for vertex activity, uses the aggregate, not the momentary, network size. 

When \code{result.type='fraction'}:
\itemize{
  \item formation returns the ratio of number of  ties forming to the number of possible empty dyads that could have formed ties. So value of 1 would mean all empty dyads formed ties, value of 0 means no ties formed.  In sparse networks, the numbers will tend to be very, very small.    
  \item dissolution returns the ratio of the number of ties dissolving to the number preexisting ties that could have dissolved. So value of 1 means all ties disolved, 0 means no ties disolved. 
}

When \code{include.censored=FALSE} spells of edges which onset outside of the query range will not be included in formation counts. 

}
\value{
For \code{tEdgeFormation} and \code{tEdgeDissolution}, a numeric vector of class \code{ts} giving the formation and dissolution counts (respectively) as a time-series.  For \code{edgeFormationAt} and \code{edgeDissolutionsAt}, a single numeric value
}

\author{
skyebend@uw.edu
}
\note{
should add additional args to allow binning other than 'at' for working with non-discrete time, options to deal with how censored edges are calculated.  
}


\examples{
  library(networkDynamicData)
  data(concurrencyComparisonNets)
  # plot formation and dissolution counts time-series
  plot(tEdgeFormation(base),col='green', 
    main='edge formation and dissolution rates per timestep of base')
  points(tEdgeDissolution(base),col='red',type='l')
  \dontrun{
  # compute fraction of ties dissolving every 10 steps
  tEdgeDissolution(base,time.interval = 10,result.type = 'fraction')
  # compute fraction of empty dyads forming ties every 10 steps
  tEdgeFormation(base,time.interval = 10,result.type = 'fraction')
  }
  
}

