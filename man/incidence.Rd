\name{incidence_and_dissolution}
\alias{tEdgeIncidence}
\alias{tEdgeDissolution}
\alias{edgeIncidenceAt}
\alias{edgeDissolutionAt}
\title{
functions to calculate incidence (onset) or dissolution (termination) counts of edges in a networkDynamic object
}
\description{
The functions \code{tEdgeIncidence} and \code{tEdgeDissolution} evaluate a network object at multiple time points and return counts of the number of edges forming (edge onset at time point) and dissolving (edge terminus at time point). The counts are returned as numeric vector which is a time-series object (class \code{\link{ts}})

The functions \code{edgeIncidenceAt} and \code{edgeDissolutionsAt} perform similar calculations, but only at a single time point and return only a single value. 
}
\usage{
tEdgeIncidence(nd, start, end, time.interval = 1)
tEdgeDissolution(nd, start, end, time.interval = 1)
edgeIncidenceAt(nd, at)
edgeDissolutionAt(nd, at)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{\link{networkDynamic}} object
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
  \item{at}{
   numeric value giving the time point at which incidence or dissolution counts should be evaluated.
  }
}
\details{
Uses \code{as.data.frame.networkDynamic} internally.  Does not yet correct for censoring of edges forming at the beginning or end of the network.
}
\value{
For \code{tEdgeIncidence} and \code{tEdgeDissolution}, a numeric vector of class \code{ts} giving the incidence and dissolution counts (respectively) as a time-series.  For \code{edgeIncidenceAt} and \code{edgeDissolutionsAt}, a single numeric value
}

\author{
skyebend@uw.edu
}
\note{
should add additional args to allow binning other than 'at', options to deal with how censored edges are calculated.  As evaluation is currently done with 'at' syntax, these functions are probably only useful for discrete time networks. 
}


\examples{
  library(networkDynamicData)
  data(concurrencyComparisonNets)
  # plot formation and dissolution time-series
  plot(tEdgeIncidence(base),col='green', 
    main='edge formation and dissolution rates per timestep of base')
  points(tEdgeDissolution(base),col='red',type='l')
}

