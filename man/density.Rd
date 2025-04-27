%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{temporal density}
\alias{tEdgeDensity}

\title{
Functions to compute temporal density-related measures on dynamic networks
}
\description{
These functions provide various network-level statistics giving information on the fraction of time edges are active in \code{networkDynamic} objects
}
\usage{
tEdgeDensity(nd,mode=c('duration','event'),
              agg.unit=c('edge','dyad'),active.default=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{\link[networkDynamic]{networkDynamic}} object to evaluate density on
  }
  \item{mode}{
  option indicating if \code{'duration'} of edge spells should be considered or only the \code{'event'} count (for networks in which events have zero-durations)
  }
  \item{agg.unit}{
  option indicating how to calculate the possible observable time to be used as the denominator: \code{'edge'} only counts existing edges \code{'dyad'} counts all possible dyads. 
  }
  \item{active.default}{
  logical, default TRUE.  should edges without explicit timing information be considered active by default?
  }
}
\details{

The \code{tEdgeDensity} measure by default (\code{mode='duration', agg.unit='edge'}) computes the total duration of activity of all the edges in the network and divides by the total amount of observable time for all the dyads \emph{between which edges are ever observed}. Can be interpreted as the average fraction of observed edges active at any time. A value of 1 corresponds to a network in which all of the observed edges are always active (but the network still may be topologically sparse, having a low density)

The \code{tEdgeDensity} function with \code{mode='event'} computes the number of events (spells) occurring on each edge the network and divides it by the total amount of observable time per dyad ever observed to have an edge within the time bounds of the network.  Can be interpreted as the fraction of existing ties toggling in a unit time step?

The \code{agg.unit='dyad'} measure computes the total duration (or count of events) of activity of all the edges in the network, and divides by the total amount of observable time for all the \emph{possible dyads} (existing and non-existing edges). Value of 1 corresponds to a fully-connected network in which all edges are always active, value of 0 would be a network with no active edges. Can be interpreted as the average fraction of possible edges active at any time. 



For networks with instantaneous spells, the event measures would be used in preference to the duration measures, as all of the events will have zero durations. 

Note that all of these measures depend on having an accurate value for the temporal bounds of the network.  If a \code{net.obs.period} exists, it will determine the range of observations.  If it does not exist, the range will be the (non-Inf) range of earliest and latest events found on the network by \code{get.change.times}. If no non-Inf range exists (presumably because all ties are always active or always inactive), the range (0-1) will be used. 

For sensible results on discrete networks, the measures are effectively making the assumption that the time increment is 1. TODO: read time increment from net.obs.period if it exits?

Networks with no edges or vertices will return 0, although they are technically undefined. 

Behavior with multiplex ties? dyad measures could range above 1, edge measures will re-normalize.
}
\value{
A numeric value representing the network-level measure of the density metric applied
}
\references{
none yet
}
\author{
skyebend
}
\note{
These are experimental functions, names and arguments still subject to change.  Should these be collapsed to a single measure with multiple arguments?
}


\examples{
\dontrun{
require(networkDynamicData)
data(hospital_contact)
tEdgeDensity(hospital)
}
}

