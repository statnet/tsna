%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{tReachableSetSizes}
\alias{tReachableSetSizes}
\alias{tReach}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
computes sizes of temporally reachable sets in a dynamicNetwork
}
\description{
computes sizes of temporally reachable sets in a dynamicNetwork, using either a full census or sample of starting vertices of the specified size
}
\usage{
tReachableSetSizes(nd, direction = c("fwd","bkwd"), sample = FALSE)
tReach(nd, direction = c("fwd","bkwd"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a networkDynamic object
}
  \item{direction}{
 currently only \code{'fwd'} works, to calculate the sizes forward-reachable sets (as opposed to backwards-reachable)
}
  \item{sample}{
 default is \code{FALSE}, if numeric, indicates the size of the sample of vertices to use to calculate the reachable sets
}
}
\details{
Calls \code{link{tPath}} on with each starting vertex to determine the sizes of the sets of vertices that are reachable. If sample is set to a numeric value, it will only sample that many vertices instead of doing the (expensive) full census.

\code{tReach} calculates the fraction of the vertices in the network than each vertex can reach (or be reached by if \code{direction='bkwd'}) assuming a start at the beginning of the network. 
}
\value{
For \code{tReachableSetSizes},a vector giving the sizes of the set of vertices reachable from each seed vertex. For \code{tReach}, the same vector divided by the number of vertices in the network. 
}

\author{
skyebend
}
\note{
Needs additional options for time range specification, implementation of backwards sets
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also as \code{\link{tPath}}
}
\examples{
data(moodyContactSim)
tReachableSetSizes(moodyContactSim)

# only sample 3 paths
tReachableSetSizes(moodyContactSim,sample=3)

# what fraction of the network could each vertex reach?
tReach(moodyContactSim)

# what fraction of the network could each vertex be reached by?
tReach(moodyContactSim,direction='bkwd')
}