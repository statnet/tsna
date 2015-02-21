%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{tReach}
\alias{tReach}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
computes sizes of temporally reachable sets in a dynamicNetwork
}
\description{
computes sizes of temporally reachable sets in a dynamicNetwork, using either a full census or sample of starting vertices of the specified size
}
\usage{
tReach(nd, direction = c("fwd","bkwd"), sample=network.size(nd),
      start, end,  graph.step.time=0)
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
  numeric, indicates the size of the sample of vertices to use to calculate the reachable sets
}
\item{start}{optional numeric start time to begin path search}
\item{end}{optional numeric time to end path search }
\item{graph.step.time}{How much time should be added for each edge traversal (graph hop)? See \code{\link{tPath}} for details.} 
}
\details{
\code{tReach} calls \code{link{tPath}} on with each starting vertex to determine the sizes of the sets of vertices that are reachable. If \code{sample} is set to something less than the size of the network, it will sample that many vertices instead of doing the (expensive) full census.  Note that when the vertices are chosen as a sample, results may vary between calls to this function
}

 
\value{
a vector of length equal to \code{sample} giving the sizes of the set of vertices reachable from each seed vertex within the specified time bounds
}

\author{
skyebend
}
\note{
Needs implementation of backwards sets
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also as \code{\link{tPath}}
}
\examples{
data(moodyContactSim)
tReach(moodyContactSim)

# only sample 3 paths
tReach(moodyContactSim,sample=3)

# what fraction of the network could each vertex reach?
tReach(moodyContactSim)/network.size(moodyContactSim)

# what fraction of the network could each vertex be reached by?
tReach(moodyContactSim,direction='bkwd')/network.size(moodyContactSim)
}