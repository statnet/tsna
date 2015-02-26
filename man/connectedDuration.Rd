\name{connectedDuration}
\alias{connectedDuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the duration of time (or count of events) that each vertex is connected by an edge
}
\description{
Computes the total duration that each vertex in the network is connected by an incident edge.  Alternately, if \code{mode="counts"}, computes the total number of spells each vertex is connected by an incident edge.  The later is especially useful for continuous time networks connected by edges with 0-duration events. For directed networks, the durations can be filtered using the \code{neighborhood} argument to include only incoming, outgoing, or all ties combined in order to return out-connected or in-connected durations. For directed networks, this makes it possible to compare in vs. out connection rates
}
\usage{
connectedDuration(nd, mode = c("duration", "counts"), 
                     active.default = TRUE, 
                     neighborhood = c("out", "in", "combined"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{\link{networkDynamic}} object describing the network for which durations should be calculated
}
  \item{mode}{
   either \code{"duration"} or \code{"count"} indicating if the sum of edge durations or the count of the number of edge events should be returned
}
  \item{active.default}{
logical, should edges with no defined activity spells be considered active by default?
}
  \item{neighborhood}{
  value of \code{"out"}, \code{"in"} or \code{"combined"} indicating if -- for directed networks -- the durations (counts) should be limited to there vertex's ties edges outgoing, incoming, or combined (both).
}
}
\details{
Implemented internally using the \code{\link[networkDynamic]{as.data.frame.networkDynamic}} function and so follows the same truncation conventions for handling censored edges (edges that are active before or after the observation window of the network)
}
\value{
a numeric vector of length equal to the number of vertices in the network with a value equal to the sum of durations (or counts) of active edges incident upon the vertex. 
}

\author{
skyebend@uw.edu
}
\note{
Should the default neighborhood for directed network be 'combined'?
}

\examples{
  data(moodyContactSim)
  connectedDuration(moodyContactSim)
  
  data(McFarland_cls33_10_16_96)
  # compute ration of incoming vs. outgoing speach acts
  connectedDuration(cls33_10_16_96, mode='counts',neighborhood = 'out') / connectedDuration(cls33_10_16_96, mode='counts',neighborhood = 'in')
}

