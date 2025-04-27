\name{tiedDuration}
\alias{tiedDuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the duration of time (or count of events) that each vertex is tied/connected to others by an edge
}
\description{
Computes the total duration that each vertex in the network is tied to other vertices by incident edges.  Alternately, if \code{mode="counts"}, computes the total number of incident edge spells each vertex is tied by.  The later is especially useful for continuous time networks tied by edges with 0-duration events. For directed networks, the durations can be filtered using the \code{neighborhood} argument to include only incoming, outgoing, or all ties combined in order to return out-tiedor in-tied durations.
}
\usage{
tiedDuration(nd, mode = c("duration", "counts"), 
                     active.default = TRUE, 
                     neighborhood = c("out", "in", "combined"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{\link[networkDynamic]{networkDynamic}} object describing the network for which durations should be calculated
}
  \item{mode}{
   either \code{"duration"} or \code{"count"} indicating if the sum of edge durations or the count of the number of edge events should be returned
}
  \item{active.default}{
logical, should edges with no defined activity spells be considered active by default?
}
  \item{neighborhood}{
  value of \code{"out"}, \code{"in"} or \code{"combined"} indicating if -- for directed networks -- the durations (counts) should be limited to the vertices' ties outgoing, incoming, or combined (both).
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
  tiedDuration(moodyContactSim)
  
  data(McFarland_cls33_10_16_96)
  # compute ratio of incoming vs. outgoing speach acts
  outDur <- tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'out') 
  inDur <- tiedDuration(cls33_10_16_96, mode='counts',neighborhood = 'in')
  outDur / inDur
}

