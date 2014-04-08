\name{paths}
\alias{paths}
\alias{paths.fwd.earliest}
\alias{paths.fwd.latest}
\alias{paths.bkwd.latest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate temporally reachable paths in a dynamic network
}
\description{
Functions to search out the set of vertices reachable from an initial vertex following the earliest paths constrained by edge timing.
}
\usage{
paths.fwd.earliest(nd, v, start, end, active.default = TRUE, graph.step.time = 0)
paths.bkwd.latest(nd, v, start, end, active.default = TRUE, graph.step.time = 0)
paths.fwd.latest(nd, v, start, end, active.default = TRUE, graph.step.time = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 networkDynamic object to be searched for paths
}
  \item{v}{
 integer id of the starting vertex
}
  \item{start}{
 time at which to begin searching. Edges that terminate before this time will not be considered. If not specified, defaults to earliest time observed on the network according to \code{get.change.times}.
}
  \item{end}{
 time to end the path search.  Edges that onset on or after this time will not be considered.
}
  \item{active.default}{
  Boolean, default TRUE. Should edges with no timing information be considered active by default?
}
  \item{graph.step.time}{
numeric.  How much time should be added for each edge traversal (graph hop)? Default is 0, meaning that path distances returned will be purely temporal and will not incorporate geodesic graph path distances.  A value of 1 would correspond to counting path distances like a traditional centrality score or discrete time simulation.   NAME AND FUNCTION OF THIS ARG LIKELY TO CHANGE 
}
}
\details{
\code{paths.fwd.earliest} uses a Dijkstra's style Depth First Search to find the set of vertices reachable on a path from the initial seed vertex while respecting the constraints of edge timing.  The path found is the \emph{earliest arriving} (but not necessarily the \emph{earliset leaving} or \emph{quickest} or \emph{most sustained} path). When there are multiple equivalent paths only a single one will be arbitrarily returned.   Vertex activity is currently ignored, and it is assumed that once a path reaches a vertex, all future edges from the vertex are accessible.  The path search can be constrained in time using the \code{start} and \code{end} parameters. Distance values represent the earliest elapsed time (from the \code{start} parameter) at which vertices can be reached from \code{v}.

THESE ARE NOT CORRECTLY IMPLEMENTED YET:

\code{paths.bwkd.latest} is essentially the inverse of \code{paths.fwd.earliest}.  It finds the latest time paths backwards from the initial seed vertex. This is the \emph{latest-leaving} time. Note that the distance returned are positive, but represent the latest distance back in time from the \code{end} paramter time at which a vertex can reach \code{v}. 

Because we don't have an efficient algorithm for maximizing path length, \code{paths.fwd.latest} first finds the set of reachable vertices using the earliest path, and the finds the latest time those vertices can be reached by calling \code{paths.bwkd.latest} on each. Distances represent the latest time a path could leave \code{v} and still reach the corresponding vertex

}
\value{
Currently (THIS MAY CHANGE) a list with two elements providing information on the path found. 
 \item{distance}{A numeric vector with length equal to network size in which each element contains the earliest/latest (usually temporal) \emph{distance} at which the corresponding vertex could reach / be reached from the seed vertex}
 \item{previous}{A numeric vector with length equal to network size in which each element indicates the previous vertex along (a possible)  reachable path. Can be used to reconstruct the path tree.  }

}
\references{
Unpublished discussions with James Moody and Martina Morris
}
\author{
Skye Bender-deMoll
}
\note{
Distances are in terms of time from the \code{start} parameter, so to recover the model times at which each vertex was reached for forward paths use \code{$distance+start} and backward paths with \code{end- $distance}. This is an early draft of the function, its name and arguments are subject to change before release. 
}




\examples{
require(networkDynamicData)
data(hospital_contact)
hosPath<-paths.fwd.earliest(hospital,v=1)
}

