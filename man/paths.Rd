\name{paths}
\alias{paths}
\alias{paths.fwd.earliest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate temporally reachable paths in a dynamic network
}
\description{
Functions to search out the set of vertices reachable from an initial vertex following the earliest paths constrained by edge timing.
}
\usage{
paths.fwd.earliest(nd, v, start, end, active.default = TRUE, graph.step.time = 0)
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
Uses a Dijkstra's style Depth First Search to find the set of vertices reachable on a path from the initial seed vertex while respecting the constraints of edge timing.  The path found is the \emph{earliest} (but not necessarily the \emph{quickest} or \emph{most sustained} path). When there are multiple equivalent paths only a single one will be arbitrarily returned.   Vertex activity is currently ignored, and it is assumed that once a path reaches a vertex, all future edges from the vertex are accessible.  The path search can be constrained in time using the \code{start} and \code{end} parameters.    
}
\value{
Currently (THIS MAY CHANGE) a list with two elements providing information on the path found. 
 \item{distance}{A numeric vector with length equal to network size in which each element contains the earliest (usually temporal) distance at which the corresponding vertex could be reached from the seed vertex}
 \item{previous}{A numeric vector with length equal to network size in which each element indicates the previous vertex along (a possible) earliest reachable path. Can be used to reconstruct the path tree.  }

}
\references{
Unpublished discussions with James Moody and Martina Morris
}
\author{
Skye Bender-deMoll
}
\note{
Distances are in terms of time from the \code{start} parameter, so to recover the model times at which each vertex was reached: \code{$distance+start}. This is an early draft of the function, its name and arguments are subject to change before release. 
}




\examples{
require(networkDynamicData)
data(hospital_contact)
hosPath<-paths.fwd.earliest(hospital,v=1)
}

