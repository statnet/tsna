%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{paths}
\alias{paths}
\alias{tPath}
\alias{temporalPath}
\alias{is.tPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Temporally Reachable Paths in a networkDynamic Object
}
\description{
Functions to search out the set of vertices reachable from an initial vertex following the earliest paths constrained by edge timing in a networkDynamic object.
}
\usage{
tPath(nd, v, direction=c('fwd','bkwd'), 
                 type=c('earliest.arrive', 'latest.depart'),
                 start, end, active.default = TRUE, graph.step.time = 0)

is.tPath(x)
}



%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 networkDynamic object to be searched for paths
}
  \item{v}{
 integer id of the starting vertex
}
 \item{direction}{
  option indicating the direction in which the network should be searched: \code{'fwd'} means search forwards in time and forward along edge directions, \code{'bkwd'} means search backwards in time and backwards along edge directions.
 }
 \item{type}{
 option indicating the type of path (temporal constraint of the path) be searched for: \code{'earliest.arrive'} will find the paths that arrive first at the target vertices, \code{'latest.depart'} will find the paths that leave the source vertex at the latest possible time.  Additional options will be added as implemented. 
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
numeric.  How much time should be added for each edge traversal (graph hop)? Default is 0, meaning that path distances returned will be purely temporal and will not incorporate geodesic graph path distances.  A value of 1 would correspond to counting path distances like a traditional centrality score or discrete time simulation.   NAME AND FUNCTION OF THIS ARG LIKELY TO CHANGE.  Note that if \code{graph.step.time} > the remaining duration of an edge's activity spell, the edge is considered non-traverseable. 
}
 \item{x}{an object to be tested for inheriting the class \code{'tPath'}}
}
\details{
When set to use \code{'fwd'} , \code{'earliest.arrive'} \code{tPath} a Dijkstra's style Depth First Search to find the set of vertices reachable on a path from the initial seed vertex while respecting the constraints of edge timing.  The path found is the \emph{earliest arriving} (but not necessarily the \emph{earliest leaving} or \emph{quickest} or \emph{most sustained} path). When there are multiple equivalent paths only a single one will be arbitrarily returned.   Vertex activity is currently ignored, and it is assumed that once a path reaches a vertex, all future edges from the vertex are accessible.  The path search can be constrained in time using the \code{start} and \code{end} parameters. Distance values represent the earliest elapsed time (from the \code{start} parameter) at which vertices can be reached from \code{v}.

THESE ARE NOT CORRECTLY IMPLEMENTED YET:

\code{'bwkd'} \code{'latest.depart'} is essentially the inverse of fwd earliest arrive.  It finds the latest time paths backwards from the initial seed vertex. This is the \emph{latest-leaving} time. Note that the distance returned are positive, but represent the latest distance back in time from the \code{end} parameter time at which a vertex can reach \code{v}. 

Because we don't have an efficient algorithm for maximizing path length, \code{paths.fwd.latest} first finds the set of reachable vertices using the earliest path, and the finds the latest time those vertices can be reached by calling \code{paths.bwkd.latest} on each. Distances represent the latest time a path could leave \code{v} and still reach the corresponding vertex

The \code{is.tPath} function checks if an object has the class \code{tPath}.

}
\value{
Currently an object of class \code{tPath} which is essentially list with several elements providing information on the path found. 


 \item{distance}{A numeric vector with length equal to network size in which each element contains the earliest/latest (usually temporal) \emph{distance} at which the corresponding vertex could reach / be reached from the seed vertex. Unreachable vertices are marked with \code{Inf}}
 \item{previous}{A numeric vector with length equal to network size in which each element indicates the previous vertex along (a possible)  reachable path. Can be used to reconstruct the path tree. The initial vertex and unreachable vertices are marked with \code{0}}
 \item{geodist}{A numeric vector with length equal to network size in which each element indicates the geodesic distances (number of hops) of the vertex along the temporal path starting at the seed vertex.}
 \item{direction}{The direction \code{'fwd'} or \code{'bkwd'} of the path}
\item{type}{The type of temporal constraint for the path}

}
\references{
Unpublished discussions with James Moody and Martina Morris and the statnet team
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
hosPath<-tPath(hospital,v=1)
}

