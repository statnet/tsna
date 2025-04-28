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
Functions to search out the sequence and distances of vertices in a \code{networkDynamic} object reachable from an initial vertex by following paths constrained by edge timing.
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
 networkDynamic object to be searched for temporal paths
}
  \item{v}{
 integer id of the vertex to be used as the starting point of the search
}
 \item{direction}{
  option indicating the temporal direction in which the network should be searched: \code{'fwd'} means search forwards in time and forward along edge directions, \code{'bkwd'} means search backwards in time and backwards along edge directions.
 }
 \item{type}{
 option indicating the type of path (temporal constraint of the path) be searched for:
  \itemize{
     \item \code{'earliest.arrive'} will find the paths that arrive first at the target vertices, 
     \item \code{'latest.depart'} will find the paths that leave the source vertex at the latest possible time.  
  }
  Additional options will be added as implemented. 
 }
  \item{start}{
 time at which to begin searching. Edges that terminate before this time will not be considered. If not specified, defaults to earliest time observed on the network according to \code{get.change.times}.
}
  \item{end}{
 time to end the path search.  Edges that onset on or after this time will not be considered in the path search.
}
  \item{active.default}{
  Boolean, default TRUE. Should edges with no timing information be considered active by default?
}
  \item{graph.step.time}{
numeric.  How much time should be added for each edge traversal (graph hop)? Default is 0, meaning that path distances returned will be purely temporal and will not incorporate graph path distances and 'transmission' can cross multiple edges in a single instant.  A value of 1 would correspond to counting path distances like a traditional centrality score or discrete time simulation.  
}
 \item{x}{an object to be tested for inheriting the class \code{'tPath'}}
}
\details{

A \emph{temporal path} in a dynamic network is a sequence of vertices and edges such that the onset times of successive elements are greater than or equal than those of the previous. In other words, the path is a directed traversal of the network that respects the constraints of edge activity spells and permits 'waiting' at intermediate vertices for 'future' edges to form.

When set to use \code{direction='fwd'} , \code{type='earliest.arrive'} \code{tPath} performs a time-minimizing Dijkstra's style Depth First Search to find the set of vertices reachable on a \emph{forward temporal path} from the initial seed vertex \code{v} while respecting the constraints of edge timing.The path found is a \emph{earliest arriving} (in contrast to the \emph{earliest leaving} or \emph{quickest} or \emph{latest arriving} path). When there are multiple equivalent paths only a single one will be arbitrarily returned.   NOTE THAT THE PATH-FINDING ALGORITHM WILL NOT GIVE CORRECT RESULTS IF ANY SPELLS CONTAIN VALUES LESS THAN 0.

When set to \code{direction='bkwd'} and \code{type='latest.depart'} the path will be found by searching backwards in time from the \code{end} point. In other words, it returns the set of vertices that can reach \code{v}, along with latest possible departure times from those vertices.  Note that in this case the elapsed time values returned for \code{tdist} will be negative, indicating time measured backwards from the \code{end} bound. 

When set to \code{type='fewest.steps'} the path returned will be a 'shortest' (fewest steps/graph hops) time-respecting path.  This would not be necessiairly the quickest or earliest route, but would pass across the fewest possible number of edges (requires the fewest number of transmission steps).   

The \code{graph.step.time} parmeter allows specifying an explicit duration for edge traversals.  In this case the algorithm considers both the onset and terminus times of activity spells to ensure that suffecient time remains for an edge traversal to be made.  If \code{graph.step.time} > the remaining duration of an edge's activity spell, the edge is considered non-traverseable. The primary use case for this parameter is to align the paths discovered with those that might be found by a discrete time transmission simulation in which a path can only spread a single graph hop per model timestep. 

Vertex activity is currently ignored, and it is assumed that once a path reaches a vertex, all future edges from the vertex are accessible.  The path search can be constrained in time using the \code{start} and \code{end} parameters to bound the time span to be explored by the path search.





\code{'bwkd'} \code{'latest.depart'} is essentially the inverse of fwd earliest arrive.  It finds the latest time paths backwards from the initial seed vertex. This is the \emph{latest-leaving} time. Note that the distance returned are positive, but represent the latest distance back in time from the \code{end} parameter time at which a vertex can reach \code{v}. 


The \code{is.tPath} function checks if an object has the class \code{tPath}.

}
\value{
Currently an object of class \code{tPath} which is essentially list with several elements providing information on the path found. 


 \item{tdist}{A numeric vector with length equal to network size in which each element contains the earliest/latest \emph{temporal} distance at which the corresponding vertex could reach / be reached from the seed vertex. Values are elapsed time, as measured from the \code{start} parameter. Unreachable vertices are marked with \code{Inf}}
 \item{previous}{A numeric vector with length equal to network size in which each element indicates the previous vertex along (a possible)  reachable path. Can be used to reconstruct the path tree. The initial vertex and unreachable vertices are marked with \code{0}}
 \item{gsteps}{A numeric vector (of length equal to network size) in which each element indicates the number of steps in the path (number of graph hops) to the vertex along the temporal path found starting at the seed vertex.}
 \item{start}{the numeric start value that was used as the earliest bound for the path calculation (may not have been explicitly set)}
 \item{end}{the numerid end value that was used as the latest bound for the path calculation (may not have been explicitly set)}
 \item{direction}{The direction \code{'fwd'} or \code{'bkwd'} of the path}
\item{type}{The type of temporal constraint for the path}

}
\references{
Unpublished discussions with James Moody and Martina Morris and the statnet team.  

Useful background information (for a slightly different algorithm) can be found in:
B. Bui Xuan, Afonso Ferreira, Aubin Jarry. "Computing shortest, fastest, and foremost journeys
in dynamic networks." RR-4589, 2002. https://inria.hal.science/inria-00071996/document

B. Bui Xuan, Afonso Ferreira, Aubin Jarry. Evolving graphs and least cost journeys in dynamic
networks. WiOpt'03: Modeling and Optimization in Mobile, Ad Hoc and Wireless Networks,
Mar 2003, Sophia Antipolis, France. 10 p., 2003
https://inria.hal.science/inria-00466676/document
}
\author{
Skye Bender-deMoll
}
\note{
Temporal distances are in terms of time measured from the \code{start} parameter, so to recover the model times at which each vertex was reached for forward paths use \code{$tdist+start} and backward paths with \code{end- $tdist}. This is an early draft of the function, its name and arguments are subject to change before release. 
}




\examples{
require(networkDynamicData)
data(hospital_contact)
hosPath<-tPath(hospital,v=1)
}

