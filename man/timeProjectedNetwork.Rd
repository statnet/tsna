\name{timeProjectedNetwork}
\alias{timeProjectedNetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a time-projected ("multi-slice") network by binning a networkDynamic object
}
\description{
Builds a new static representation of a dynamic network constructed by binning the dynamic network into static slices and constructing new directed 'identity-arcs' between the vertices' realizations in sucessive time slices. 
}
\usage{
timeProjectedNetwork(nd, start = NULL, end = NULL, 
                        time.increment = NULL, onsets = NULL, termini = NULL, 
                        ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 the \code{\link{networkDynamic}} object for which the time projected network should be constructed
}
  \item{start}{
optional numeric start time to be use as lower bound for binning interval (default is what is observed in network)
}
  \item{end}{
optional numeric end time to be use as upper bound for binning interval (default is what is observed in network)
}
  \item{time.increment}{
value for the offset (and duration) between sucessive samples. Will default to 1 if not otherwise specified
}
  \item{onsets}{
A numeric vector containing the onset times of the networks to be extracted. This must be accompanied by \code{termini} of the same length.
}
  \item{termini}{
A numeric vector containing the terminus times of the networks to be extracted. This must be accompanied by \code{onsets} of the same length.
}
  \item{\dots}{
Additional arguments to \code{\link{network.collapse}} (such as \code{rule},\code{active.default}
}
}
\details{
Uses \code{\link{network.collapse}} to bin the \code{nd} network into a list of static networks, aggregates them into a new network with size equal to original network size X number of slices.  To assist with plotting an edge attribute \code{edge.type} is added to all of the edges, having the value \code{'within_slice'} for edges existing the the original network and \code{'identity_arc'} for edges linking the vertices in time.  Vertex attributes (possibly collapsed TEAs) are copied from the original network to the projected network. 

Vertex activity is currently ignored(\code{retain.all.vertices} is set to true internally to force all time slice networks to have the same size).

This function can be used as a means to construct a diagonal block matrix. 
}

\note{
As with all discrete representations of dynamic processes, the time projected graph is an approximation and may over- or under-represent some transmission potential depending on the bin size. 
}

\value{
a \code{\link{network}} object that encodes a discrete time representation of the temporal evolution of the input \code{\link{networkDynamic}} object. 
}
\references{ 
James Moody (2015) Static Representations of Dynamic Networks (DRAFT)

Earlier citations?

}
\author{
Skye Bender-deMoll (skyebend@uw.edu), James Moody
}




\seealso{
\code{\link{network.collapse}}
}
\examples{
data(moodyContactSim)


# use slices at each changing time point
library(networkDynamicData)
data(vanDeBunt_students)
times<-get.change.times(vanDeBunt_students)
vanDProj<-timeProjectedNetwork(vanDeBunt_students,onsets = times,termini = times)
# plot it with gray for the time edges
plot(vanDProj,
    arrowhead.cex = 0,
     edge.col=ifelse(vanDProj\%e\%'edge.type'=='within_slice','black','gray'),
     vertex.cex=0.7,mode='kamadakawai')


# compute shortest temporal path distances from each vertex in first slice
# to each vertex in last slice
library(sna)
geodist(vanDProj)$gdist[1:32,193:224]

# bin the moody sim into 100 timestep chunks
# (this will over-represent some transmission potential)
moodyProj<-timeProjectedNetwork(moodyContactSim,time.increment=100)
plot(moodyProj,arrowhead.cex = 0,
    edge.col=ifelse(moodyProj\%e\%'edge.type'=='within_slice','black','gray'),
    vertex.cex=0.7,displaylabels = TRUE,label.cex=0.6)
    
}  
