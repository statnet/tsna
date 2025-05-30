%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{duration}
\alias{duration}
\alias{edgeDuration}
\alias{vertexDuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Observed Activity Durations or Event Counts for Edges or Vertices
}
\description{
Computes observed activity durations or event counts for edges or vertices, aggregating at the edges, spell, or dyads level.  
}
\usage{
edgeDuration(nd, mode = c("duration", "counts"), subject = c("edges", "spells", "dyads"),
          e = seq_along(nd$mel), start = NULL, end = NULL, active.default = TRUE)
          
vertexDuration(nd, mode = c("duration", "counts"), subject = c("vertices", "spells"),
     v = seq_len(network.size(nd)), active.default = TRUE)           
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
networkDynamic object to be evaluated
}
  \item{mode}{
option determining if the \code{'duration'} of the spells should be summed, or simply the \code{'counts'} of the number of spells.  The later option is useful when the network contains events of zero duration. 
}
  \item{subject}{
option describing the unit of analysis that the durations or counts should be aggregated at. \code{'spells'} considers each event independently. For \code{tVertexDuration}, \code{'vertices'} sums all the spells corresponding to a single vertex id.  For \code{edgeDuration}, \code{'edges'} sums all of the spells corresponding to a single edge id and \code{'dyads'} sums all of the spells coresponding to a single (directed) tail-head pair (this means spells from multiplex edges conecting the same vertices will be added together).
}
  \item{e}{
 numeric vector of edge ids to consider. 
}

 \item{v}{
 numeric vector of vertex ids to consider. 
}
  \item{start}{
optional numeric value to be used to censor onset times. (not yet supported for vertices, must use \code{\link[networkDynamic]{net.obs.period}} instead.)
}
  \item{end}{
optional numeric value to be used to censor terminus times. (not yet supported for vertices, must use \code{\link[networkDynamic]{net.obs.period}} instead.)
}
  \item{active.default}{
logial, should edges or vertices with no timing information be considered always active by default?
}
}
\details{
 The function sums the durations of edge or vertex events or,in order to provide a useful metric for networks having zero-duration events, simply counts them.  It is implemented by wrapping a call to \code{\link[networkDynamic]{as.data.frame.networkDynamic}} or \code{\link[networkDynamic]{get.vertex.activity}}. In many cases the output of the function will be fed to another statistical summary function like \code{summary} or \code{hist}. The level of aggregation can be selected by setting the subject to either \code{spells}, \code{edges}, or \code{dyads}.
 
Note that the 'observed' durations may not match the 'true' (statistically estimated) durations for a network due to the censoring of edges/vertices.
}
\value{
A vector of values corresponding to the selected subjects with the count or duration of events. If the network contains no edges/vertices, \code{numeric(0)} will be returned. 
}

\author{
skyebend
}
\note{
This is an early implementation of the function and its name and arguments are subject to change
}


\seealso{
See also \code{\link[networkDynamic]{as.data.frame.networkDynamic}}
}
\examples{

# look at the distributions of edge durations for
# a real-world contact network
\dontrun{
require(networkDynamicData)
data(hospital_contact)
summary(edgeDuration(hospital,subject='edges'))
summary(edgeDuration(hospital,mode='counts',subject='edges'))
}
# look at the vertex durations for a network were 
# vertices are not present every day
require(networkDynamic)
data(windsurfers)
vertexDuration(windsurfers)
}

