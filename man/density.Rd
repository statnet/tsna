\name{edge_event_density}
\alias{density}
\alias{edge_event_density}
\alias{dyad_duration_density}
\alias{edge_duration_density}

\title{
Functions to compute temporal density-related measures on dynamic networks
}
\description{
These functions provide various generalizations of the concept of "network density" from traditional SNA to \code{networkDynamic} objects
}
\usage{
edge_event_density(nd)
dyad_duration_density(nd,active.default=TRUE)
edge_duration_density(nd,active.default=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a \code{\link{networkDynamic}} object to evaluate density on
  }
  \item{active.default}{
  logical, default TRUE.  should edges without explicit timing information be considered active by default?
  }
}
\details{
The \code{edge_event_density} function computes the number of events (spells) occuring on each edge the network and divides it by the total amount of observable time per edge within the time bounds of the network.  Can be interpreted as the fraction of existing ties toggling in a unit timestep?

The \code{dyad_duration_density} measure computes the total duration of activity of all the edges in the network, and divides by the total amount of observable time for all the \emph{possible dyads} (existing and non-existing edges). Value of 1 corresponds to a fully-connected network in which all edges are always active, value of 0 would be a network with no active edges. Can be interpreted as the average fraction of possible edges active at any time. 

The \code{edge_duration_density} measure computes the total duration of activity of all the edges in the network and divides by the total amount of observable time for all the \emph{existing edges}. Can be interpreted as the average fraction of observed edges active at any time. A value of 1 corresponds to a network in which all of the existing edges are always active (but the network still may be topologically sparse)

For networks with instantansous spells, the event measures would be used in preference to the duration measures, as all of the events will have zero durations. 

Note that all of these measures depend on having an accurate value for the temporal bounds of the network.  If a \code{net.obs.period} exists, it will determine the range of observations.  If it does not exist, the range will be the (non-Inf) range of earliest and latest events found on the network by \code{get.change.times}. If no non-Inf range exists (presumeably because all ties are always active or always inactive), the range (0-1) will be used. 

For sensible results on discrete networks, the measures are effectively making the assumption that the time increment is 1. TODO: read time increment from net.obs.period if it exits?

Networks with no edges or vertices will return 0, although they are technically undefined. 

Behavior with multiplex ties? dyad measures could range above 1, edge measures will renormalize.
}
\value{
A numeric value representing the network-level measure of the density metric applied
}
\references{
none yet
}
\author{
skyebend
}
\note{
These are experimental functions, names and arguments still subject to change.  Should these be collapsed to a single measure with multiple arguments?
}


\examples{
require(networkDynamicData)
data(hospital_contact)
dyad_duration_density(hospital)
}

