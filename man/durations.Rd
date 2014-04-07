\name{durations}
\alias{durations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute observed activity durations or event counts for edges
}
\description{
Computes observed activity durations or event counts for edges, aggregating at the edges, spell, or dyads level.  
}
\usage{
durations(nd, mode = c("duration", "counts"), subject = c("spells", "edges", "dyads"),
          e = seq_along(nd$mel), start = NULL, end = NULL, active.default = TRUE)
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
option describing the unit of analysis that the durations or counts should be aggregated at. \code{'spells'} considers each event independently, \code{'edges'} sums all of the spells corresponding to a single edge id.  \code{'dyads'} sums all of the spells coresponding to a single (directed) tail-head pair (this means spells from multiplex edges conecting the same vertices will be added together).
}
  \item{e}{
 numeric vector of edge ids to consider. 
}
  \item{start}{
optional numeric value to be used to censor onset times. 
}
  \item{end}{
optional numeric value to be used to censor terminus times.
}
  \item{active.default}{
logial, should edges with no timing information be considered always active by default?
}
}
\details{
 The function either sums the durations of edge events, or, in order to provide a useful metric for networks having zero-duration events, simply counts them.  It is implemented by wrapping a call to \code{\link{as.data.frame.networkDynamic}}. In many cases the output of the function will be fed to another statistical summary function like \code{summary} or \code{hist}. The level of aggregation can be selected by setting the subject to either \code{spells}, \code{edges}, or \code{dyads}.
 
Note that the 'observed' durations may not match the 'true' (statistically estimated) durations for a network due to the censoring of edges.
}
\value{
A vector of values corresponding to the selected subjects with the count or duration of events.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
skyebend
}
\note{
This is an early implementation of the function and its name and arguments are subject to change
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{as.data.frame.networkDynamic}}
}
\examples{
require(networkDynamicData)
data(hospital_contact)
summary(durations(hospital,subject='edges'))
summary(durations(hospital,mode='counts',subject='edges'))
}

